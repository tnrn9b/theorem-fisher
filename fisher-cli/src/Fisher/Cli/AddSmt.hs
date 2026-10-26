{-# LANGUAGE MultiWayIf #-}
-- | @fisher project add smt@: require lean-smt in a Lean project's lakefile,
-- pinned to the revision Reservoir built on the project's toolchain, then
-- @lake update smt@, @lake build@ and a one-line check that the @smt@
-- tactic runs. The output of the lake runs goes to @.fisher/add-smt.log@.
module Fisher.Cli.AddSmt
  ( AddSmtDeps (..)
  , defaultAddSmtDeps
  , addSmt
  , addSmtWith
  , smtSampleText
  , addSmtLogName
  , smtFallbackWarning
  , requiresMathlib
  ) where

import Control.Exception (AsyncException (..), catchJust)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.FilePath ((</>))
import System.IO (hFlush, stdin, stdout)

import Fisher.Cli.LakeRun (appendRun, askOn, interruptible, lakeErrorLines, probeProblems, removeStaleProbes, runIn)
import Fisher.Paths (fisherDirName)
import Fisher.ProjectCommand (AddSmtOpts (..))
import Fisher.Prover (ProverKind (..), binaryFix)
import Fisher.Smt.Lake (LakeFile (..), addSmtRequire, findLakeFile, hasMathlib, hasSmt, hasSmtRequire, leanToolchain)
import Fisher.Smt.Reservoir (fetchBuilds, pickRevision, validRevision)

-- | What adding depends on, so tests can stand in for the terminal, the
-- network and lake.
data AddSmtDeps = AddSmtDeps
  { adSay :: Text -> IO ()
  -- ^ one progress line
  , adAsk :: Text -> IO Bool
  -- ^ the question before the lakefile changes; 'False' unless answered yes
  , adFetch :: IO (Either Text [(Text, Text)])
  -- ^ Reservoir's builds of lean-smt, @(revision, toolchain)@, newest first
  , adLake :: FilePath
  , adSecs :: Int
  -- ^ the time limit of each lake run, in seconds
  }

-- | stdout, stdin, 'fetchBuilds', @lake@ from PATH, 30 minutes a run.
defaultAddSmtDeps :: AddSmtDeps
defaultAddSmtDeps = AddSmtDeps
  { adSay = \l -> TIO.putStrLn l >> hFlush stdout
  , adAsk = askOn stdin
  , adFetch = fetchBuilds
  , adLake = "lake"
  , adSecs = 30 * 60 }

-- | 'addSmtWith' the real dependencies.
addSmt :: AddSmtOpts -> FilePath -> IO (Int, [Text])
addSmt = addSmtWith defaultAddSmtDeps

-- | The check's file: Smt imported, one goal for the tactic.
smtSampleText :: Text
smtSampleText = "import Smt\n\nexample : \8704 x : Int, x + 0 = x := by smt\n"

addSmtLogName :: FilePath
addSmtLogName = "add-smt.log"

-- | Add lean-smt to the Lean project at @root@. The progress lines go to
-- 'adSay'; the result is the exit status (0 done or already present, 1
-- failed, 2 for a @--rev@ that is not a revision, 130 interrupted) and the
-- lines to print after.
addSmtWith :: AddSmtDeps -> AddSmtOpts -> FilePath -> IO (Int, [Text])
addSmtWith d o root
  | Just r <- asRev o, not (validRevision r) =
      pure (2, ["(>_<) `--rev` takes main, no_mathlib or a commit of 7 to 40 hex digits, not " <> r])
  | otherwise = findLakeFile root >>= \case
    Nothing -> pure (1, ["(>_<) No lakefile.lean or lakefile.toml in " <> T.pack root])
    Just lf -> do
      let path = lakePath lf
      text <- TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile path
      inManifest <- hasSmt root
      if
        | hasSmtRequire text -> pure (0, ["(^_^) smt is already present in " <> T.pack path])
        | inManifest -> pure (0, ["(^_^) smt is already present in " <> T.pack (root </> "lake-manifest.json")])
        | otherwise -> do
            lakeFound <- if asNoBuild o then pure True else (/= Nothing) <$> findExecutable (adLake d)
            if not lakeFound
              then pure (1, ["(>_<) lake is not on PATH: " <> binaryFix Lean4 "lake"])
              else do
                -- an interrupt before the lakefile changes leaves it as it was
                asked <- stoppable $ do
                  rev <- revision text
                  let what = "Add smt at " <> rev <> " to " <> T.pack path
                        <> (if asNoBuild o then "" else ", then run lake update smt and lake build")
                  go <- if asYes o then pure True else adAsk d (what <> "? [y/N] ")
                  pure (rev, go)
                case asked of
                  Nothing -> pure (130, ["(>_<) Interrupted; nothing was changed."])
                  Just (_, False) -> pure (1, ["(>_<) Nothing was changed; pass --yes to add smt without asking."])
                  Just (rev, True) -> addSmtRequire lf rev >>= \case
                    Left e -> pure (1, ["(>_<) " <> e])
                    Right () -> do
                      adSay d ("Added the smt require at " <> rev <> " to " <> T.pack path)
                      if asNoBuild o
                        then do
                          adSay d ("Not built; run lake update smt and lake build in " <> T.pack root)
                          pure (0, [])
                        else build path
  where
    lakePath = \case
      LakeLean p -> p
      LakeToml p -> p
    revision text = case asRev o of
      Just r -> adSay d ("smt revision: " <> r <> " (--rev)") >> pure r
      Nothing -> do
        mathlib <- (|| requiresMathlib text) <$> hasMathlib root
        tc <- leanToolchain root
        builds <- if mathlib then adFetch d else pure (Right [])
        case pickRevision tc (either (const []) id builds) mathlib of
          Right c -> do
            adSay d ("smt revision: " <> c <> " (Reservoir's build for " <> maybe "" id tc <> ")")
            pure c
          Left branch -> do
            adSay d (smtFallbackWarning mathlib builds tc branch)
            pure branch
    stoppable act = catchJust (\e -> if e == UserInterrupt then Just () else Nothing) (Just <$> interruptible act) (\() -> pure Nothing)
    logPath = root </> fisherDirName </> addSmtLogName
    revert path = "  Revert: delete the require line for smt in " <> T.pack path
    build path = do
      createDirectoryIfMissing True (root </> fisherDirName)
      let failed msg = pure (1, ["(>_<) " <> msg <> "; see " <> T.pack (fisherDirName </> addSmtLogName), revert path])
          lakeRun args k = do
            adSay d ("  lake " <> T.unwords (map T.pack args))
            (code, out) <- runIn (adLake d) (adSecs d) root args
            appendRun logPath "add smt" ("lake " <> T.unwords (map T.pack args)) code out
            if code == 0
              then k
              else pure (1, ("(>_<) lake " <> T.unwords (map T.pack args) <> " failed (exit " <> T.pack (show code) <> "); see " <> T.pack (fisherDirName </> addSmtLogName)) : take 5 (map ("  " <>) (lakeErrorLines out)) <> [revert path])
      r <- stoppable $ lakeRun ["update", "smt"] $ lakeRun ["build"] $ do
        adSay d "  check: by smt"
        removeStaleProbes (root </> fisherDirName)
        problems <- probeProblems (adLake d) (adSecs d) logPath "add smt" root (fisherDirName </> "add-smt-check.lean") smtSampleText []
        if null problems
          then adSay d "Ready: smt" >> pure (0, [])
          else do
            (code, ls) <- failed "the smt check failed"
            pure (code, take 1 ls <> map ("  " <>) (take 5 problems) <> drop 1 ls)
      pure (maybe (130, ["(>_<) Interrupted; the smt require stays in " <> T.pack path, revert path]) id r)

-- | The warning when the revision falls back to a branch: from whether the
-- project has Mathlib, Reservoir's answer and the toolchain.
smtFallbackWarning :: Bool -> Either Text [(Text, Text)] -> Maybe Text -> Text -> Text
smtFallbackWarning mathlib builds tc branch = "Warning: " <> case (mathlib, builds, tc) of
  (False, _, _) -> "this project has no Mathlib; using smt's " <> branch <> " branch"
  (_, Left e, _) -> "could not read Reservoir's builds of smt (" <> e <> "); using " <> branch
  (_, _, Nothing) -> "no lean-toolchain here; using " <> branch
  (_, _, Just t) -> "Reservoir has no build of smt for " <> t <> "; using " <> branch

-- | Does the lakefile require Mathlib: a @name = "mathlib"@ line in a
-- @[[require]]@ block, or a @require@ line naming mathlib (@require mathlib
-- from ...@ or @require "leanprover-community" / "mathlib"@)? Comments and
-- other mentions do not count.
requiresMathlib :: Text -> Bool
requiresMathlib = any req . T.lines
  where
    req l =
      let s = T.strip l
          toks = map (T.dropAround (== '"')) (T.words s)
       in T.filter (/= ' ') s == "name=\"mathlib\""
            || (take 1 toks == ["require"] && "mathlib" `elem` takeWhile (`notElem` ["from", "@"]) (drop 1 toks))
