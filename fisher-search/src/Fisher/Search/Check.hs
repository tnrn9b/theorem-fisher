-- | The kernel's verdict on a merged file, the way the C3 scorer gives it
-- (@tools/bench/c3/score.py@): compile the file with @#print axioms@ and a
-- @pp.all@ @#check@ for each goal appended, then read the messages.
--
-- A goal is closed when the file compiles, no declaration but the other
-- open goals uses @sorry@, its type is the one the original file gave it,
-- and it depends on no axiom beyond the allowed ones. A goal whose only
-- extra axiom is @sorryAx@, coming from goals still open that it uses, is
-- waiting: it closes when they do.
module Fisher.Search.Check
  ( LeanRun (..)
  , lakeLeanRun
  , lakeLeanRunWith
  , Probe (..)
  , probe
  , AxiomPolicy (..)
  , axiomAllowed
  , standardAxioms
  , Verdict (..)
  , verdict
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, onException, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, replaceFileName, takeBaseName, (</>))
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (..), createPipe, createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

import Fisher.Search.Lean

-- | How a file is compiled: the project root and the file (relative to
-- it) in, the exit code and the merged output out.
newtype LeanRun = LeanRun {runLean :: FilePath -> FilePath -> IO (Int, Text)}

-- | @lake env lean FILE@ in the project, with a time limit in seconds
-- (the whole process group is killed at the limit). Output is read as
-- UTF-8 whatever the locale, stdout and stderr together.
lakeLeanRun :: Int -> LeanRun
lakeLeanRun = lakeLeanRunWith "lake"

-- | 'lakeLeanRun' with the @lake@ to run (a path, or a name looked up on
-- PATH).
lakeLeanRunWith :: FilePath -> Int -> LeanRun
lakeLeanRunWith lake secs = LeanRun $ \root rel -> do
  (readEnd, writeEnd) <- createPipe
  (_, _, _, ph) <- createProcess (proc lake ["env", "lean", rel])
    { cwd = Just root, std_in = NoStream, std_out = UseHandle writeEnd, std_err = UseHandle writeEnd, create_group = True }
  outVar <- newEmptyMVar
  _ <- forkIO (BS.hGetContents readEnd >>= putMVar outVar)
  let killGroup = getPid ph >>= mapM_ (\p -> try (signalProcessGroup sigKILL p) :: IO (Either SomeException ()))
  -- any exception (a caller's timeout, a cancel) kills the process group too
  r <- timeout (secs * 1000000) (waitForProcess ph) `onException` killGroup
  code <- case r of
    Just c -> pure (case c of ExitSuccess -> 0; ExitFailure k -> k)
    Nothing -> do
      killGroup
      _ <- waitForProcess ph
      pure (-1)
  out <- takeMVar outVar
  let txt = TE.decodeUtf8With TEE.lenientDecode out
  pure (code, if code == -1 then "timed out after " <> T.pack (show secs) <> " s\n" <> txt else txt)

data Probe = Probe
  { prExit :: Int
  , prErrors :: [Text]
  , prSorryLines :: [Int]
  -- ^ 1-based lines of "declaration uses sorry" warnings
  , prAxioms :: Map.Map Text [Text]
  , prTypes :: Map.Map Text Text
  , prOutput :: Text
  }
  deriving (Eq, Show)

-- | Compile @text@ in place of @rel@ (from a sibling file, so the original
-- stays as it is) with the axioms and the type of each name printed.
probe :: LeanRun -> FilePath -> FilePath -> Text -> [Text] -> IO Probe
probe lr root rel text names = do
  let probeRel = makeRelative root (replaceFileName (root </> rel) ("_fisher_search_" <> takeBaseName rel <> ".lean"))
      tailCmds = T.concat [ "\n\n#print axioms " <> n <> "\n\nset_option pp.all true in\n#check @" <> n | n <- names ]
  BS.writeFile (root </> probeRel) (TE.encodeUtf8 (T.dropWhileEnd (== '\n') text <> tailCmds <> "\n"))
  (code, out) <- runLean lr root probeRel `finally` removeFile (root </> probeRel)
  pure (parseProbe code out names)

parseProbe :: Int -> Text -> [Text] -> Probe
parseProbe code out names =
  Probe code errors sorries (Map.fromList (mapMaybe axiomsOf names)) (Map.fromList (mapMaybe typeOf names)) out
  where
    ls = T.lines out
    errors = take 5 [l | l <- ls, ": error" `T.isInfixOf` l]
    sorries = mapMaybe sorryLine ls
    sorryLine l
      | "declaration uses 'sorry'" `T.isInfixOf` l || "declaration uses `sorry`" `T.isInfixOf` l =
          case T.splitOn ":" (fst (T.breakOn ": warning" l)) of
            parts | length parts >= 3 -> readInt (parts !! (length parts - 2))
            _ -> Nothing
      | otherwise = Nothing
    readInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing
    axiomsOf n =
      let none' = "'" <> n <> "' does not depend on any axioms"
          some' = "'" <> n <> "' depends on axioms: ["
       in if none' `T.isInfixOf` out
            then Just (n, [])
            else case T.breakOn some' out of
              (_, rest) | not (T.null rest) ->
                let inside = fst (T.breakOn "]" (T.drop (T.length some') rest))
                 in Just (n, filter (not . T.null) (map T.strip (T.splitOn "," (T.replace "\n" " " inside))))
              _ -> Nothing
    -- Lean prints `#check @n` as "n : type" (or "@n.{u} : type" when n has
    -- universe parameters), continued on indented lines,
    -- with or without a "file:line:col: info: " prefix
    unprefix l = case T.breakOn ": info: " l of
      (_, r) | not (T.null r) -> T.drop (T.length ": info: ") r
      _ -> l
    typeOf n =
      let starts l = any (\p -> any (`T.isPrefixOf` unprefix l) [p <> " :", p <> ".{"]) [n, "@" <> n]
          hits = [i | (i, l) <- zip [0 :: Int ..] ls, starts l]
       in case reverse hits of
            (i : _) ->
              let first = unprefix (ls !! i)
                  more = takeWhile (\x -> not (T.null x) && T.head x == ' ') (drop (i + 1) ls)
               in Just (n, T.strip (T.intercalate "\n" (first : more)))
            [] -> Nothing

data AxiomPolicy = AxiomPolicy
  { apAllowed :: Set.Set Text
  , apNativeDecide :: Bool
  -- ^ native_decide's per-use axioms (Lean 4.33) are allowed
  }

-- | Whether the policy allows an axiom (@sorryAx@ never is, unless named).
axiomAllowed :: AxiomPolicy -> Text -> Bool
axiomAllowed pol a = Set.member a (apAllowed pol) || (apNativeDecide pol && nativeAxiom a)

nativeAxiom :: Text -> Bool
nativeAxiom a = "._native.native_decide.ax_" `T.isInfixOf` a

standardAxioms :: Set.Set Text
standardAxioms = Set.fromList ["propext", "Classical.choice", "Quot.sound"]

data Verdict = Closed | Waiting | Rejected Text
  deriving (Eq, Show)

-- | The verdict on goal @g@ in a merged file (@text@ is what was probed),
-- given the type its statement had in the original file.
verdict :: AxiomPolicy -> Text -> Text -> Text -> Probe -> Verdict
verdict pol text g expectedType pr
  | (e : _) <- prErrors pr = Rejected ("does not compile: " <> T.take 200 e)
  | prExit pr /= 0 && null (prErrors pr) = Rejected ("lean exited " <> T.pack (show (prExit pr)) <> ": " <> T.takeEnd 200 (prOutput pr))
  | otherwise = case findDecl g ds of
      Nothing -> Rejected "the goal is not in the file"
      Just d
        | isOpen d -> Rejected "still sorry"
        | any (inside d) (prSorryLines pr) -> Rejected "the proof uses sorry"
        | (l : _) <- filter (\l -> not (any (`inside` l) opens)) (prSorryLines pr) ->
            Rejected ("a declaration that is not an open goal uses sorry (line " <> T.pack (show l) <> ")")
        | otherwise -> case (Map.lookup g (prTypes pr), Map.lookup g (prAxioms pr)) of
            (Nothing, _) -> Rejected "no type printed for the goal"
            (_, Nothing) -> Rejected "no axioms printed for the goal"
            (Just ty, Just axs)
              | ty /= expectedType -> Rejected "the statement elaborates to a different type"
              | otherwise ->
                  let extra = [a | a <- axs, not (axiomAllowed pol a)]
                   in case extra of
                        [] -> Closed
                        ["sorryAx"] -> Waiting
                        _ -> Rejected ("axioms beyond the allowed: " <> T.intercalate ", " (filter (/= "sorryAx") extra))
  where
    ds = declarations text
    opens = filter isOpen ds
    inside d l = l >= dStart d + 1 && l <= max (dStart d + 1) (dEnd d)
