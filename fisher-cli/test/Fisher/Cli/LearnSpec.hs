-- | @fisher learn stats|show|diff|reset@ over a temporary run log (the
-- statistics spec's data) and a temporary Fisher home.
module Fisher.Cli.LearnSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.IORef
import Data.List (findIndex, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, helper, info, renderFailure, (<**>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.Learn
import Fisher.Cli.Search (policyShapes, savedDefaults)
import Fisher.Learn.Policy (computePolicy, policyPath, writePolicy)
import Fisher.Learn.Stats (loadStats)
import Fisher.Runlog

projA :: FilePath
projA = "/work/a"

-- | The command over the run log and Fisher home in a temporary
-- directory, from a working directory: its output.
learnIn :: FilePath -> FilePath -> [String] -> IO Text
learnIn home cwd argv = do
  cmd <- case execParserPure defaultPrefs (info learnParser mempty) argv of
    Success c -> pure c
    _ -> fail ("argv did not parse: " <> show argv)
  out <- newIORef ([] :: [Text])
  runLearn (LearnEnv (runlogPath home) home cwd (object []) (pure 1000) (\t -> modifyIORef out (t :))) cmd
  T.unlines . reverse <$> readIORef out

newRun :: Text -> RunKind -> Maybe Text -> Maybe Text -> NewRun
newRun i k par back =
  NewRun
    { nrId = i, nrKind = k, nrParent = par, nrProject = Just projA, nrProver = Just "lean", nrBackend = back
    , nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = 100, nrRawDir = Nothing
    , nrCapture = Full }

-- | The statistics spec's basic data: on shape S, fisher closes 8 of 10,
-- claude-code 2 of 10, at a dollar an attempt.
basic :: Runlog -> IO ()
basic rl = do
  startRun rl (newRun "search-a" SearchRun Nothing Nothing)
  forM_ [("g1", "S"), ("g2", "T")] $ \(g, sh) -> upsertGoal rl "search-a" (GoalRow g g sh "open" Nothing Nothing Nothing)
  forM_ [("fisher", 8), ("claude-code", 2)] $ \(b, c) ->
    forM_ [1 .. 10 :: Int] $ \i -> do
      let aid = "search-a-" <> b <> "-" <> T.pack (show i)
      startRun rl (newRun aid SearchAttempt (Just "search-a") (Just b))
      event rl aid 0 "attempt" (object ["goal" .= ("g1" :: Text), "shape" .= ("S" :: Text), "backend" .= b])
      endRun rl aid (RunEnd (if i <= c then 160 else 700) Nothing (if i <= c then "closed" else "failed") (Just 1) Nothing Nothing Nothing)

withHome :: (FilePath -> IO a) -> IO a
withHome = withSystemTempDirectory "learn"

seeded :: FilePath -> IO ()
seeded home = withRunlog (runlogPath home) basic

spec :: Spec
spec = do
  it "stats --shape S ranks the backends, fisher first, at the shape and project levels" $ withHome $ \home -> do
    seeded home
    out <- learnIn home home ["stats", "--project", projA, "--shape", "S"]
    let ls = T.lines out
    case findIndex (== "backends (shape S)") ls of
      Just i -> do
        let rows = drop (i + 2) ls
        take 1 (T.words (rows !! 0)) `shouldBe` ["fisher"]
        take 1 (T.words (rows !! 1)) `shouldBe` ["claude-code"]
        T.words (rows !! 0) `shouldSatisfy` (\ws -> take 2 (drop 1 ws) == ["10", "8"])
      Nothing -> expectationFailure ("no backends table for S in:\n" <> T.unpack out)
    ls `shouldSatisfy` elem "backends (project)"
    ls `shouldSatisfy` elem "tactics (project)"

  it "show with no policy says so" $ withHome $ \home -> do
    seeded home
    out <- learnIn home home ["show", "--project", projA]
    out `shouldSatisfy` T.isInfixOf "no policy for this project"

  it "diff after writing the policy the stats give prints no changes" $ withHome $ \home -> do
    seeded home
    withRunlog (runlogPath home) $ \rl -> do
      st <- loadStats rl (projectKeyOf projA)
      let key = projectKeyOf projA
      writePolicy (policyPath home key) (computePolicy 5 key (savedDefaults (object [])) st (policyShapes [] st Nothing))
    out <- learnIn home home ["diff", "--project", projA]
    T.strip out `shouldBe` "no changes"
    shown <- learnIn home home ["show", "--project", projA]
    shown `shouldSatisfy` T.isInfixOf "shape S"
    shown `shouldSatisfy` T.isInfixOf "backends fisher, claude-code"

  it "diff with no policy lists what a recompute would write" $ withHome $ \home -> do
    seeded home
    out <- learnIn home home ["diff", "--project", projA]
    out `shouldSatisfy` T.isInfixOf "S backends: null -> [\"fisher\",\"claude-code\"]"

  it "reset removes the policy file, and says no policy when there is none" $ withHome $ \home -> do
    seeded home
    let path = policyPath home (projectKeyOf projA)
    withRunlog (runlogPath home) $ \rl -> do
      st <- loadStats rl (projectKeyOf projA)
      writePolicy path (computePolicy 5 (projectKeyOf projA) (savedDefaults (object [])) st ["S"])
    _ <- learnIn home home ["reset", "--project", projA]
    doesFileExist path `shouldReturn` False
    again <- learnIn home home ["reset", "--project", projA]
    T.strip again `shouldBe` "no policy"

  it "an empty run log prints the table header and then the import hint" $ withHome $ \home -> do
    out <- learnIn home home ["stats", "--project", projA]
    let ls = T.lines out
    case findIndex (== "no runs logged yet: fisher runs import ~/.cache/fisher/search") ls of
      Just i -> do
        i `shouldSatisfy` (> 0)
        (ls !! (i - 1)) `shouldSatisfy` T.isInfixOf "attempts"
      Nothing -> expectationFailure ("no hint in:\n" <> T.unpack out)
    doesFileExist (runlogPath home) `shouldReturn` False

  it "takes the project from the working directory's Lake project" $ withHome $ \home -> do
    let root = home </> "proj"
    createDirectoryIfMissing True (root </> "Sub")
    writeFile (root </> "lakefile.toml") "name = \"p\"\n"
    seeded home
    withRunlog (runlogPath home) $ \rl -> do
      st <- loadStats rl (projectKeyOf root)
      writePolicy (policyPath home (projectKeyOf root)) (computePolicy 5 (projectKeyOf root) (savedDefaults (object [])) st [])
    out <- learnIn home (root </> "Sub") ["show"]
    out `shouldSatisfy` T.isInfixOf "shape *"
    T.unpack out `shouldSatisfy` ("policy " `isPrefixOf`)

  it "fisher learn --help lists the four commands" $
    case execParserPure defaultPrefs (info (learnParser <**> helper) mempty) ["--help"] of
      Failure f -> do
        let (text, _) = renderFailure f "fisher learn"
        forM_ ["stats", "show", "diff", "reset"] $ \c -> T.words (T.pack text) `shouldSatisfy` elem c
      _ -> expectationFailure "--help did not fail with the help text"
