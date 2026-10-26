module Fisher.Learn.StatsSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Learn.Stats
import Fisher.Runlog

projA, projB :: FilePath
projA = "/work/a"
projB = "/work/b"

keyA :: Text
keyA = projectKeyOf projA

-- | A search run of a project, with goals (name, shape).
search :: Runlog -> FilePath -> Text -> [(Text, Text)] -> IO ()
search rl proj pid goals = do
  startRun rl (newRun pid SearchRun Nothing proj Nothing Nothing Nothing 0)
  forM_ goals $ \(g, sh) -> upsertGoal rl pid (GoalRow g g sh "open" Nothing Nothing Nothing)

-- | An attempt of a search: id, backend, goal, shape, outcome, dollars, seconds.
attempt :: Runlog -> FilePath -> Text -> Text -> Text -> Text -> Text -> Text -> Double -> Double -> IO ()
attempt rl proj pid aid backend goal shape outcome dollars secs = do
  startRun rl (newRun aid SearchAttempt (Just pid) proj (Just backend) Nothing Nothing 100)
  event rl aid 0 "attempt" (object ["goal" .= goal, "shape" .= shape, "backend" .= backend])
  endRun rl aid (RunEnd (100 + secs) Nothing outcome (Just dollars) Nothing Nothing Nothing)

newRun :: Text -> RunKind -> Maybe Text -> FilePath -> Maybe Text -> Maybe Text -> Maybe Text -> Double -> NewRun
newRun i k par proj back model tier t =
  NewRun
    { nrId = i, nrKind = k, nrParent = par, nrProject = Just proj, nrProver = Just "lean", nrBackend = back
    , nrModel = model, nrTier = tier, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = t, nrRawDir = Nothing
    , nrCapture = Full }

-- | n attempts of a backend on one goal, the first c of them closed.
batch :: Runlog -> FilePath -> Text -> Text -> Text -> Text -> Int -> Int -> IO ()
batch rl proj pid backend goal shape n c =
  forM_ [1 .. n] $ \i ->
    attempt rl proj pid (pid <> "-" <> backend <> "-" <> goal <> "-" <> T.pack (show i)) backend goal shape
      (if i <= c then "closed" else "failed") 1 (if i <= c then 60 else 600)

withStats :: (Runlog -> IO ()) -> IO Stats
withStats fill = withSystemTempDirectory "stats" $ \tmp ->
  withRunlog (tmp </> "runs.db") $ \rl -> fill rl >> loadStats rl keyA

basic :: Runlog -> IO ()
basic rl = do
  search rl projA "search-a" [("g1", "S"), ("g2", "T")]
  batch rl projA "search-a" "fisher" "g1" "S" 10 8
  batch rl projA "search-a" "claude-code" "g1" "S" 10 2

spec :: Spec
spec = do
  it "smooths the close rate of each backend on a shape" $ do
    st <- withStats basic
    smoothedRate st "S" KBackend "fisher" `shouldSatisfy` (> 0.7)
    smoothedRate st "S" KBackend "claude-code" `shouldSatisfy` (< 0.3)

  it "tables a knob's arms: the given order first, then the others seen, with their numbers" $ do
    st <- withStats basic
    let rows = statsTable st LShape "S" KBackend ["fisher"]
    map arArm rows `shouldBe` ["fisher", "claude-code"]
    case rows of
      f : _ -> do
        (arAttempts f, arCloses f) `shouldBe` (10, 8)
        arRate f `shouldBe` smoothedRate st "S" KBackend "fisher"
        arCostPerClose f `shouldBe` Just (costPerClose st "S" KBackend "fisher")
        arClosesPerHour f `shouldBe` closesPerHour st "S" KBackend "fisher"
      [] -> expectationFailure "no rows"
    map arArm (statsTable st LProject "*" KBackend []) `shouldBe` ["claude-code", "fisher"]
    statsTable st LProject "*" KTactic [] `shouldBe` []
    -- an arm the order names that has no attempts has no dollars per close
    map arCostPerClose (statsTable st LShape "S" KBackend ["codex"]) `shouldSatisfy` (\cs -> take 1 cs == [Nothing])
    tableShapes st `shouldBe` ["S"]

  it "counts a shape's attempts in the project over the backends" $ do
    st <- withStats $ \rl -> do
      basic rl
      search rl projB "search-b" [("h", "S")]
      batch rl projB "search-b" "fisher" "h" "S" 6 0
    shapeAttempts st "S" `shouldBe` 20
    shapeAttempts st "T" `shouldBe` 0

  it "gives a shape with one attempt a rate near its project's" $ do
    st <- withStats $ \rl -> do
      search rl projA "search-a" [("g1", "S"), ("g2", "T")]
      batch rl projA "search-a" "codex" "g1" "S" 20 18
      attempt rl projA "search-a" "one" "codex" "g2" "T" "closed" 1 60
    let project = smoothedRateAt st LProject "T" KBackend "codex"
    abs (smoothedRate st "T" KBackend "codex" - project) `shouldSatisfy` (< 0.05)

  it "gives a shape with no attempts exactly its project's rate" $ do
    st <- withStats basic
    smoothedRate st "T" KBackend "fisher" `shouldBe` smoothedRateAt st LProject "T" KBackend "fisher"
    smoothedRate st "*" KBackend "fisher" `shouldBe` smoothedRateAt st LProject "" KBackend "fisher"

  it "gives a new project another project's rate on the same shape" $ do
    st <- withStats $ \rl -> do
      search rl projB "search-b" [("h", "S")]
      batch rl projB "search-b" "fisher" "h" "S" 10 9
      batch rl projB "search-b" "fisher" "h2" "U" 10 1
    smoothedRate st "S" KBackend "fisher" `shouldBe` smoothedRateAt st LGlobalShape "S" KBackend "fisher"
    smoothedRate st "S" KBackend "fisher" `shouldSatisfy` (> smoothedRate st "U" KBackend "fisher" + 0.3)

  it "counts neither cancelled nor unknown attempts" $ do
    st <- withStats $ \rl -> do
      basic rl
      forM_ [1 .. 5 :: Int] $ \i -> do
        attempt rl projA "search-a" ("c" <> T.pack (show i)) "fisher" "g1" "S" "cancelled" 1 10
        attempt rl projA "search-a" ("u" <> T.pack (show i)) "fisher" "g1" "S" "unknown" 1 10
    cAttempts (cellAt st LShape "S" KBackend "fisher") `shouldBe` 10
    cCloses (cellAt st LShape "S" KBackend "fisher") `shouldBe` 8

  it "keeps other projects out of the project levels and in the global ones" $ do
    st <- withStats $ \rl -> do
      basic rl
      search rl projB "search-b" [("h", "S")]
      batch rl projB "search-b" "fisher" "h" "S" 6 0
    cAttempts (cellAt st LShape "S" KBackend "fisher") `shouldBe` 10
    cAttempts (cellAt st LProject "" KBackend "fisher") `shouldBe` 10
    cAttempts (cellAt st LGlobalShape "S" KBackend "fisher") `shouldBe` 16
    cAttempts (cellAt st LGlobal "" KBackend "fisher") `shouldBe` 16

  it "sums dollars and seconds and keeps the seconds to close per shape" $ do
    st <- withStats basic
    let c = cellAt st LShape "S" KBackend "fisher"
    cDollars c `shouldBe` 10
    cSeconds c `shouldBe` (8 * 60 + 2 * 600)
    Map.lookup "S" (stCloseSeconds st) `shouldBe` Just (replicate 10 (60, Nothing))

  it "keeps the budget each close ran at, from its attempt event" $ do
    st <- withStats $ \rl -> do
      search rl projA "search-a" [("g1", "S")]
      startRun rl (newRun "b1" SearchAttempt (Just "search-a") projA (Just "fisher") Nothing Nothing 100)
      event rl "b1" 0 "attempt" (object ["goal" .= ("g1" :: Text), "shape" .= ("S" :: Text), "backend" .= ("fisher" :: Text), "budget_secs" .= (600 :: Int)])
      endRun rl "b1" (RunEnd 160 Nothing "closed" (Just 1) Nothing Nothing Nothing)
    Map.lookup "S" (stCloseSeconds st) `shouldBe` Just [(60, Just 600)]
    Map.lookup "" (stCloseSeconds st) `shouldBe` Just [(60, Just 600)]

  it "skips an attempt of no known statement: an empty shape in its attempt event, or goal *" $ do
    st <- withStats $ \rl -> do
      basic rl
      attempt rl projA "search-a" "none" "fisher" "lemma_x" "" "closed" 1 30
      attempt rl projA "search-a" "whole" "fisher" "*" "other|||small" "closed" 1 7000
    cAttempts (cellAt st LProject "" KBackend "fisher") `shouldBe` 10
    [sh | ((_, sh, _, _), _) <- Map.toList (stCells st)] `shouldSatisfy` notElem "other|||small"
    Map.lookup "" (stCloseSeconds st) `shouldBe` Just (replicate 10 (60, Nothing))

  it "skips a goal whose row has an empty shape, for its attempts and its auto events" $ do
    st <- withStats $ \rl -> do
      basic rl
      search rl projA "search-c" [("g9", "")]
      startRun rl (newRun "c1" SearchAttempt (Just "search-c") projA (Just "fisher") Nothing Nothing 100)
      event rl "c1" 0 "attempt" (object ["goal" .= ("g9" :: Text), "backend" .= ("fisher" :: Text)])
      endRun rl "c1" (RunEnd 130 Nothing "closed" (Just 1) Nothing Nothing Nothing)
      event rl "search-c" 1 "auto missed" (object ["goal" .= ("g9" :: Text), "seconds" .= (10 :: Double)])
    cAttempts (cellAt st LProject "" KBackend "fisher") `shouldBe` 10
    [k | ((_, _, k@KTactic, _), _) <- Map.toList (stCells st)] `shouldBe` []
    Map.lookup "" (stCloseSeconds st) `shouldBe` Just (replicate 10 (60, Nothing))

  it "scores the model tier of native attempts by model" $ do
    st <- withStats $ \rl -> do
      search rl projA "search-a" [("g1", "S")]
      startRun rl (newRun "n1" SearchAttempt (Just "search-a") projA (Just "fisher") (Just "m2") (Just "1") 0)
      event rl "n1" 0 "attempt" (object ["goal" .= ("g1" :: Text), "shape" .= ("S" :: Text), "backend" .= ("fisher" :: Text)])
      endRun rl "n1" (RunEnd 30 Nothing "closed" (Just 0.5) Nothing Nothing Nothing)
    cCloses (cellAt st LShape "S" KTier "m2") `shouldBe` 1

  it "does not score a tiered attempt with no model on the tier knob" $ do
    st <- withStats $ \rl -> do
      search rl projA "search-a" [("g1", "S")]
      startRun rl (newRun "n1" SearchAttempt (Just "search-a") projA (Just "fisher") Nothing (Just "1") 0)
      event rl "n1" 0 "attempt" (object ["goal" .= ("g1" :: Text), "shape" .= ("S" :: Text), "backend" .= ("fisher" :: Text)])
      endRun rl "n1" (RunEnd 30 Nothing "closed" (Just 0.5) Nothing Nothing Nothing)
    [arm | ((_, _, KTier, arm), _) <- Map.toList (stCells st)] `shouldBe` []
    cCloses (cellAt st LShape "S" KBackend "fisher") `shouldBe` 1

  it "takes tactics from the auto events, a miss as one failure per default tactic" $ do
    st <- withStats $ \rl -> do
      search rl projA "search-a" [("g1", "S"), ("g2", "S")]
      event rl "search-a" 1 "auto closed" (object ["goal" .= ("g1" :: Text), "snippets" .= ("set_option maxHeartbeats 200000 in omega; set_option maxHeartbeats 200000 in simp_all [foo]; set_option maxHeartbeats 200000 in norm_num" :: Text), "seconds" .= (4 :: Double)])
      event rl "search-a" 2 "auto missed" (object ["goal" .= ("g2" :: Text), "seconds" .= (14 :: Double)])
    let omega = cellAt st LShape "S" KTactic "omega"
        rfl = cellAt st LShape "S" KTactic "rfl"
    (cAttempts omega, cCloses omega) `shouldBe` (2, 1)
    cCloses (cellAt st LShape "S" KTactic "simp_all") `shouldBe` 1
    (cAttempts rfl, cCloses rfl, cDollars rfl) `shouldBe` (1, 0, 0)
    cCloses (cellAt st LShape "S" KTactic "norm_num") `shouldBe` 1
    [arm | ((_, _, KTactic, arm), _) <- Map.toList (stCells st), arm `notElem` ["rfl", "decide", "omega", "simp_all", "aesop", "grind", "smt", "exact?"]]
      `shouldBe` ["norm_num", "norm_num", "norm_num", "norm_num"]

  it "charges a miss to the search's logged portfolio" $ do
    st <- withStats $ \rl -> do
      search rl projA "search-a" [("g1", "S")]
      event rl "search-a" 0 "portfolio" (object ["tactics" .= (["norm_num", "omega"] :: [Text])])
      event rl "search-a" 1 "auto missed" (object ["goal" .= ("g1" :: Text), "seconds" .= (10 :: Double)])
    [(arm, cAttempts c, cCloses c, cSeconds c) | ((LShape, "S", KTactic, arm), c) <- Map.toList (stCells st)]
      `shouldBe` [("norm_num", 1, 0, 5), ("omega", 1, 0, 5)]
    [arm | ((_, _, KTactic, arm), _) <- Map.toList (stCells st), arm `notElem` ["norm_num", "omega"]] `shouldBe` []

  it "maps a snippet back to the portfolio tactic that made it" $ do
    let p = ["rfl", "omega", "exact?", "apply?", "aesop"]
    map (snippetTactic p)
      [ "set_option maxHeartbeats 200000 in rfl"
      , "set_option maxHeartbeats 400000 in by omega"
      , "set_option maxHeartbeats 200000 in exact Nat.le_add_right_of_le h"
      , "apply Foo.bar"
      , "aesop"
      , "set_option maxHeartbeats 200000 in norm_num [foo]"
      , "" ]
      `shouldBe` [Just "rfl", Just "omega", Just "exact?", Just "apply?", Just "aesop", Just "norm_num", Nothing]

  it "reads the auto events of bench-import and import parents as a search's" $ do
    st <- withStats $ \rl -> forM_ [(BenchImport, "bench-a"), (ImportRun, "import-a")] $ \(k, pid) -> do
      startRun rl (newRun pid k Nothing projA Nothing Nothing Nothing 0)
      upsertGoal rl pid (GoalRow "g1" "g1" "S" "open" Nothing Nothing Nothing)
      event rl pid 0 "portfolio" (object ["tactics" .= (["omega"] :: [Text])])
      event rl pid 1 "auto missed" (object ["goal" .= ("g1" :: Text), "seconds" .= (4 :: Double)])
    cAttempts (cellAt st LShape "S" KTactic "omega") `shouldBe` 2
