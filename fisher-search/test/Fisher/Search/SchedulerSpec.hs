module Fisher.Search.SchedulerSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Check (AxiomPolicy (..), standardAxioms)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.Lean
import Fisher.Search.Scheduler

file :: T.Text
file = T.unlines
  [ "import Mathlib"
  , ""
  , "theorem a : A := by"
  , "  sorry"
  , ""
  , "theorem b : B := by"
  , "  sorry"
  , ""
  , "theorem c : C := by"
  , "  sorry"
  ]

-- | Attempts per (goal, backend): the proof to write and a delay in ms.
type Script = Map.Map (T.Text, T.Text) (T.Text, Int)

fakeRun :: Script -> AttemptSpec -> IO AttemptResult
fakeRun script sp = do
  let (proof, delay) = fromMaybe ("sorry", 0) (Map.lookup (asGoal sp, asBackend sp) script)
  threadDelay (delay * 1000)
  let ls = T.splitOn "\n" (asStart sp)
      d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
      out = T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)
  pure (plainResult (Just out) Nothing "")

search :: SearchConfig -> Script -> IO (SearchReport, [Event])
search cfg script = withSystemTempDirectory "search" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  let env = SearchEnv
        { seRun = fakeRun script, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seNow = realToFrac <$> getPOSIXTime, seLog = \e -> modifyIORef events (e :), seBaseChanged = \_ -> pure ()
        , seAuto = Nothing, seRetrieval = Nothing, sePrice = \_ -> pure Nothing }
  r <- runSearch cfg env file
  es <- reverse <$> readIORef events
  either (fail . T.unpack) (\rep -> pure (rep, es)) r

status :: SearchReport -> [(T.Text, GoalStatus)]
status rep = [(g, gsStatus s) | (g, s) <- srGoals rep]

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["claude-code", "gauss"], scSlots = 4, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = True}

spec :: Spec
spec = describe "the swarm's scheduler" $ do
  it "closes every goal and merges the proofs into one base" $ do
    (rep, _) <- search cfg0 {scParallelPerGoal = False}
      (Map.fromList [((g, "claude-code"), ("exact " <> g <> "_ok", 10)) | g <- ["a", "b", "c"]])
    status rep `shouldBe` [("a", Closed), ("b", Closed), ("c", Closed)]
    srClosed rep `shouldBe` 3
    length (openGoals (srBase rep)) `shouldBe` 0
    T.isInfixOf "exact b_ok" (srBase rep) `shouldBe` True
  it "gives every goal a first attempt before any second, spread over backends" $ do
    (rep, es) <- search cfg0 {scSlots = 3, scParallelPerGoal = False}
      (Map.fromList [((g, b), ("ok", 20)) | g <- ["a", "b", "c"], b <- ["claude-code", "gauss"]])
    srClosed rep `shouldBe` 3
    [(g, b) | Launched _ _ g b <- es] `shouldBe` [("a", "claude-code"), ("b", "gauss"), ("c", "claude-code")]
  it "retries a failed goal on the backend that has not tried it" $ do
    (rep, es) <- search cfg0 {scSlots = 1}
      (Map.fromList [ (("a", "claude-code"), ("BAD", 5)), (("a", "gauss"), ("ok", 5))
                    , (("b", "claude-code"), ("ok", 5)), (("c", "claude-code"), ("ok", 5)) ])
    lookup "a" (status rep) `shouldBe` Just Closed
    [b | Launched _ _ "a" b <- es] `shouldBe` ["claude-code", "gauss"]
  it "keeps a proof that uses an open goal waiting, and closes it when that goal closes" $ do
    (rep, es) <- search cfg0 {scSlots = 3, scParallelPerGoal = False}
      (Map.fromList [ (("a", "claude-code"), ("exact b", 5)), (("b", "gauss"), ("ok", 200)), (("c", "claude-code"), ("ok", 5)) ])
    status rep `shouldBe` [("a", Closed), ("b", Closed), ("c", Closed)]
    [g | GoalWaiting _ g _ <- es] `shouldBe` ["a"]
  it "runs a goal on two backends at once and cancels the slower when one closes it" $ do
    (rep, es) <- search cfg0 {scSlots = 6}
      (Map.fromList ([ (("a", "claude-code"), ("ok", 5)), (("a", "gauss"), ("ok", 5000)) ]
                     <> [((g, b), ("ok", 5)) | g <- ["b", "c"], b <- ["claude-code", "gauss"]]))
    lookup "a" (status rep) `shouldBe` Just Closed
    [g | AttemptCancelled _ _ g _ <- es] `shouldSatisfy` elem "a"
    srElapsed rep `shouldSatisfy` (< 4)
  it "gives up after the attempts allowed" $ do
    (rep, es) <- search cfg0 {scSlots = 2, scMaxAttempts = 2, scParallelPerGoal = False}
      (Map.fromList ([((g, b), ("BAD", 5)) | g <- ["a"], b <- ["claude-code", "gauss"]] <> [((g, "claude-code"), ("ok", 5)) | g <- ["b", "c"]]))
    lookup "a" (status rep) `shouldBe` Just GaveUp
    length [() | Launched _ _ "a" _ <- es] `shouldBe` 2
  it "stops at the budget and cancels what runs" $ do
    (rep, es) <- search cfg0 {scBudget = 0.5}
      (Map.fromList [((g, b), ("ok", 60000)) | g <- ["a", "b", "c"], b <- ["claude-code", "gauss"]])
    srBudgetSpent rep `shouldBe` True
    srClosed rep `shouldBe` 0
    [() | BudgetSpent _ <- es] `shouldBe` [()]
    srElapsed rep `shouldSatisfy` (< 3)
  it "picks nothing when every goal is decided" $
    pickLaunch cfg0 ["a"] (Map.fromList [("a", GoalState Closed [] [] Nothing Nothing Nothing Nothing Nothing False Nothing [])]) Map.empty `shouldBe` Nothing
