-- | CLM with a store replays (docs/superpowers/specs/2026-10-14-clm-store-design.md
-- section 4): each store scenario of "Fisher.Agent.ClmStoreScenarioSpec"
-- runs with a replay log, and 'replayFileWith' (recall on the harness's
-- fake RLM) runs it again from the log. The log holds the config's store
-- and spill, the loaded store, recall's configuration, and a @clm_spill@
-- (item, turn, chars) or @clm_archive@ (items, turns, why, chars) event
-- for each spill and each archive step; a log with one of them changed
-- reports a difference, and an old CLM log (no store key) replays with the
-- store off.
module Fisher.Agent.ClmStoreReplaySpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.AuxSpec (testSys)
import Fisher.Agent.ClmHarness (fakeRepl, runScenarioWith)
import Fisher.Agent.ClmStoreScenarioSpec (scenarios, storeOn, storeScenarios2, toolScenarios)
import Fisher.Agent.Context.Clm (ClmConfig (..), clmConfigFromPy)
import Fisher.Agent.Context.ClmStore (Item (..), ItemKind (..), Store (..), storeToPy)
import Fisher.Agent.Replay
import Fisher.Agent.TestUtil
import Fisher.Py.Value

record :: FilePath -> PyVal -> IO FilePath
record dir sc = do
  lg <- openReplayLog dir "clm-store"
  _ <- runScenarioWith (Just lg) sc
  closeReplayLog lg
  pure (replayPath dir "clm-store")

replay :: FilePath -> FilePath -> IO ReplayReport
replay dir path = do
  let base = ReplayBase (testSys dir [] (\_ -> pure ())) (LocalTime (fromGregorian 2026 10 14) (TimeOfDay 9 0 0)) (pure (PDict [])) (const Nothing)
  replayFileWith (const fakeRepl) base path >>= either (fail . T.unpack) pure

events :: FilePath -> IO [PyVal]
events path = do
  ls <- BC.lines <$> BS.readFile path
  pure [v | l <- ls, Right v <- [loads l]]

kindIs :: Text -> PyVal -> Bool
kindIs k v = get "kind" v == Just (PStr k)

ofKind :: Text -> [PyVal] -> [PyVal]
ofKind k = filter (kindIs k)

-- | Rewrite the first event the predicate picks.
rewriteFirst :: (PyVal -> Bool) -> (PyVal -> PyVal) -> FilePath -> IO ()
rewriteFirst pick f path = do
  vs <- events path
  let go _ [] = []
      go done (v : rest)
        | not done && pick v = f v : go True rest
        | otherwise = v : go done rest
  if any pick vs then pure () else expectationFailure "no event to change"
  BS.writeFile path (BC.unlines (map (TE.encodeUtf8 . dumpsAscii) (go False vs)))

-- | The log without the first event the predicate picks.
dropFirst :: (PyVal -> Bool) -> FilePath -> IO ()
dropFirst pick path = do
  vs <- events path
  let (pre, post) = break pick vs
  case post of
    [] -> expectationFailure "no event to drop"
    (_ : rest) -> BS.writeFile path (BC.unlines (map (TE.encodeUtf8 . dumpsAscii) (pre <> rest)))

named :: Text -> PyVal
named n = head [sc | sc <- scenarios <> toolScenarios <> storeScenarios2, txt "name" sc == n]

-- | The store scenarios: the spill and archive scripts with the store on,
-- and the tool scripts (which have it on already).
storeScenarios :: [PyVal]
storeScenarios = map storeOn scenarios <> toolScenarios

-- | The store a resumed session loads: S1 to S3 (ids 1, 2 and 7, so the
-- next is 8), from positions of an earlier process.
loadedStore :: Store
loadedStore =
  Store
    [ Item 1 Spilled Nothing (Just "terminal") (Just 2) (obj [("role", PStr "tool"), ("content", PStr "old build"), ("tool_call_id", PStr "c1")])
    , Item 2 Archived (Just 4) Nothing (Just 3) (obj [("role", PStr "assistant"), ("content", PStr "old plan")])
    , Item 7 Archived (Just 9) Nothing (Just 4) (obj [("role", PStr "assistant"), ("content", PStr "older plan")]) ]
    3
    mempty

intOf :: PyVal -> Maybe Integer
intOf = \case
  PInt n -> Just n
  _ -> Nothing

spec :: Spec
spec = describe "CLM with a store: replay" $ do
  describe "a recorded store session replays with no differences" $
    forM_ storeScenarios $ \sc -> it (T.unpack (txt "name" sc)) $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir sc
      r <- replay dir path
      rpMismatches r `shouldBe` []
  describe "the log" $ do
    it "the clm record has the config's store and spill" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path0 <- record dir (storeOn (named "three-spills"))
      vs <- events path0
      case ofKind "clm" vs of
        [c] -> do
          (get "config" c >>= get "store") `shouldBe` Just (PBool True)
          (get "config" c >>= get "spill") `shouldBe` Just (PFloat 0.25)
          get "store" c `shouldBe` Just PNull
          (get "recall" c >>= get "model") `shouldBe` Just (PStr "openai/gpt-5")
        cs -> expectationFailure ("clm records: " <> show (length cs))
    it "with the store off, a spill share other than the default is not logged: the record is as before the store" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      let sc = named "three-spills"
      path <- record dir (set "config" (set "spill" (PFloat 0.05) (field "config" sc)) sc)
      vs <- events path
      [get "config" c >>= get "spill" | c <- ofKind "clm" vs] `shouldBe` [Nothing]
      [get "config" c >>= get "store" | c <- ofKind "clm" vs] `shouldBe` [Nothing]
    it "a spill share other than the default is logged and replays" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "store-spill-low")
      vs <- events path
      [get "config" c >>= get "spill" | c <- ofKind "clm" vs] `shouldBe` [Just (PFloat 0.05)]
    it "each spill: its item, turn and characters" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      vs <- events path
      let spills = ofKind "clm_spill" vs
      [get "item" e >>= intOf | e <- spills] `shouldBe` map Just [1, 2, 3]
      -- the CLM turns the previews are (the assistant that called them is
      -- turn 1; the task is pinned)
      [get "turn" e | e <- spills] `shouldBe` map (Just . PInt) [2, 3, 4]
      [get "chars" e >>= intOf | e <- spills] `shouldBe` replicate 3 (Just 20000)
      ofKind "clm_archive" vs `shouldBe` []
    it "a spilled result its batch rolled back has no turn" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "resume-rollback")
      vs <- events path
      [get "turn" e | e <- ofKind "clm_spill" vs] `shouldBe` replicate 3 (Just PNull)
      r <- replay dir path
      rpMismatches r `shouldBe` []
    it "each archive step: its items, turns, why and characters" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "rollback-stored"))
      vs <- events path
      case ofKind "clm_archive" vs of
        [a] -> do
          (get "items" a >>= \case PList xs -> traverse intOf xs; _ -> Nothing) `shouldBe` Just [1 .. 6]
          get "why" a `shouldBe` Just (PStr "rolled_back")
          (case get "turns" a of Just (PList ts) -> length ts; _ -> 0) `shouldBe` 6
          -- the five results and the assistant that called them
          (get "chars" a >>= intOf) `shouldSatisfy` maybe False (> 25000)
        as -> expectationFailure ("clm_archive events: " <> show (length as))
    it "each recall: question, answer, ending, iterations, calls and dollars (as rlm_recall)" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "store-tools")
      vs <- events path
      let recalls = ofKind "clm_recall" vs
      map (get "question") recalls `shouldBe` [Just (PStr "What failed?"), Just (PStr "Again?")]
      map (get "answer") recalls `shouldBe` [Just (PStr "answer B"), Just (PStr "answer C")]
      map (get "ending") recalls `shouldBe` replicate 2 (Just (PStr "answered"))
      map (get "iterations") recalls `shouldBe` replicate 2 (Just (PInt 1))
      map (get "calls") recalls `shouldBe` replicate 2 (Just (PInt 1))
      map (get "dollars") recalls `shouldBe` replicate 2 (Just (PFloat 0.25))
      [map fst kvs | PDict kvs <- recalls] `shouldBe` replicate 2 ["kind", "question", "answer", "ending", "iterations", "calls", "dollars"]
      ofKind "rlm_recall" vs `shouldBe` []
    it "no recall, no clm_recall event" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      ofKind "clm_recall" <$> events path `shouldReturn` []
    it "a resumed session's log holds the store it loaded, and replays; new ids continue at 8" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (set "store_load" (storeToPy loadedStore) (storeOn (named "three-spills")))
      vs <- events path
      -- as loaded: the next id from the highest
      [get "store" c | c <- ofKind "clm" vs] `shouldBe` [Just (storeToPy loadedStore {sNext = 8})]
      [get "item" e >>= intOf | e <- ofKind "clm_spill" vs] `shouldBe` map Just [8, 9, 10]
      r <- replay dir path
      rpMismatches r `shouldBe` []
  describe "a changed log reports a difference" $ do
    it "a spill's characters" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      rewriteFirst (kindIs "clm_spill") (set "chars" (PInt 1)) path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "spill 1 differs")
    it "a spill's item" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      rewriteFirst (kindIs "clm_spill") (set "item" (PInt 9)) path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "spill 1 differs")
    it "an archive's why" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "rollback-stored"))
      rewriteFirst (kindIs "clm_archive") (set "why" (PStr "deleted")) path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "archive 1 differs")
    it "an archive's items" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "rollback-stored"))
      rewriteFirst (kindIs "clm_archive") (set "items" (PList [PInt 1])) path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "archive 1 differs")
    it "a spill missing from the log" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      dropFirst (kindIs "clm_spill") path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "spill 3: not in the log")
    it "a logged archive that was not made" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "rollback-stored"))
      vs <- events path
      let extra = head (ofKind "clm_archive" vs)
      BS.appendFile path (TE.encodeUtf8 (dumpsAscii extra) <> "\n")
      r <- replay dir path
      rpMismatches r `shouldSatisfy` elem "1 logged archive(s) were not made"
    it "a logged spill that was not made" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      vs <- events path
      BS.appendFile path (TE.encodeUtf8 (dumpsAscii (head (ofKind "clm_spill" vs))) <> "\n")
      r <- replay dir path
      rpMismatches r `shouldSatisfy` elem "1 logged spill(s) were not made"
    it "a recall's answer from recall's model" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "store-tools")
      let rootReply v = kindIs "http" v && T.isInfixOf "```repl\\\\nB\\\\n```" (dumpsAscii (maybe PNull id (get "response" v)))
          change v = case get "response" v of
            Just resp -> set "response" (set "body" (PStr (T.replace "repl\\nB\\n" "repl\\nZ\\n" (maybe "" pyStrOf (get "body" resp)))) resp) v
            Nothing -> v
      vs <- events path
      length (filter rootReply vs) `shouldBe` 1
      rewriteFirst rootReply change path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "recall 1 differs")
    it "a recall's logged answer" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "store-tools")
      rewriteFirst (kindIs "clm_recall") (set "answer" (PStr "answer Z")) path
      r <- replay dir path
      rpMismatches r `shouldSatisfy` any (T.isInfixOf "recall 1 differs")
    it "a logged recall that was not made" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "store-tools")
      vs <- events path
      BS.appendFile path (TE.encodeUtf8 (dumpsAscii (set "question" (PStr "Never asked?") (head (ofKind "clm_recall" vs)))) <> "\n")
      r <- replay dir path
      rpMismatches r `shouldSatisfy` elem "1 logged CLM recall(s) were not made"
  describe "an old CLM log" $ do
    it "has no store key, and replays unchanged with the store off" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (named "three-spills")
      vs <- events path
      case ofKind "clm" vs of
        [c] -> do
          (get "config" c >>= get "store") `shouldBe` Nothing
          (get "config" c >>= get "spill") `shouldBe` Nothing
          get "store" c `shouldBe` Nothing
          get "recall" c `shouldBe` Nothing
          let cc = clmConfigFromPy (get "config" c)
          (ccStore cc, ccSpill cc) `shouldBe` (False, 0.25)
        cs -> expectationFailure ("clm records: " <> show (length cs))
      ofKind "clm_spill" vs `shouldBe` []
      ofKind "clm_archive" vs `shouldBe` []
      r <- replay dir path
      rpMismatches r `shouldBe` []
    it "a store session's log with the store key removed replays with the store off (and differs)" $ withSystemTempDirectory "clm-store-replay" $ \dir -> do
      path <- record dir (storeOn (named "three-spills"))
      rewriteFirst (kindIs "clm") (\c -> maybe c (\cfg -> set "config" (delete "store" cfg) c) (get "config" c)) path
      r <- replay dir path
      -- the store off: nothing is spilled, so the logged spills are left over
      rpMismatches r `shouldSatisfy` elem "3 logged spill(s) were not made"
