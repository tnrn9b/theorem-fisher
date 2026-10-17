-- | Setting up @--context clm@ with the store (docs/superpowers/specs/2026-10-14-clm-store-design.md
-- sections 3.3 and 4), the parts that need no chat session: 'clmSetupFor'
-- with @fisher.clm.store@ on loads @store.json@ beside @state.json@ (ids
-- continue from the highest; an invalid file warns once and resumes with an
-- empty store), saves it atomically (a write failure warns once and the
-- session carries on), and sets up recall as @--context rlm@ does (the same
-- interpreter check; without python3 the store works without recall, with
-- one warning). With the store off none of this happens. The chat itself
-- is in "Fisher.ClmCliSpec".
module Fisher.ClmSetupSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Context.Clm (ClmSetup (..), MirrorIO (..))
import Fisher.Agent.Context.ClmStore (Item (..), ItemKind (..), Store (..), emptyStore, storeFromPy, storeToPy)
import Fisher.Agent.Replay (closeReplayLog, openReplayLog, replayPath)
import Fisher.Cli.Clm
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..))
import Fisher.RlmSetupSpec (pathSys, pythonDir)

sid :: Text
sid = "20261014_101500_ef56ab"

storeCfg :: PyVal
storeCfg = PDict [("fisher", PDict [("clm", PDict [("store", PBool True)])])]

-- | Items 1, 2 and 7 (hand-edited: not contiguous), with next 3.
gappy :: Store
gappy =
  Store
    [ Item 1 Spilled Nothing (Just "terminal") (Just 2) (PDict [("role", PStr "tool"), ("content", PStr "build"), ("tool_call_id", PStr "c1")])
    , Item 2 Archived (Just 4) Nothing (Just 3) (PDict [("role", PStr "assistant"), ("content", PStr "plan")])
    , Item 7 Archived (Just 9) Nothing (Just 5) (PDict [("role", PStr "assistant"), ("content", PStr "later plan")]) ]
    3
    (S.fromList [2, 3, 5])

-- | Set up in a fresh GAUSS_HOME under tmp: the result, every warning, and
-- store.json's path.
setupIn :: FilePath -> [(Text, Text)] -> PyVal -> IO (Either Text ClmSetup, [Text], FilePath)
setupIn tmp env cfg = do
  let gauss = tmp </> ".gauss"
  createDirectoryIfMissing True gauss
  (sys, _) <- pathSys gauss env
  warned <- newIORef []
  r <- clmSetupFor sys (\w -> modifyIORef warned (<> [w])) cfg sid "openai/gpt-5" Nothing
  ws <- readIORef warned
  pure (fmap fst r, ws, ctxDir gauss sid </> "store.json")

-- | Write store.json before the setup.
seed :: FilePath -> BS.ByteString -> IO ()
seed tmp bytes = do
  let file = ctxDir (tmp </> ".gauss") sid </> "store.json"
  createDirectoryIfMissing True (takeDirectory file)
  BS.writeFile file bytes

withPython :: IO [(Text, Text)]
withPython = (\py -> [("PATH", T.pack py)]) <$> pythonDir

ok :: Either Text a -> IO a
ok = either (fail . T.unpack) pure

spec :: Spec
spec = describe "clmSetupFor with the store" $ do
  describe "store off" $ do
    it "reads no store.json, sets up no recall and checks no interpreter" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "empty")
      seed tmp "{\"items\": [oops"
      (r, ws, _) <- setupIn tmp [("PATH", T.pack (tmp </> "empty"))] (PDict [])
      setup <- ok r
      csStoreLoad setup `shouldReturn` Nothing
      isNothing' (csRecall setup) `shouldBe` True
      ws `shouldBe` []
    it "a bad fisher.rlm section does not matter" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      (r, _, _) <- setupIn tmp [] (PDict [("fisher", PDict [("rlm", PDict [("colour", PStr "red")])])])
      either (const False) (const True) r `shouldBe` True
  describe "recall" $ do
    it "with python3: recall's agent defaults on the agent's model, nothing warned" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      (r, ws, _) <- setupIn tmp env storeCfg
      setup <- ok r
      case csRecall setup of
        Just (rc, _) -> do
          rcModel rc `shouldBe` "openai/gpt-5"
          rcMaxIterations rc `shouldBe` 15
          rcMaxBudget rc `shouldBe` Just 0.5
        Nothing -> expectationFailure "no recall"
      ws `shouldBe` []
    it "without python3: the store works without recall, with one warning" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "empty")
      (r, ws, _) <- setupIn tmp [("PATH", T.pack (tmp </> "empty"))] storeCfg
      setup <- ok r
      isNothing' (csRecall setup) `shouldBe` True
      csStoreLoad setup `shouldReturn` Nothing
      ws `shouldBe` ["fisher: python3 not found; CLM's store works without recall"]
    it "refuses a bad fisher.rlm section by name, as --context rlm does" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      (r, _, _) <- setupIn tmp env (PDict [("fisher", PDict [("clm", PDict [("store", PBool True)]), ("rlm", PDict [("colour", PStr "red")])])])
      either id (const "set up") r `shouldBe` "fisher.rlm: unknown key colour"
  describe "store.json" $ do
    it "missing: nothing is loaded, nothing warned" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      (r, ws, file) <- setupIn tmp env storeCfg
      setup <- ok r
      csStoreLoad setup `shouldReturn` Nothing
      doesFileExist file `shouldReturn` False
      ws `shouldBe` []
    it "ids 1, 2 and 7: loaded, and new ids continue at 8" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      seed tmp (TE.encodeUtf8 (dumpsAscii (storeToPy gappy)))
      (r, ws, _) <- setupIn tmp env storeCfg
      setup <- ok r
      loaded <- csStoreLoad setup
      fmap (map itId . sItems) loaded `shouldBe` Just [1, 2, 7]
      fmap sNext loaded `shouldBe` Just 8
      ws `shouldBe` []
    it "not valid: one warning, an empty store, however often it is loaded" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      seed tmp "{\"items\": [oops"
      warned <- newIORef []
      let gauss = tmp </> ".gauss"
      (sys, _) <- pathSys gauss env
      r <- clmSetupFor sys (\w -> modifyIORef warned (<> [w])) storeCfg sid "openai/gpt-5" Nothing
      setup <- ok (fmap fst r)
      csStoreLoad setup `shouldReturn` Nothing
      csStoreLoad setup `shouldReturn` Nothing
      readIORef warned `shouldReturn` ["fisher: store.json is not valid; resuming with an empty store"]
    it "valid JSON that is not a store: the same warning" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      seed tmp "{\"items\": 3}"
      (r, ws, _) <- setupIn tmp env storeCfg
      setup <- ok r
      csStoreLoad setup `shouldReturn` Nothing
      ws `shouldBe` ["fisher: store.json is not valid; resuming with an empty store"]
    it "saved beside state.json, atomically, and loaded back by the next setup" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      (r, _, file) <- setupIn tmp env storeCfg
      setup <- ok r
      csStoreSave setup gappy
      takeDirectory file `shouldBe` takeDirectory (T.unpack (mioPathOf setup))
      bytes <- BS.readFile file
      (either (const Nothing) storeFromPy (loads bytes)) `shouldBe` Just gappy {sNext = 8}
      -- no temporary file left beside it
      listDirectory (takeDirectory file) >>= (`shouldSatisfy` all (not . (".tmp" `T.isSuffixOf`) . T.pack))
      (r2, ws2, _) <- setupIn tmp env storeCfg
      setup2 <- ok r2
      fmap (map itId . sItems) <$> csStoreLoad setup2 `shouldReturn` Just [1, 2, 7]
      ws2 `shouldBe` []
    it "a write failure warns once and does not stop the session" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      warned <- newIORef []
      let gauss = tmp </> ".gauss"
      createDirectoryIfMissing True gauss
      (sys, _) <- pathSys gauss env
      r <- clmSetupFor sys (\w -> modifyIORef warned (<> [w])) storeCfg sid "openai/gpt-5" Nothing
      setup <- ok (fmap fst r)
      -- store.json is a directory: the rename onto it fails
      createDirectoryIfMissing True (ctxDir gauss sid </> "store.json" </> "x")
      csStoreSave setup emptyStore
      csStoreSave setup gappy
      ws <- readIORef warned
      ws `shouldBe` ["fisher: store.json cannot be written; the store stays in memory"]
  describe "the replay log" $
    it "records each spill and archive step" $ withSystemTempDirectory "clm-setup" $ \tmp -> do
      env <- withPython
      let gauss = tmp </> ".gauss"
      createDirectoryIfMissing True gauss
      (sys, _) <- pathSys gauss env
      lg <- openReplayLog gauss sid
      r <- clmSetupFor sys (\_ -> pure ()) storeCfg sid "openai/gpt-5" (Just lg)
      setup <- ok (fmap fst r)
      let it1 = head (sItems gappy)
      csOnSpill setup it1
      csOnArchive setup (drop 1 (sItems gappy)) "deleted"
      closeReplayLog lg
      ls <- BC.lines <$> BS.readFile (replayPath gauss sid)
      let vs = [v | l <- ls, Right v <- [loads l]]
      [get "kind" v | v <- vs] `shouldBe` [Just (PStr "clm_spill"), Just (PStr "clm_archive")]
      (get "item" (head vs)) `shouldBe` Just (PInt 1)
      (get "items" (vs !! 1)) `shouldBe` Just (PList [PInt 2, PInt 7])
      (get "why" (vs !! 1)) `shouldBe` Just (PStr "deleted")
  where
    isNothing' = \case
      Nothing -> True
      Just _ -> False
    mioPathOf = mioPath . csIO
