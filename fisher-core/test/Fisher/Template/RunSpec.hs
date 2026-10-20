module Fisher.Template.RunSpec (spec) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Project (renderManifest)
import Fisher.Template
import Fisher.Template.Run
import Fisher.TestUtil (withTmp)

sh :: Text -> Text -> Step
sh name script = Step name ["sh", "-c", script] 1

-- | A project directory with a manifest holding a name, as create writes it.
withProject :: (FilePath -> IO a) -> IO a
withProject k = withTmp $ \d -> do
  createDirectoryIfMissing True (d </> ".fisher")
  BS.writeFile (d </> ".fisher" </> "project.yaml")
    (renderManifest (object ["schema_version" .= (1 :: Int), "name" .= ("Demo" :: Text), "kind" .= ("lean" :: Text)]))
  k d

collect :: IO (Text -> IO (), IO [Text])
collect = do
  r <- newIORef []
  pure (\l -> modifyIORef' r (l :), reverse <$> readIORef r)

-- | The (event, step, exit) of each line of the events file.
eventSummary :: FilePath -> IO [(Text, Text, Maybe Int)]
eventSummary d = map f <$> readEvents d
  where
    f = \case
      StepStart s -> ("step start", s, Nothing)
      StepEnd s n _ -> ("step end", s, Just n)
      CheckEnd _ _ -> ("check", "", Nothing)
      Ready -> ("ready", "", Nothing)
      Failed _ -> ("failed", "", Nothing)

-- | A process is gone when /proc has no entry for it or it is a zombie
-- waiting for its reaper. Polls for up to five seconds.
eventuallyGone :: Int -> IO Bool
eventuallyGone pid = go (50 :: Int)
  where
    go 0 = gone
    go n = gone >>= \g -> if g then pure True else threadDelay 100000 >> go (n - 1)
    gone = do
      let stat = "/proc/" <> show pid <> "/stat"
      e <- doesFileExist stat
      if not e
        then pure True
        else do
          r <- try (BS.readFile stat) :: IO (Either SomeException BS.ByteString)
          pure $ case r of
            Left _ -> True
            Right s -> case BC.words (snd (BC.breakEnd (== ')') s)) of
              (st : _) -> st `elem` ["Z", "X"]
              _ -> False

readPid :: FilePath -> IO Int
readPid p = read . BC.unpack . BC.strip <$> BS.readFile p

waitForFile :: FilePath -> IO ()
waitForFile p = go (100 :: Int)
  where
    go 0 = expectationFailure ("never appeared: " <> p)
    go n = doesFileExist p >>= \e -> if e then BS.readFile p >>= \c -> if BC.null (BC.strip c) then threadDelay 50000 >> go (n - 1) else pure () else threadDelay 50000 >> go (n - 1)

-- | A configured template in a directory of its own.
writeTemplate :: FilePath -> Text -> IO Template
writeTemplate dir greeting = do
  createDirectoryIfMissing True dir
  BS.writeFile (dir </> "Main.lean") (BC.pack ("-- " <> T.unpack greeting <> "\n"))
  BS.writeFile (dir </> manifestName) $ BC.unlines
    [ "name: mine", "prover: lean4", "description: Mine", "files: [Main.lean]"
    , "steps:", "  - name: build", "    run: [lake, build]", "sample: Main.lean" ]
  loadTemplate (Configured (T.pack dir)) dir >>= either (fail . T.unpack) pure

spec :: Spec
spec = do
  describe "runSteps" $ do
    let three two = [sh "one" "echo a", sh "two" two, sh "three" "echo c"]

    it "stops at the failing step, names it and the log, and never runs the next" $ withProject $ \d -> do
      (sink, lines') <- collect
      r <- runSteps d (three "exit 3") sink
      r `shouldBe` Left "step two failed (exit 3); see .fisher/create.log"
      eventSummary d `shouldReturn`
        [("step start", "one", Nothing), ("step end", "one", Just 0), ("step start", "two", Nothing), ("step end", "two", Just 3)]
      doneSteps d `shouldReturn` Right ["one"]
      ls <- lines'
      length ls `shouldBe` 2
      logText <- BS.readFile (d </> ".fisher" </> "create.log")
      BC.lines logText `shouldSatisfy` elem "a"
      BC.lines logText `shouldSatisfy` notElem "c"

    it "resumes at the step that failed, skipping the ones recorded done" $ withProject $ \d -> do
      _ <- runSteps d (three "exit 3") (const (pure ()))
      r <- runSteps d (three "echo b") (const (pure ()))
      r `shouldBe` Right ()
      evs <- eventSummary d
      [s | ("step start", s, _) <- evs] `shouldBe` ["one", "two", "two", "three"]
      doneSteps d `shouldReturn` Right ["one", "two", "three"]
      -- the rest of the manifest is kept, in its key order
      m <- Y.decodeFileThrow (d </> ".fisher" </> "project.yaml") :: IO Value
      case m of
        Object o -> KM.lookup "name" o `shouldBe` Just (String "Demo")
        _ -> expectationFailure "manifest is not a mapping"
      bytes <- BS.readFile (d </> ".fisher" </> "project.yaml")
      bytes `shouldBe` renderManifest m

    it "keeps both runs' output in the log after a resume" $ withProject $ \d -> do
      _ <- runSteps d (three "echo first-two; exit 3") (const (pure ()))
      _ <- runSteps d (three "echo second-two") (const (pure ()))
      ls <- BC.lines <$> BS.readFile (d </> ".fisher" </> "create.log")
      filter (`elem` ["a", "first-two", "second-two", "c"]) ls `shouldBe` ["a", "first-two", "second-two", "c"]
      length (filter ("== step two: exit" `BC.isPrefixOf`) ls) `shouldBe` 2

    it "refuses a manifest that does not parse, and leaves it as it is" $ withProject $ \d -> do
      let mf = d </> ".fisher" </> "project.yaml"
      BS.writeFile mf "name: [unclosed\n"
      r <- runSteps d (three "true") (const (pure ()))
      r `shouldSatisfy` either ("cannot read .fisher/project.yaml: " `T.isPrefixOf`) (const False)
      BS.readFile mf `shouldReturn` "name: [unclosed\n"
      readEvents d `shouldReturn` []
      doneSteps d >>= (`shouldSatisfy` either ("cannot read .fisher/project.yaml: " `T.isPrefixOf`) (const False))

    it "stops, without overwriting it, when a step leaves the manifest unreadable" $ withProject $ \d -> do
      let mf = d </> ".fisher" </> "project.yaml"
      r <- runSteps d [sh "one" "printf 'a: [\\n' > .fisher/project.yaml", sh "two" "true"] (const (pure ()))
      r `shouldSatisfy` either ("cannot read .fisher/project.yaml: " `T.isPrefixOf`) (const False)
      BS.readFile mf `shouldReturn` "a: [\n"
      evs <- eventSummary d
      [s' | ("step start", s', _) <- evs] `shouldBe` ["one"]

    it "keeps other fields under template when recording a step" $ withProject $ \d -> do
      BS.writeFile (d </> ".fisher" </> "project.yaml")
        (renderManifest (object ["name" .= ("Demo" :: Text), "template" .= object ["name" .= ("lean" :: Text), "revision" .= ("abc" :: Text)]]))
      _ <- runSteps d [sh "one" "true"] (const (pure ()))
      m <- Y.decodeFileThrow (d </> ".fisher" </> "project.yaml") :: IO Value
      m `shouldBe` object ["name" .= ("Demo" :: Text), "template" .= object ["name" .= ("lean" :: Text), "revision" .= ("abc" :: Text), "done" .= (["one"] :: [Text])]]

    it "kills a step at its time limit, with the group's children" $ withProject $ \d -> do
      let pidFile = d </> "child.pid"
      r <- runStepsWith 1 d [Step "slow" ["sh", "-c", "sleep 30 & echo $! > " <> T.pack pidFile <> "; wait"] 1] (const (pure ()))
      r `shouldBe` Left "step slow timed out after 1 min; see .fisher/create.log"
      child <- readPid pidFile
      eventuallyGone child `shouldReturn` True
      doneSteps d `shouldReturn` Right []
      eventSummary d `shouldReturn` [("step start", "slow", Nothing), ("step end", "slow", Just (-1))]

    it "kills the group when the thread running a step is cancelled, and does not record the step" $ withProject $ \d -> do
      let pidFile = d </> "child.pid"
      finished <- newEmptyMVar
      t <- forkIO $ do
        _ <- try (runSteps d [Step "long" ["sh", "-c", "sleep 30 & echo $! > " <> T.pack pidFile <> "; wait"] 1] (const (pure ())))
               :: IO (Either SomeException (Either Text ()))
        putMVar finished ()
      waitForFile pidFile
      child <- readPid pidFile
      killThread t
      takeMVar finished
      eventuallyGone child `shouldReturn` True
      doneSteps d `shouldReturn` Right []

    it "puts the last 40 output lines in the step end event" $ withProject $ \d -> do
      _ <- runSteps d [sh "many" "i=1; while [ $i -le 100 ]; do echo line$i; i=$((i+1)); done; echo oops >&2"] (const (pure ()))
      evs <- readEvents d
      case [t | StepEnd _ _ t <- evs] of
        [t] -> do
          length t `shouldBe` 40
          head t `shouldBe` "line62"
          last t `shouldBe` "oops"
        other -> expectationFailure ("step end events: " <> show other)

    it "runs each step with stdin closed, in the project directory" $ withProject $ \d -> do
      r <- runSteps d [sh "stdin" "if read x; then exit 9; fi; pwd > where"] (const (pure ()))
      r `shouldBe` Right ()
      BC.strip <$> BS.readFile (d </> "where") `shouldReturn` BC.pack d

    it "reports a command that cannot start" $ withProject $ \d -> do
      r <- runSteps d [Step "nope" ["fisher-no-such-command-xyz"] 1] (const (pure ()))
      r `shouldSatisfy` either ("step nope could not start" `T.isPrefixOf`) (const False)
      doneSteps d `shouldReturn` Right []

  describe "events" $ do
    it "writes one JSON line per event, in the documented shape" $ withTmp $ \d -> do
      forM_ [StepStart "a", StepEnd "a" 0 ["x"], CheckEnd False ["p"], Ready, Failed "r"] (writeEvent d)
      ls <- BC.lines <$> BS.readFile (d </> ".fisher" </> "create.jsonl")
      ls `shouldBe`
        [ "{\"event\":\"step start\",\"step\":\"a\"}"
        , "{\"event\":\"step end\",\"step\":\"a\",\"exit\":0,\"tail\":[\"x\"]}"
        , "{\"event\":\"check\",\"ok\":false,\"problems\":[\"p\"]}"
        , "{\"event\":\"ready\"}"
        , "{\"event\":\"failed\",\"reason\":\"r\"}" ]
      readEvents d `shouldReturn` [StepStart "a", StepEnd "a" 0 ["x"], CheckEnd False ["p"], Ready, Failed "r"]

  describe "trust" $ do
    it "trusts a template once recorded, and not after its revision changes" $ withTmp $ \d -> do
      let trust = d </> "fh" </> "fisher" </> "templates-trusted.json"
      t <- writeTemplate (d </> "tpl") "one"
      isTrusted trust t `shouldReturn` False
      recordTrust trust t
      isTrusted trust t `shouldReturn` True
      t' <- writeTemplate (d </> "tpl") "two"
      isTrusted trust t' `shouldReturn` False
      recordTrust trust t'
      isTrusted trust t' `shouldReturn` True

    it "keys the file by template name and keeps other entries" $ withTmp $ \d -> do
      let trust = d </> "templates-trusted.json"
      BS.writeFile trust "{\"other\":\"r1\"}"
      t <- writeTemplate (d </> "tpl") "one"
      recordTrust trust t
      rev <- templateRevision t
      Y.decodeFileThrow trust `shouldReturn` object ["other" .= ("r1" :: Text), "mine" .= rev]

    it "always trusts the built-ins" $ withTmp $ \d -> do
      ts <- withFisherHome (d </> "fh") builtinTemplates
      length ts `shouldBe` 2
      forM_ ts $ \t -> isTrusted (d </> "none.json") t `shouldReturn` True

    it "treats a missing or unreadable file as trusting nothing" $ withTmp $ \d -> do
      t <- writeTemplate (d </> "tpl") "one"
      BS.writeFile (d </> "bad.json") "not json"
      isTrusted (d </> "bad.json") t `shouldReturn` False
      isTrusted (d </> "missing.json") t `shouldReturn` False

-- | FISHER_HOME pointed at a directory for the action.
withFisherHome :: FilePath -> IO a -> IO a
withFisherHome p act = bracket (lookupEnv "FISHER_HOME") restore (const (setEnv "FISHER_HOME" p >> act))
  where
    restore = maybe (unsetEnv "FISHER_HOME") (setEnv "FISHER_HOME")
