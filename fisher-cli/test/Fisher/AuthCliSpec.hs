-- | upstream's @status@, @logout@ and @login@ commands (with the argparse
-- help and errors of the auth commands) against upstream's recorded runs
-- (@tools/corpus/b1_auth.py@: @status.json@, @logout.json@,
-- @login.json@). Each case, and each later step of it, is replayed
-- through a scripted 'Sys' (later steps in the same world, each one
-- snapshotted when it ends) and compared on the transcript, exit code, HTTP
-- requests, subprocesses, browser and every file left behind.
module Fisher.AuthCliSpec (spec) where

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Cli.AuthArgs
import Fisher.Cli.Login (loginMain)
import Fisher.Cli.Logout (logoutMain)
import Fisher.Cli.Status (StatusInfo (..), statusMain)
import Fisher.LLM.Auth.Registry (removedProviderMessage)
import Fisher.LLM.Replay
import Fisher.LLM.Sys
import Fisher.Py.Value

-- | The recording masks upstream's install path and Python version.
info :: StatusInfo
info = StatusInfo "<SRC>" "<PYVER>"

-- | @fisher <argv>@ for the commands these recordings cover.
dispatch :: PyVal -> Sys -> IO Int
dispatch c sys = case maybe [] (\case PList xs -> [pyStrOf x | x <- xs]; _ -> []) (get "argv" c) of
  (_ : "status" : rest) -> statusMain info sys rest
  (_ : "logout" : rest) -> logoutMain sys rest
  (_ : "login" : rest) -> loginMain sys rest
  (_ : "setup" : rest) -> argsOnly setupParser rest
  (_ : "model" : rest) -> argsOnly modelParser rest
  argv -> fail ("no command for " <> show argv)
  where
    argsOnly p rest = case parseArgs p rest of
      Left (msg, code) -> sysOut sys msg >> pure code
      Right _ -> fail "setup and model runs are not covered by these recordings"

httpView :: PyVal -> PyVal
httpView e =
  obj
    [ (k, norm k v)
    | k <- ["via", "method", "url", "headers", "body", "timeout", "status", "unmatched", "raised"]
    , Just v <- [get k e] ]
  where
    norm "timeout" (PInt n) = PFloat (fromInteger n)
    norm _ v = v

listOf :: T.Text -> PyVal -> [PyVal]
listOf k c = case get k c of
  Just (PList xs) -> xs
  _ -> []

-- | Upstream's traceback runs through the recording harness's own frames,
-- which Fisher cannot print; the text before it and its last line (the
-- exception) are compared instead.
tracebackView :: T.Text -> (T.Text, T.Text)
tracebackView t = case T.breakOn "Traceback (most recent call last):" t of
  (pre, tb) | not (T.null tb) -> (pre, last ("" : filter (not . T.null) (T.lines tb)))
  _ -> (t, "")

golden :: FilePath -> Spec
golden file = do
  (doc, cases) <- runIO (loadCases ("test/golden/b1/" <> file))
  let soul = maybe "" pyStrOf (get "soul_md" doc)
  describe file $
    forM_ cases $ \(Case c0) -> it (T.unpack (maybe "?" pyStrOf (get "name" c0))) $ do
      let later = listOf "then" c0
          runStep mw c = do
            (w, o) <- replay doc mw c (dispatch c)
            files <- snapshotFiles w soul
            pure (c, w, o, files)
      first <- runStep Nothing c0
      let (_, w0, _, _) = first
      rest <- mapM (runStep (Just w0)) later
      forM_ (zip [0 :: Int ..] (first : rest)) $ \(i, (c, _, o, files)) -> do
        let tag = "step " <> show i <> ": "
        let want = maybe "" pyStrOf (get "transcript" c)
        (tag, tracebackView (oTranscript o)) `shouldBe` (tag, tracebackView want)
        (tag, oExit o) `shouldBe` (tag, maybe 0 (\case PInt n -> fromInteger n; _ -> -1) (get "exit" c))
        (tag, map httpView (oHttp o)) `shouldBe` (tag, map httpView (httpLog c))
        (tag, oSubprocess o) `shouldBe` (tag, [[pyStrOf a | a <- listOf "argv" s] | s <- listOf "subprocess" c])
        (tag, oBrowser o) `shouldBe` (tag, map pyStrOf (listOf "browser" c))
        let sleeps = [s | Just s <- map (get "seconds") (listOf "sleeps" c)]
        unless (null sleeps) $
          (tag, map PFloat (oSleeps o)) `shouldBe` (tag, map (\case PInt n -> PFloat (fromInteger n); v -> v) sleeps)
        (tag, files) `shouldBe` (tag, expectedFiles c)

spec :: Spec
spec = do
  golden "status.json"
  golden "logout.json"
  golden "login.json"
  describe "status when auth.json's active provider is the removed Nous Portal" $ do
    let run platform = do
          let c =
                obj
                  [ ("name", str "active-nous"), ("mode", str "pipe"), ("argv", PList [str "fisher", str "status"]), ("env", obj [])
                  , ("scenario", obj [("platform", str platform)])
                  , ("initial_files", obj [("fisher_home", obj [("auth.json", obj [("mode", str "0600"), ("text", str activeNous)])]), ("home", obj [])]) ]
              doc = obj [("epoch", PInt 1790000000), ("env_base", obj [("PATH", str "/usr/bin:/bin")])]
          snd <$> replay doc Nothing c (dispatch c)
    it "says so on the Provider line" $ do
      o <- run "linux"
      filter ("  Provider:" `T.isPrefixOf`) (T.lines (oTranscript o)) `shouldBe` ["  Provider:     " <> removedProviderMessage]
      oExit o `shouldBe` 0
    forM_ ["linux", "darwin"] $ \platform ->
      it ("says on " <> T.unpack platform <> " that the gateway is not part of Fisher, and runs nothing") $ do
        o <- run platform
        T.isInfixOf "Gateway Service\n  Status:       not part of Fisher\n" (oTranscript o) `shouldBe` True
        oSubprocess o `shouldBe` []
  describe "--provider naming the removed Nous Portal provider" $
    forM_ [(loginParser, "fisher login"), (logoutParser, "fisher logout")] $ \(p, prog) ->
      forM_ [["--provider", "nous"], ["--provider=Nous"], ["--prov", "nous"]] $ \args ->
        it (T.unpack (T.unwords (prog : args))) $
          parseArgsOk p args
            `shouldBe` Left (apUsage p <> prog <> ": error: argument --provider: " <> removedProviderMessage <> "\n", 2)
  where
    activeNous = "{\n  \"version\": 1,\n  \"providers\": {\n    \"nous\": {\"access_token\": \"at-1\"}\n  },\n  \"active_provider\": \"nous\"\n}\n"
    parseArgsOk p args = either Left (const (Right ())) (parseArgs p args)
