-- | The New project view's routes through the whole server: the templates
-- (the built-ins and a configured one), and creates run by a fake fisher
-- that writes a project's events file as the command line does, then
-- stops, retries and scratch environments.
module Fisher.UI.TemplatesSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, setFileMode)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Config (loadConfigAt)
import Fisher.Template (findTemplate, templateRevision)
import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Live (LiveEnv (..), newLiveEnv, stopRuns)
import Fisher.UI.Api.Search (newSearchEnv, stopSearches)
import Fisher.UI.Api.Templates (newCreateEnv, stopCreates)
import Fisher.UI.Db (newUiEnv)
import Fisher.UI.Server (allApis, serve)
import Fisher.UI.TestDirs (fixturesDir)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

call :: Application -> Method -> B.ByteString -> [Header] -> BL.ByteString -> IO SResponse
call app m p hs body = runSession (srequest (SRequest req body)) app
  where
    req =
      setPath
        defaultRequest
          { requestMethod = m
          , requestHeaderHost = Just "127.0.0.1:8732"
          , requestHeaders =
              hs <> [ ("Host", "127.0.0.1:8732"), (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")
                    , ("Origin", "http://127.0.0.1:8732") ] }
        p

getJ :: Application -> B.ByteString -> IO (Int, A.Value)
getJ app p = do
  r <- call app methodGet p [] ""
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

postJ :: Application -> B.ByteString -> A.Value -> IO (Int, A.Value)
postJ app p v = do
  r <- call app methodPost p [(hContentType, "application/json")] (A.encode v)
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

key :: Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

txt :: A.Value -> Text
txt = \case
  A.String t -> t
  _ -> ""

-- | The stream: the (id, data line) of each message, and the end event's data.
data Stream = Stream { stEvents :: [(Integer, B.ByteString)], stEnd :: Maybe A.Value }
  deriving (Show)

parseStream :: BL.ByteString -> Stream
parseStream body =
  let blocks = splitBlocks (BL.toStrict body)
      one b =
        let ls = BC.lines b
            field f = [B.drop (B.length f) l | l <- ls, f `B.isPrefixOf` l]
         in (field "event: ", field "id: ", field "data: ")
      parsed = map one blocks
   in Stream
        [(read (BC.unpack i), d) | ([], [i], [d]) <- parsed]
        (case [d | (["end"], _, [d]) <- parsed] of
           d : _ -> A.decodeStrict d
           [] -> Nothing)
  where
    splitBlocks s = case B.breakSubstring "\n\n" s of
      (a, rest) | B.null rest -> [a | not (B.null a)]
                | otherwise -> a : splitBlocks (B.drop 2 rest)

stream :: Application -> B.ByteString -> IO (Int, Stream)
stream app p = do
  r <- timeout 30000000 (call app methodGet p [] "")
  case r of
    Nothing -> fail ("the stream of " <> BC.unpack p <> " did not end")
    Just x -> pure (statusCode (simpleStatus x), parseStream (simpleBody x))

eventNames :: Stream -> [Text]
eventNames s = mapMaybe (\(_, d) -> A.decodeStrict d >>= \v -> case key "event" v of A.String t -> Just t; _ -> Nothing) (stEvents s)

-- | What a test has: the temporary directory, the Fisher home and the server.
data World = World { wTmp :: FilePath, wHome :: FilePath, wTemplate :: FilePath, wApp :: Application }

-- | A Fisher home whose config names a custom template @fake@ (one step,
-- @true@), and the server over it with a fake fisher whose body is the
-- given shell text (after it writes its arguments and cwd to the home).
-- FISHER_HOME is the home for the length of the test, as for the child,
-- so the built-ins are written out there.
withCreate :: Text -> (World -> IO a) -> IO a
withCreate body k = withSystemTempDirectory "fisher-ui-create" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let home = tmp </> "fisher"
      tpl = tmp </> "tpl"
      fake = tmp </> "fake-fisher"
  mapM_ (createDirectoryIfMissing True) [home, tpl]
  TIO.writeFile (tpl </> "fisher-template.yaml") $
    T.unlines
      [ "name: fake", "prover: lean4", "description: A custom test template", "files:", "  - Main.lean"
      , "steps:", "  - name: build", "    run: [\"true\", \"{{project}}\"]", "sample: Main.lean" ]
  TIO.writeFile (tpl </> "Main.lean") "theorem t : True := trivial\n"
  TIO.writeFile (home </> "config.yaml") ("fisher:\n  templates:\n    fake: " <> T.pack tpl <> "\n")
  TIO.writeFile fake ("#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$FISHER_HOME/args.txt\"\npwd > \"$FISHER_HOME/cwd.txt\"\n" <> body)
  setFileMode fake 0o755
  withFisherHome home $ do
    ui <- newUiEnv home (fixturesDir </> "bench") (tmp </> "repo")
    le0 <- newLiveEnv ui fake
    let le = le0 {leGrace = 1000000, leIdle = 1500000}
    se <- newSearchEnv le (tmp </> "search")
    ce <- newCreateEnv le
    r <- k (World tmp home tpl (serve gate (allApis le se ce)))
    stopCreates ce
    stopSearches se
    stopRuns le
    pure r
  where
    withFisherHome h act =
      bracket (lookupEnv "FISHER_HOME" <* setEnv "FISHER_HOME" h) (maybe (unsetEnv "FISHER_HOME") (setEnv "FISHER_HOME")) (const act)

-- | Poll the create until its status is the one wanted (up to 10 s).
statusBecomes :: Application -> Text -> Text -> IO A.Value
statusBecomes app cid want = go (200 :: Int)
  where
    go n = do
      (_, v) <- getJ app ("/api/create/" <> TE.encodeUtf8 cid)
      if key "status" v == A.String want
        then pure v
        else if n > 0 then threadDelay 50000 >> go (n - 1) else fail ("create " <> T.unpack cid <> " never became " <> T.unpack want <> ": " <> show v)

waitFile :: FilePath -> IO ()
waitFile p = do
  ok <- timeout 10000000 (let go = doesFileExist p >>= \y -> if y then threadDelay 100000 else threadDelay 50000 >> go in go)
  maybe (fail ("no " <> p)) pure ok

argsOf :: World -> IO [String]
argsOf w = waitFile (wHome w </> "args.txt") >> lines <$> readFile (wHome w </> "args.txt")

createIn :: World -> FilePath -> Text -> [(Text, Text)] -> IO (Int, A.Value)
createIn w path name vars =
  postJ (wApp w) "/api/create" (A.object ["template" A..= name, "path" A..= path, "vars" A..= A.object [(K.fromText k, A.String v) | (k, v) <- vars]])

-- | A template's revision as the server (and the child) sees it.
revOf :: World -> Text -> IO Text
revOf w name = do
  (cfg, _) <- loadConfigAt (wHome w)
  findTemplate cfg name >>= either (fail . T.unpack) templateRevision

idOf :: A.Value -> B.ByteString
idOf = TE.encodeUtf8 . txt . key "id"

spec :: Spec
spec = describe "Fisher.UI.Api.Templates" $ do
  describe "GET /api/templates" $
    it "lists the built-ins (trusted) and a configured template (not trusted yet) with their variables and steps" $ withCreate "exit 0" $ \w -> do
      (code, v) <- getJ (wApp w) "/api/templates"
      code `shouldBe` 200
      let byName n = case [t | t <- arr (key "templates" v), key "name" t == A.String n] of
            t : _ -> pure t
            [] -> fail ("no template " <> T.unpack n <> " in " <> show v)
      lean <- byName "lean"
      _ <- byName "lean-mathlib"
      fake <- byName "fake"
      (key "origin" lean, key "trusted" lean, key "prover" lean, key "source" lean) `shouldBe` (A.String "builtin", A.Bool True, A.String "lean4", A.Null)
      map (key "name") (arr (key "variables" lean)) `shouldMatchList` [A.String "project", A.String "lean"]
      [(key "name" s, key "run" s) | s <- arr (key "steps" lean)] `shouldBe` [(A.String "build", A.toJSON ["lake", "build" :: Text])]
      (key "origin" fake, key "trusted" fake, key "source" fake) `shouldBe` (A.String "configured", A.Bool False, A.String (T.pack (wTemplate w)))
      [(key "name" s, key "run" s, key "timeout_minutes" s) | s <- arr (key "steps" fake)] `shouldBe` [(A.String "build", A.toJSON ["true", "{{project}}" :: Text], A.Number 30)]
      key "notes" v `shouldBe` A.toJSON ([] :: [Text])

  describe "POST /api/create" $ do
    it "runs fisher project create PATH --template lean --var ... --yes in the home, streams its step events and ends ready" $
      withCreate stepsReady $ \w -> do
        let path = wTmp w </> "Demo"
        (code, v) <- createIn w path "lean" [("lean", "v4.9.0")]
        code `shouldBe` 200
        let cid = idOf v
        cid `shouldSatisfy` ("c." `B.isPrefixOf`)
        rev <- revOf w "lean"
        argsOf w `shouldReturn` ["project", "create", path, "--template", "lean", "--var", "lean=v4.9.0", "--trust-revision", T.unpack rev]
        readFile (wHome w </> "cwd.txt") `shouldReturn` (wHome w <> "\n")
        (_, s) <- stream (wApp w) ("/api/create/" <> cid <> "/events")
        eventNames s `shouldBe` ["step start", "step end", "step start", "step end", "check", "ready"]
        raw <- B.readFile (path </> ".fisher" </> "create.jsonl")
        map snd (stEvents s) `shouldBe` BC.lines raw
        map fst (stEvents s) `shouldBe` scanl1 (+) (map (fromIntegral . (+ 1) . B.length) (BC.lines raw))
        fmap (key "status") (stEnd s) `shouldBe` Just (A.String "ready")
        d <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "ready"
        (key "root" d, key "file" d, key "mode" d, key "running" d) `shouldBe` (A.String (T.pack path), A.String "Demo/Sample.lean", A.String "project", A.Bool False)
        -- at the end of a run that has ended: 204
        r <- call (wApp w) methodGet ("/api/create/" <> cid <> "/events?from=" <> BC.pack (show (B.length raw))) [] ""
        statusCode (simpleStatus r) `shouldBe` 204
        -- an offset inside a line is refused
        r2 <- call (wApp w) methodGet ("/api/create/" <> cid <> "/events?from=3") [] ""
        statusCode (simpleStatus r2) `shouldBe` 400

    it "answers a variable that does not match its pattern, an unknown one, a relative or full folder and an unknown template with 400 and the message, running nothing" $
      withCreate stepsReady $ \w -> do
        (c1, v1) <- createIn w (wTmp w </> "A") "lean" [("lean", "v5")]
        (c1, txt (key "error" v1)) `shouldSatisfy` (\(c, e) -> c == 400 && "Variable `lean` is `v5`, which does not match" `T.isInfixOf` e)
        (c2, v2) <- createIn w (wTmp w </> "A") "lean" [("mathlib", "latest")]
        (c2, txt (key "error" v2)) `shouldSatisfy` (\(c, e) -> c == 400 && "Unknown variable `mathlib`" `T.isInfixOf` e)
        (c3, v3) <- createIn w "rel/A" "lean" []
        (c3, txt (key "error" v3)) `shouldSatisfy` (\(c, e) -> c == 400 && "absolute" `T.isInfixOf` e)
        createDirectoryIfMissing True (wTmp w </> "full")
        writeFile (wTmp w </> "full" </> "x.txt") "x\n"
        (c4, v4) <- createIn w (wTmp w </> "full") "lean" []
        (c4, txt (key "error" v4)) `shouldSatisfy` (\(c, e) -> c == 400 && "not empty" `T.isInfixOf` e)
        (c5, _) <- createIn w (wTmp w </> "A") "no-such-template" []
        c5 `shouldBe` 400
        (c6, _) <- postJ (wApp w) "/api/create" (A.object ["template" A..= ("lean" :: Text)])
        c6 `shouldBe` 400
        threadDelay 200000
        doesFileExist (wHome w </> "args.txt") `shouldReturn` False

    it "answers 409 with the rendered steps and the revision for a template not trusted yet, and runs it once that revision is confirmed" $ withCreate stepsReady $ \w -> do
      let path = wTmp w </> "Custom"
          confirmWith c = postJ (wApp w) "/api/create" (A.object ["template" A..= ("fake" :: Text), "path" A..= path, "confirm" A..= c])
      (code, v) <- createIn w path "fake" []
      code `shouldBe` 409
      key "template" v `shouldBe` A.String "fake"
      [(key "name" s, key "run" s) | s <- arr (key "steps" v)] `shouldBe` [(A.String "build", A.toJSON ["true", "Custom" :: Text])]
      txt (key "error" v) `shouldSatisfy` ("confirm" `T.isInfixOf`)
      rev <- revOf w "fake"
      key "revision" v `shouldBe` A.String rev
      -- a bare true, or another revision, is not a confirm of these steps
      fst <$> confirmWith (A.Bool True) `shouldReturn` 409
      (c3, v3) <- confirmWith (A.String "0000")
      (c3, key "revision" v3) `shouldBe` (409, A.String rev)
      txt (key "error" v3) `shouldSatisfy` ("changed since" `T.isInfixOf`)
      threadDelay 200000
      doesFileExist (wHome w </> "args.txt") `shouldReturn` False
      (code2, v2) <- confirmWith (A.String rev)
      code2 `shouldBe` 200
      argsOf w `shouldReturn` ["project", "create", path, "--template", "fake", "--trust-revision", T.unpack rev]
      void' (statusBecomes (wApp w) (txt (key "id" v2)) "ready")

    it "answers a fresh 409 when the template changed after its steps were shown" $ withCreate stepsReady $ \w -> do
      let path = wTmp w </> "Moved"
      (_, v) <- createIn w path "fake" []
      let shown = key "revision" v
      appendFile (wTemplate w </> "Main.lean") "-- changed\n"
      (code, v2) <- postJ (wApp w) "/api/create" (A.object ["template" A..= ("fake" :: Text), "path" A..= path, "confirm" A..= shown])
      code `shouldBe` 409
      key "revision" v2 `shouldNotBe` shown
      rev <- revOf w "fake"
      key "revision" v2 `shouldBe` A.String rev
      threadDelay 200000
      doesFileExist (wHome w </> "args.txt") `shouldReturn` False

    it "takes the folder through its deepest existing parent made canonical, and gives the child that path" $ withCreate stepsReady $ \w -> do
      createDirectoryIfMissing True (wTmp w </> "real")
      createSymbolicLink (wTmp w </> "real") (wTmp w </> "link")
      (code, v) <- createIn w (wTmp w </> "link" </> "Deep" </> "Proj") "lean" []
      code `shouldBe` 200
      args <- argsOf w
      take 3 args `shouldBe` ["project", "create", wTmp w </> "real" </> "Deep" </> "Proj"]
      d <- statusBecomes (wApp w) (TE.decodeUtf8 (idOf v)) "ready"
      key "root" d `shouldBe` A.String (T.pack (wTmp w </> "real" </> "Deep" </> "Proj"))
      -- the same folder by its link while that one runs would be busy; by now it is not empty
      (c2, e2) <- createIn w (wTmp w </> "real" </> "Deep" </> "Proj") "lean" []
      (c2, txt (key "error" e2)) `shouldSatisfy` (\(c, e) -> c == 400 && "not empty" `T.isInfixOf` e)

    it "refuses a cross-site page (403)" $ withCreate stepsReady $ \w -> do
      r <- call (wApp w) methodPost "/api/create" [("Origin", "http://evil.example"), (hContentType, "application/json")] (A.encode (A.object ["template" A..= ("lean" :: Text), "path" A..= (wTmp w </> "X")]))
      statusCode (simpleStatus r) `shouldBe` 403
      threadDelay 200000
      doesFileExist (wHome w </> "args.txt") `shouldReturn` False

  describe "POST /api/create/:id/stop" $
    it "stops the create (its process tree) and says stopped; a second stop is 409" $ withCreate "trap '' TERM; sleep 300 & wait" $ \w -> do
      (_, v) <- createIn w (wTmp w </> "Slow") "lean" []
      let cid = idOf v
      _ <- argsOf w
      (code, _) <- postJ (wApp w) ("/api/create/" <> cid <> "/stop") (A.object [])
      code `shouldBe` 202
      _ <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "stopped"
      fst <$> postJ (wApp w) ("/api/create/" <> cid <> "/stop") (A.object []) `shouldReturn` 409
      fst <$> postJ (wApp w) "/api/create/c.nope/stop" (A.object []) `shouldReturn` 404

  describe "POST /api/create/:id/retry" $
    it "after a failed step runs fisher project build PATH --yes, which resumes, and the stream from the offset it gives ends ready" $
      withCreate failThenBuild $ \w -> do
        let path = wTmp w </> "Again"
        (_, v) <- createIn w path "lean" []
        let cid = idOf v
        f <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "failed"
        txt (key "reason" f) `shouldSatisfy` ("Step build failed (exit 1)" `T.isInfixOf`)
        size <- getFileSize (path </> ".fisher" </> "create.jsonl")
        (code, r) <- postJ (wApp w) ("/api/create/" <> cid <> "/retry") (A.object [])
        code `shouldBe` 200
        (key "id" r, fmap toInteger (numOf (key "from" r))) `shouldBe` (A.String (TE.decodeUtf8 cid), Just size)
        _ <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "ready"
        argsOf w `shouldReturn` ["project", "build", path, "--yes"]
        (_, s) <- stream (wApp w) ("/api/create/" <> cid <> "/events?from=" <> BC.pack (show size))
        eventNames s `shouldBe` ["step start", "step end", "check", "ready"]
        fmap (key "status") (stEnd s) `shouldBe` Just (A.String "ready")
        -- ready: nothing to retry
        fst <$> postJ (wApp w) ("/api/create/" <> cid <> "/retry") (A.object []) `shouldReturn` 409

  describe "POST /api/create/:id/retry (template changed)" $
    it "answers 409 with the template's steps and revision as they are now, and the request to send again, running nothing" $
      withCreate changedSinceConfirmed $ \w -> do
        let path = wTmp w </> "Changed"
        rev <- revOf w "fake"
        (code, v) <- postJ (wApp w) "/api/create" (A.object ["template" A..= ("fake" :: Text), "path" A..= path, "vars" A..= A.object ["project" A..= ("Moved" :: Text)], "confirm" A..= rev])
        code `shouldBe` 200
        let cid = idOf v
        f <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "failed"
        txt (key "reason" f) `shouldSatisfy` ("changed since it was confirmed" `T.isInfixOf`)
        -- the template changes again; the retry shows it as it is now
        appendFile (wTemplate w </> "Main.lean") "-- changed\n"
        removeFile (wHome w </> "args.txt")
        (c2, r) <- postJ (wApp w) ("/api/create/" <> cid <> "/retry") (A.object [])
        c2 `shouldBe` 409
        rev2 <- revOf w "fake"
        key "revision" r `shouldBe` A.String rev2
        key "template" r `shouldBe` A.String "fake"
        txt (key "error" r) `shouldSatisfy` ("confirm" `T.isInfixOf`)
        [(key "name" st, key "run" st) | st <- arr (key "steps" r)] `shouldBe` [(A.String "build", A.toJSON ["true", "Moved" :: Text])]
        key "request" r `shouldBe` A.object ["template" A..= ("fake" :: Text), "path" A..= path, "vars" A..= A.object ["project" A..= ("Moved" :: Text)]]
        threadDelay 200000
        doesFileExist (wHome w </> "args.txt") `shouldReturn` False

  describe "POST /api/create/:id/stop (after ready)" $
    it "says ready when the events end with ready, even when a stop came after it" $ withCreate readyThenHang $ \w -> do
      let path = wTmp w </> "Late"
      (_, v) <- createIn w path "lean" []
      let cid = idOf v
      waitFile (wHome w </> "ready-written")
      (code, _) <- postJ (wApp w) ("/api/create/" <> cid <> "/stop") (A.object [])
      code `shouldBe` 202
      d <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "ready"
      (key "running" d, key "root" d) `shouldBe` (A.Bool False, A.String (T.pack path))

  describe "POST /api/create/:id/retry (busy)" $
    it "refuses a retry while another create of this server runs in the folder (409)" $ withCreate failFastOrHang $ \w -> do
      let path = wTmp w </> "Shared"
      (_, a) <- createIn w path "lean" []
      _ <- statusBecomes (wApp w) (TE.decodeUtf8 (idOf a)) "failed"
      -- the first left the folder empty; a second create there now runs
      writeFile (wHome w </> "hang") ""
      (cb, b) <- createIn w path "lean" []
      cb `shouldBe` 200
      _ <- statusBecomes (wApp w) (TE.decodeUtf8 (idOf b)) "running"
      (code, e) <- postJ (wApp w) ("/api/create/" <> idOf a <> "/retry") (A.object [])
      (code, txt (key "error" e)) `shouldBe` (409, "a create is already going in that folder")
      fst <$> postJ (wApp w) ("/api/create/" <> idOf b <> "/stop") (A.object []) `shouldReturn` 202

  describe "POST /api/create (scratch)" $
    it "runs fisher scratch --template NAME --statement TEXT --yes, follows the base's events and gives the scratch's root and file" $
      withCreate scratchRun $ \w -> do
        (code, v) <- postJ (wApp w) "/api/create" (A.object ["template" A..= ("lean" :: Text), "scratch" A..= True, "statement" A..= ("1 + 1 = 2" :: Text)])
        code `shouldBe` 200
        let cid = idOf v
        rev <- revOf w "lean"
        argsOf w `shouldReturn` ["scratch", "--template", "lean", "--statement", "1 + 1 = 2", "--trust-revision", T.unpack rev]
        (_, s) <- stream (wApp w) ("/api/create/" <> cid <> "/events")
        eventNames s `shouldBe` ["step start", "step end", "check", "ready"]
        let scr = wHome w </> "scr"
        (fmap (key "status") (stEnd s), fmap (key "root") (stEnd s), fmap (key "file") (stEnd s))
          `shouldBe` (Just (A.String "ready"), Just (A.String (T.pack scr)), Just (A.String "Scratch.lean"))
        d <- statusBecomes (wApp w) (TE.decodeUtf8 cid) "ready"
        (key "mode" d, key "events" d) `shouldBe` (A.String "scratch", A.String (T.pack (wHome w </> "base" </> ".fisher" </> "create.jsonl")))
        (c2, _) <- postJ (wApp w) "/api/create" (A.object ["template" A..= ("lean" :: Text), "scratch" A..= True, "path" A..= (wTmp w </> "X")])
        c2 `shouldBe` 400
  where
    void' :: IO a -> IO ()
    void' a = a >> pure ()
    numOf = \case
      A.Number n -> toBoundedInteger n :: Maybe Int
      _ -> Nothing
    -- a create that writes two steps, the check and ready, as the command line does
    stepsReady =
      T.unlines
        [ "root=\"$3\"; e=\"$root/.fisher/create.jsonl\"; mkdir -p \"$root/.fisher\""
        , "printf '%s\\n' '{\"event\":\"step start\",\"step\":\"cache\"}' >> \"$e\"; sleep 0.3"
        , "printf '%s\\n' '{\"event\":\"step end\",\"step\":\"cache\",\"exit\":0,\"tail\":[]}' >> \"$e\"; sleep 0.3"
        , "printf '%s\\n' '{\"event\":\"step start\",\"step\":\"build\"}' >> \"$e\"; sleep 0.3"
        , "printf '%s\\n' '{\"event\":\"step end\",\"step\":\"build\",\"exit\":0,\"tail\":[\"ok\"]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"check\",\"ok\":true,\"problems\":[]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"ready\"}' >> \"$e\""
        , "echo \"Ready: $root\"" ]
    -- create: a manifest, then a step that fails; build: the step again, the check, ready
    failThenBuild =
      T.unlines
        [ "root=\"$3\"; e=\"$root/.fisher/create.jsonl\"; mkdir -p \"$root/.fisher\""
        , "if [ \"$2\" = build ]; then"
        , "  printf '%s\\n' '{\"event\":\"step start\",\"step\":\"build\"}' >> \"$e\"; sleep 0.2"
        , "  printf '%s\\n' '{\"event\":\"step end\",\"step\":\"build\",\"exit\":0,\"tail\":[]}' >> \"$e\""
        , "  printf '%s\\n' '{\"event\":\"check\",\"ok\":true,\"problems\":[]}' >> \"$e\""
        , "  printf '%s\\n' '{\"event\":\"ready\"}' >> \"$e\"; exit 0"
        , "fi"
        , "echo 'kind: lean4' > \"$root/.fisher/project.yaml\""
        , "printf '%s\\n' '{\"event\":\"step start\",\"step\":\"build\"}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"step end\",\"step\":\"build\",\"exit\":1,\"tail\":[\"error: boom\"]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"failed\",\"reason\":\"Step build failed (exit 1)\"}' >> \"$e\""
        , "echo '(>_<) Step build failed (exit 1); see .fisher/create.log'"
        , "exit 1" ]
    -- the command line's stop when the template changed after the confirm: nothing written
    changedSinceConfirmed =
      T.unlines
        [ "echo '(>_<) Template `fake`: the template changed since it was confirmed (now at abc); nothing was written. Confirm its steps again.'"
        , "exit 1" ]
    -- writes ready, then hangs (TERM ignored) until it is killed
    readyThenHang =
      T.unlines
        [ "root=\"$3\"; e=\"$root/.fisher/create.jsonl\"; mkdir -p \"$root/.fisher\""
        , "printf '%s\\n' '{\"event\":\"step start\",\"step\":\"build\"}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"step end\",\"step\":\"build\",\"exit\":0,\"tail\":[]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"check\",\"ok\":true,\"problems\":[]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"ready\"}' >> \"$e\"; touch \"$FISHER_HOME/ready-written\""
        , "trap '' TERM; sleep 300 & wait" ]
    -- fails at once writing nothing, or (once FISHER_HOME/hang is there) hangs
    failFastOrHang =
      T.unlines
        [ "if [ -f \"$FISHER_HOME/hang\" ]; then trap '' TERM; sleep 300 & wait; fi"
        , "echo '(>_<) elan is not on PATH: install elan'; exit 1" ]
    -- fisher scratch: builds a base (its events), then prints the scratch's root and file
    scratchRun =
      T.unlines
        [ "base=\"$FISHER_HOME/base\"; e=\"$base/.fisher/create.jsonl\"; mkdir -p \"$base/.fisher\""
        , "echo \"Building the base for lean (once) in $base\""
        , "printf '%s\\n' '{\"event\":\"step start\",\"step\":\"build\"}' >> \"$e\"; sleep 0.3"
        , "printf '%s\\n' '{\"event\":\"step end\",\"step\":\"build\",\"exit\":0,\"tail\":[]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"check\",\"ok\":true,\"problems\":[]}' >> \"$e\""
        , "printf '%s\\n' '{\"event\":\"ready\"}' >> \"$e\""
        , "mkdir -p \"$FISHER_HOME/scr\"; echo 'theorem scratch_goal : 1 + 1 = 2 := by sorry' > \"$FISHER_HOME/scr/Scratch.lean\""
        , "echo \"Scratch: $FISHER_HOME/scr\""
        , "echo \"File: $FISHER_HOME/scr/Scratch.lean\"" ]
