-- | @fisher sessions@, @fisher config show@ and @fisher config edit@
-- against upstream's own runs (@subcommands.json@): the same fixture
-- database, clock (1790000000, UTC), config and .env, and a fake editor.
module Fisher.SubcommandsSpec (spec) where

import Control.Monad (forM, forM_, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (utc)
import qualified Data.Yaml as Y
import qualified Database.SQLite.Simple as S
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, info)
import System.Directory (doesFileExist, executable, getPermissions, listDirectory)
import System.Posix.Files (setFileMode)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.Process (CreateProcess (..), createProcess, proc, waitForProcess)
import Test.Hspec hiding (after, before)

import Fisher.A3Golden
import Fisher.A3World
import Fisher.Cli.ConfigShow
import Fisher.Cli.Sessions
import Fisher.Config (defaultConfig, loadConfigAt, saveConfigAt)
import Fisher.Env (loadEnvAt)
import Fisher.Paths (configPathIn, envPathIn)
import Fisher.Store
import Fisher.Term (plainStyle)

now :: Double
now = 1790000000

-- | The harness's fixture, written through Fisher's own store.
fixture :: FilePath -> Bool -> IO ()
fixture path titled = withStore path $ \st ->
  forM_ rows $ \(sid, src, started, title, msgs, ended) -> do
    createSession st started sid src (Just "anthropic/claude-opus-4.6")
    forM_ title $ \t -> when titled (() <$ setSessionTitle st sid t)
    forM_ msgs $ \(role, content, ts) -> appendMessage st ts sid role (Just content)
    forM_ ended $ \e -> endSession st e sid "user_exit"
  where
    d = 86400
    rows :: [(Text, Text, Double, Maybe Text, [(Text, Text, Double)], Maybe Double)]
    rows =
      [ ("20260930_100000_aaaaaa", "cli", now - 5 * 3600, Just "Lean prover",
          [("user", "Prove the main theorem\nby induction", now - 2 * 3600), ("assistant", "Done.", now - 2 * 3600 + 5)], Nothing)
      , ("20260929_090000_bbbbbb", "telegram", now - 1.5 * d, Nothing, [("user", T.replicate 70 "x", now - 1.5 * d + 60)], Nothing)
      , ("20260101_080000_cccccc", "cli", now - 120 * d, Nothing, [], Just (now - 119 * d))
      , ("20250101_080000_dddddd", "discord", now - 400 * d, Just "Old",
          [("user", "  old question  ", now - 400 * d + 10), ("tool", "out", now - 400 * d + 20)], Just (now - 399 * d))
      , ("20261001_115900_eeeeee", "cli", now - 30, Nothing, [("user", "hi", now - 10)], Nothing)
      , ("20260601_070000_ffffff", "slack", now - 30 * d, Nothing, [("user", "\n\ttabbed\r\nline", now - 30 * d)], Just (now - 29 * d)) ]

-- | The harness's @dump@: the rows the commands change, as JSON.
dump :: FilePath -> IO (Maybe Value)
dump path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      c <- S.open path
      ss <- S.query_ c "SELECT id, source, title, started_at, ended_at, message_count FROM sessions ORDER BY id" :: IO [[S.SQLData]]
      ms <- S.query_ c "SELECT session_id, role, content, timestamp FROM messages ORDER BY id" :: IO [[S.SQLData]]
      S.close c
      let row ks vs = Object (KM.fromList (zip (map K.fromText ks) (map val vs)))
      pure . Just . Object $ KM.fromList
        [ ("sessions", Array (foldMap (pure . row ["id", "source", "title", "started_at", "ended_at", "message_count"]) ss))
        , ("messages", Array (foldMap (pure . row ["session_id", "role", "content", "timestamp"]) ms)) ]
  where
    val = \case
      S.SQLInteger n -> Number (fromIntegral n)
      S.SQLFloat f -> Number (fromFloatDigits f)
      S.SQLText t -> String t
      S.SQLNull -> Null
      S.SQLBlob _ -> Null

-- | The harness's @clean@: no carriage returns, no trailing blanks.
clean :: Text -> Text
clean = T.intercalate "\n" . map T.stripEnd . T.splitOn "\n" . T.replace "\r" ""

writeConfig :: FilePath -> Value -> IO ()
writeConfig gh = \case
  Null -> pure ()
  Object o | KM.null o -> TIO.writeFile (configPathIn gh) "{}\n"
  v -> Y.encodeFile (configPathIn gh) v

caseEnv :: Value -> Map.Map Text Text
caseEnv = \case
  Object o -> Map.fromList [(K.toText k, s) | (k, String s) <- KM.toList o]
  _ -> Map.empty

withCase :: (World -> IO a) -> IO a
withCase act =
  withTempDirectory "/tmp" "fa3s-" $ \tmp -> withTempDirectory "/var/tmp" "fa3t-" $ \tmpg -> do
    w <- makeWorld tmp tmpg
    act w

maskAll :: World -> Text -> Text
maskAll w = mask (T.pack (wRoot w)) "<TMP>" . mask (T.pack (wFisherHome w)) "<GAUSS_HOME>"

spec :: Spec
spec = do
  cases <- runIO (a3Cases "subcommands.json")
  let kind k = filter ((== k) . str "kind") cases

  describe "fisher sessions" $
    forM_ (kind "sessions") $ \c -> it (T.unpack (str "name" c)) $ withCase $ \w -> do
      let gh = wFisherHome w
          db = storePath gh
          argv = [T.unpack (T.replace "<TMP>" (T.pack (wRoot w)) (str' a)) | a <- drop 1 (arr "argv" c)]
          str' = \case String s -> s; _ -> ""
      TIO.writeFile (configPathIn gh) "{}\n"
      case obj "fixture" c of
        Bool t -> fixture db t
        _ -> pure ()
      before <- dump db
      cmd <- case execParserPure defaultPrefs (info sessionsParser mempty) argv of
        Success x -> pure x
        other -> fail ("argv did not parse: " <> show argv <> " " <> show (() <$ other))
      out <- newIORef ([] :: [Text])
      inputs <- newIORef [s | String s <- arr "inputs" c]
      let put t = modifyIORef out ((t <> "\n") :)
          ask p = do
            modifyIORef out (p :)
            readIORef inputs >>= \case
              [] -> pure Nothing
              (x : xs) -> writeIORef inputs xs >> modifyIORef out ((x <> "\n") :) >> pure (Just x)
      runSessions (SessionsEnv db now utc put ask) cmd
      text <- T.concat . reverse <$> readIORef out
      maskAll w (clean text) `shouldBe` str "output" c
      after <- dump db
      (if after /= before then after else Nothing) `shouldBe` (case obj "db_after" c of Null -> Nothing; v -> Just v)
      files <- sort . filter (".jsonl" `isSuffixOf`) <$> listDirectory (wRoot w)
      exports <- forM files $ \f -> (,) (T.pack f) . maskAll w <$> TIO.readFile (wRoot w </> f)
      let expected = case obj "exports" c of
            Object o -> [(K.toText k, s) | (k, String s) <- KM.toList o]
            _ -> []
      sort exports `shouldBe` sort expected

  describe "fisher config show" $
    forM_ (kind "config-show") $ \c -> it (T.unpack (str "name" c)) $ withCase $ \w -> do
      let gh = wFisherHome w
          procEnv = caseEnv (obj "env" c)
          fileEnv = caseEnv (obj "env_file" c)
      writeConfig gh (obj "config" c)
      when (not (Map.null fileEnv)) $
        TIO.writeFile (envPathIn gh) (T.concat [k <> "=" <> v <> "\n" | (k, v) <- Map.toList fileEnv])
      (cfg, _) <- loadConfigAt gh
      let env k = case Map.lookup k procEnv of
            Just v -> pure (Just v)
            Nothing -> Map.lookup k <$> loadEnvAt gh
      ls <- showConfigLines (ShowEnv plainStyle cfg (configPathIn gh) (envPathIn gh) "<SRC>" env)
      let text = T.concat [l <> "\n" | l <- ls]
      maskAll w (clean text) `shouldBe` str "output" c

  describe "fisher config edit" $
    forM_ (kind "config-edit") $ \c -> it (T.unpack (str "name" c)) $ withCase $ \w -> do
      let gh = wFisherHome w
          b = wBin w
          path = configPathIn gh
          logf = wRoot w </> "editor.log"
          env = Map.map (T.replace "<BIN>" (T.pack b)) (caseEnv (obj "env" c))
          script = "#!/bin/bash\nexport PATH=\"/usr/bin:/bin:$PATH\"\necho \"$(basename \"$0\") $*\" >> \"$FAKE_EDITOR_LOG\"\n"
          install n = TIO.writeFile (b </> n) script >> setExecutable (b </> n)
      writeConfig gh (obj "config" c)
      case obj "editor" c of
        Bool True -> install "fake-editor"
        String n -> install (T.unpack n)
        _ -> pure ()
      out <- newIORef ([] :: [Text])
      editConfig
        EditEnv
          { edConfigPath = path
          , edConfigExists = doesFileExist path
          , edWriteDefault = saveConfigAt gh defaultConfig
          , edEnv = \k -> pure (Map.lookup k env)
          , edWhich = \n -> do
              let p = b </> T.unpack n
              ok <- doesFileExist p
              if ok then (\pm -> if executable pm then Just p else Nothing) <$> getPermissions p else pure Nothing
          , edRun = \e args -> do
              let exe = if "/" `T.isInfixOf` e then T.unpack e else b </> T.unpack e
              (_, _, _, ph) <- createProcess (proc exe (map T.unpack args)) {env = Just [("PATH", "/usr/bin:/bin"), ("FAKE_EDITOR_LOG", logf)]}
              ExitSuccess <- waitForProcess ph
              pure ()
          , edPut = \t -> modifyIORef out ((t <> "\n") :)
          }
      text <- T.concat . reverse <$> readIORef out
      maskAll w (clean text) `shouldBe` str "output" c
      hasLog <- doesFileExist logf
      calls <- if hasLog then T.lines <$> TIO.readFile logf else pure []
      map (maskAll w) calls `shouldBe` [s | String s <- arr "editor_calls" c]
      hasCfg <- doesFileExist path
      after <- if hasCfg then Just . maskAll w <$> TIO.readFile path else pure Nothing
      after `shouldBe` mstr "config_after" c
  where
    setExecutable p = setFileMode p 0o755
