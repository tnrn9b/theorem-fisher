-- | The setups @tools/corpus/a2.py@ recorded upstream in, rebuilt here, and
-- the same snapshot and normalisation it applied to what came out.
module Fisher.A2Fixture
  ( fakeTool
  , Extras
  , setupCase
  , snapshot
  , changes
  , normalize
  , normText
  , machineFor
  , arxivPath
  , withProcessEnv
  ) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime (..), fromGregorian)
import System.Directory hiding (isSymbolicLink)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink, setFileMode)

import Fisher.Golden (field)
import Fisher.Project (defaultInitOptions, initName, initializeProject)
import Fisher.Workflow.Types

type Extras = Value

-- | The fake external tool, byte for byte the one the harness writes.
fakeTool :: Text
fakeTool = T.unlines
  [ "#!/bin/bash"
  , "export PATH=\"/usr/bin:/bin:$PATH\""
  , "printf '%s' \"$(basename \"$0\")\" >> \"$FAKE_LOG\""
  , "for a in \"$@\"; do printf ' [%s]' \"$a\" >> \"$FAKE_LOG\"; done"
  , "printf '\\n' >> \"$FAKE_LOG\""
  , "name=\"$(basename \"$0\")\""
  , "if [ \"$name\" = git ] && [ -n \"$FAKE_GIT_FAIL\" ]; then echo \"boom\" >&2; exit 1; fi"
  , "if [ \"$name\" = git ] && [ \"$1\" = clone ]; then"
  , "  for last in \"$@\"; do :; done"
  , "  p=\"$last/plugins/lean4\""
  , "  mkdir -p \"$p/skills/lean4/references\" \"$p/lib/scripts\" \"$p/commands\" \"$p/.claude-plugin\" \"$last/.claude-plugin\" \"$last/.git\""
  , "  printf '# lean4 skill\\n' > \"$p/skills/lean4/SKILL.md\""
  , "  printf '{\"name\": \"lean4\", \"version\": \"2.0.0\"}' > \"$p/.claude-plugin/plugin.json\""
  , "  printf '{\"name\": \"lean4-skills\"}' > \"$last/.claude-plugin/marketplace.json\""
  , "  exit 0"
  , "fi"
  , "if [ \"$name\" = git ] && [ \"$3\" = rev-parse ]; then echo def456; exit 0; fi"
  , "if [ \"$name\" = claude ]; then"
  , "  if [ \"$1 $2\" = \"plugin list\" ]; then"
  , "    printf '[{\"id\": \"lean4@lean4-skills\", \"installPath\": \"%s\"}]\\n' \"$FAKE_PLUGIN_DIR\"; exit 0"
  , "  fi"
  , "  if [ \"$1 $2\" = \"plugin install\" ]; then"
  , "    mkdir -p \"$HOME/.claude/plugins\""
  , "    printf '{\"version\": 2, \"plugins\": {\"lean4@lean4-skills\": [{\"installPath\": \"%s\", \"version\": \"9.9.9\"}]}}' \"$FAKE_PLUGIN_DIR\" > \"$HOME/.claude/plugins/installed_plugins.json\""
  , "    exit 0"
  , "  fi"
  , "  if [ \"$1 $2 $3\" = \"plugin marketplace add\" ] && [ -n \"$FAKE_CLAUDE_ALREADY\" ]; then"
  , "    echo \"Marketplace is already installed\" >&2; exit 1"
  , "  fi"
  , "fi"
  , "exit 0" ]

arxivPath :: FilePath
arxivPath = "/fisher-test/arxiv/search_arxiv.py"

machineFor :: Value -> FilePath -> Machine
machineFor golden fisherHome =
  let mach = field "machine" golden :: Value
   in Machine
        { mIsRoot = field "is_root" mach
        , mStrictUnavailable = pure (field "strict_unavailable" mach)
        , mKeychain = pure Nothing
        , mNow = pure (UTCTime (fromGregorian 2026 10 1) 0)
        , mStamp = pure "20261001-120000"
        , mFisherHome = fisherHome
        , mArxivScript = Just arxivPath
        , mNote = \_ -> pure ()
        }

writeJson :: FilePath -> Value -> IO ()
writeJson p v = createDirectoryIfMissing True (takeDirectory p) >> BL.writeFile p (encode v)

makeBins :: FilePath -> [Text] -> IO FilePath
makeBins root names = do
  let b = root </> "bin"
  createDirectoryIfMissing True b
  forM_ names $ \n -> do
    TIO.writeFile (b </> T.unpack n) fakeTool
    setFileMode (b </> T.unpack n) 0o755
  pure b

makeCheckout :: FilePath -> Bool -> IO ()
makeCheckout assets complete = do
  let co = assets </> "lean4-skills"
      plugin = co </> "plugins" </> "lean4"
  createDirectoryIfMissing True (plugin </> "skills" </> "lean4" </> "references")
  writeFile (plugin </> "skills" </> "lean4" </> "SKILL.md") "# lean4 skill\n"
  writeFile (plugin </> "skills" </> "lean4" </> "references" </> "tactics.md") "tactics\n"
  when complete $ do
    createDirectoryIfMissing True (plugin </> "lib" </> "scripts")
    writeFile (plugin </> "lib" </> "scripts" </> "check.sh") "echo ok\n"
  createDirectoryIfMissing True (plugin </> "commands")
  writeFile (plugin </> "commands" </> "prove.md") "# prove\n"
  writeFile (plugin </> "commands" </> "autoformalize.md") "# autoformalize\n"
  writeJson (plugin </> ".claude-plugin" </> "plugin.json") (object ["name" .= ("lean4" :: Text), "version" .= ("1.2.3" :: Text)])
  writeJson (co </> ".claude-plugin" </> "marketplace.json") (object ["name" .= ("lean4-skills" :: Text)])
  writeFile (co </> ".fisher-managed-revision") "abc123\n"

makePrewarmed :: FilePath -> IO ()
makePrewarmed home = do
  let cache = home </> ".claude" </> "plugins" </> "cache" </> "lean4-skills" </> "lean4" </> "1.2.3"
      market = home </> ".claude" </> "plugins" </> "marketplaces" </> "lean4-skills"
  createDirectoryIfMissing True (cache </> "skills" </> "lean4")
  writeFile (cache </> "skills" </> "lean4" </> "SKILL.md") "# prewarmed\n"
  createDirectoryIfMissing True market
  writeJson (home </> ".claude" </> "settings.json")
    (object ["theme" .= ("light" :: Text), "enabledPlugins" .= object ["other@x" .= True]])
  let stamp = "2026-01-01T00:00:00.000Z" :: Text
  writeJson (home </> ".claude" </> "plugins" </> "installed_plugins.json") $
    object [ "version" .= (2 :: Int)
           , "plugins" .= object ["lean4@lean4-skills" .= [object
               [ "scope" .= ("user" :: Text), "installPath" .= cache, "version" .= ("1.2.3" :: Text)
               , "installedAt" .= stamp, "lastUpdated" .= stamp ]]] ]
  writeJson (home </> ".claude" </> "plugins" </> "known_marketplaces.json") $
    object ["lean4-skills" .= object
      [ "source" .= object ["source" .= ("github" :: Text), "repo" .= ("cameronfreer/lean4-skills" :: Text)]
      , "installLocation" .= market, "lastUpdated" .= stamp ]]

writeHomeFiles :: FilePath -> Value -> IO ()
writeHomeFiles home = \case
  Object files -> forM_ (KM.toList files) $ \(rel, payload) -> do
    let p = home </> K.toString rel
    createDirectoryIfMissing True (takeDirectory p)
    case payload of
      String s -> TIO.writeFile p s
      v -> BL.writeFile p (encode v)
  _ -> pure ()

opt :: Text -> Value -> Maybe Value
opt k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

-- | Rebuild a case's setup under @root@. Returns the base environment
-- (this process's plus the case's) and the managed state directory.
setupCase :: FilePath -> Extras -> IO Env
setupCase root extras = do
  let bins = maybe ["git", "uvx", "rg", "claude"] (\v -> field "bins" (object ["bins" .= v])) (opt "bins" extras)
      home = root </> "home"
      gh = root </> "fisher-home"
  binDir <- makeBins root bins
  createDirectoryIfMissing True home
  writeHomeFiles home (fromMaybe (Object KM.empty) (opt "home" extras))
  when (opt "prewarmed" extras == Just (Bool True)) (makePrewarmed home)
  createDirectoryIfMissing True gh
  let state = case opt "state_dir" extras of
        Just (String d) -> root </> T.unpack d
        _ -> gh </> "autoformalize"
      checkout = case opt "checkout" extras of
        Just (String c) -> c
        _ -> "ok"
  when (checkout /= "none") $ makeCheckout (state </> "assets") (checkout == "ok")
  when (opt "legacy_root" extras == Just (Bool True)) $
    createDirectoryIfMissing True (gh </> "autoformalize" </> "managed")
  let proj = root </> "project"
  createDirectoryIfMissing True (proj </> "Math")
  writeFile (proj </> "lakefile.lean") "-- lean\n"
  _ <- initializeProject proj defaultInitOptions { initName = Just "Demo Project" }
  forM_ (opt "project_settings" extras) $ \s -> case s of
    Null -> pure ()
    v -> writeJson (proj </> ".claude" </> "settings.local.json") v
  createDirectoryIfMissing True (root </> "broken" </> ".fisher")
  writeFile (root </> "broken" </> "lakefile.lean") ""
  writeFile (root </> "broken" </> ".fisher" </> "project.yaml") "name: x\n"
  createDirectoryIfMissing True (root </> "outside")
  createDirectoryIfMissing True (root </> "fake-plugin" </> "skills" </> "lean4")
  process <- Map.fromList <$> getEnvironment
  let extraEnv = case opt "env" extras of
        Just (Object o) -> [(K.toString k, T.unpack (T.replace "<HOME>" (T.pack home) v)) | (k, String v) <- KM.toList o]
        _ -> []
      base = foldl (\m (k, v) -> Map.insert k v m) process $
        [ ("PATH", binDir), ("HOME", home), ("FAKE_LOG", root </> "fake.log")
        , ("FAKE_PLUGIN_DIR", root </> "fake-plugin"), ("FISHER_HOME", gh)
          -- the recordings are upstream's, whose MCP server is uvx lean-lsp-mcp
        , ("FISHER_LEAN_MCP", "uvx") ] <> extraEnv
  pure base

-- | Run with the fakes' settings in this process's environment too, since
-- git and the uv prewarm inherit it, then put the environment back.
withProcessEnv :: Env -> IO a -> IO a
withProcessEnv base act = bracket set restore (const act)
  where
    keys = ["FAKE_LOG", "FAKE_PLUGIN_DIR", "FAKE_GIT_FAIL", "FAKE_CLAUDE_ALREADY", "FISHER_HOME", "FISHER_LEAN_MCP"]
    set = forM keys $ \k -> do
      old <- lookupEnv k
      maybe (unsetEnv k) (setEnv k) (Map.lookup k base)
      pure (k, old)
    restore = mapM_ (\(k, old) -> maybe (unsetEnv k) (setEnv k) old)

-- | Every file and link under @root@, as the harness's @snapshot()@ sees it.
snapshot :: FilePath -> IO (Map.Map Text Value)
snapshot root = Map.fromList . concat <$> walk root
  where
    walk dir = do
      names <- sort <$> listDirectory dir
      fmap concat . forM names $ \n -> do
        let p = dir </> n
            rel = makeRelative root p
        st <- getSymbolicLinkStatus p
        if "bin" `isPrefixOf` rel || rel == "fake.log"
          then pure []
          else
            if isSymbolicLink st
              then do
                target <- readSymbolicLink p
                pure [[(T.pack rel, object ["symlink" .= target])]]
              else do
                isDir <- doesDirectoryExist p
                if isDir
                  then walk p
                  else do
                    bytes <- BS.readFile p
                    pure . pure . pure . (T.pack rel,) $ case TE.decodeUtf8' bytes of
                      Left _ -> object ["bytes" .= BS.length bytes]
                      Right text
                        | takeExtension p == ".json", Just v <- decodeStrict bytes -> object ["json" .= (v :: Value)]
                        | otherwise -> object ["text" .= text]

changes :: Map.Map Text Value -> Map.Map Text Value -> Value
changes before after =
  Object . KM.fromList $
    [(K.fromText k, v) | (k, v) <- Map.toList after, Map.lookup k before /= Just v]

-- | The harness's @norm@: scratch root, arXiv helper, startup stamps and ISO
-- timestamps, everywhere, keys included.
normText :: FilePath -> Text -> Text
normText root =
  isoSub . stampSub . T.replace (T.pack root) "<TMP>" . T.replace (T.pack arxivPath) "<ARXIV>"

normalize :: FilePath -> Value -> Value
normalize root = \case
  String s -> String (normText root s)
  Array a -> Array (fmap (normalize root) a)
  Object o -> Object (KM.fromList [(K.fromText (normText root (K.toText k)), normalize root v) | (k, v) <- KM.toList o])
  v -> v

-- | @\d{8}-\d{6}@ to @<STAMP>@.
stampSub :: Text -> Text
stampSub = go
  where
    go t = case T.uncons t of
      Nothing -> ""
      Just (c, rest)
        | isStamp t -> "<STAMP>" <> go (T.drop 15 t)
        | otherwise -> T.cons c (go rest)
    isStamp t =
      T.length t >= 15 && T.all isDigit (T.take 8 t) && T.index t 8 == '-' && T.all isDigit (T.take 6 (T.drop 9 t))

-- | @\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z@ to @<ISO>@.
isoSub :: Text -> Text
isoSub = go
  where
    go t = case T.uncons t of
      Nothing -> ""
      Just (c, rest) -> case isoAt t of
        Just n -> "<ISO>" <> go (T.drop n t)
        Nothing -> T.cons c (go rest)
    isoAt t =
      let shape = "dddd-dd-ddTdd:dd:dd" :: String
          ok = T.length t >= 19 && and (zipWith matches shape (T.unpack (T.take 19 t)))
          matches 'd' ch = isDigit ch
          matches s ch = s == ch
          frac = case T.uncons (T.drop 19 t) of
            Just ('.', r) | n <- T.length (T.takeWhile isDigit r), n > 0 -> 1 + n
            _ -> 0
       in if ok && T.take 1 (T.drop (19 + frac) t) == "Z" then Just (20 + frac) else Nothing
