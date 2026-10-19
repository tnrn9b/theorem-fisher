-- | The process environment as upstream's CLI sees it: the Fisher home's @.env@
-- laid over it at start-up by python-dotenv (@env_loader.py@, override on),
-- @get_env_value@ (environment first, then the file) and @save_env_value@
-- (the file, then the environment), with the Anthropic helpers built on it.
module Fisher.LLM.Auth.Env
  ( loadDotenvIntoSys
  , parseDotenv
  , getEnvValue
  , saveEnvValue
  , saveAnthropicOauthToken
  , saveAnthropicApiKey
  , useAnthropicClaudeCodeCredentials
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)

import Fisher.Env (loadEnvAt, saveEnvFileAt, validEnvName)
import Fisher.LLM.Auth.Error (pyError)
import Fisher.LLM.Sys
import Fisher.Paths (envPathIn)

-- | @load_gauss_dotenv()@ as @gauss_cli/main.py@ runs it at import: when
-- the Fisher home's @.env@ exists, every binding in it with a value is set in
-- the environment, over what was there. (The project @.env@ fallback is a
-- checkout-only path upstream; Fisher has no checkout.)
loadDotenvIntoSys :: Sys -> IO ()
loadDotenvIntoSys sys = do
  let path = envPathIn (sysFisherHome sys)
  exists <- doesFileExist path
  if not exists
    then pure ()
    else do
      bytes <- BS.readFile path
      let text = either (const (T.pack (map (toEnum . fromIntegral) (BS.unpack bytes)))) id (TE.decodeUtf8' bytes)
          bindings = parseDotenv text
      resolved <- resolveVariables sys bindings
      forM_ resolved $ \(k, mv) -> forM_ mv (sysSetEnv sys k)

-- | python-dotenv's parser: the bindings in file order, a key without
-- @=@ giving no value. Lines that do not parse are skipped.
parseDotenv :: Text -> [(Text, Maybe Text)]
parseDotenv = go . dropBom
  where
    dropBom t = maybe t id (T.stripPrefix "\xFEFF" t)
    go s
      | T.null s = []
      | otherwise = case binding s of
          Just (b, rest) -> maybe id (:) b (go rest)
          Nothing -> go (restOfLine s)
    hspace c = isSpace c && c /= '\n' && c /= '\r'
    restOfLine s =
      let (_, r) = T.break (\c -> c == '\n' || c == '\r') s
       in dropNewline r
    dropNewline r
      | "\r\n" `T.isPrefixOf` r = T.drop 2 r
      | "\n" `T.isPrefixOf` r || "\r" `T.isPrefixOf` r = T.drop 1 r
      | otherwise = r
    binding s0 = do
      let s1 = T.dropWhile isSpace s0
      if T.null s1
        then Just (Nothing, "")
        else do
          let s2 = case T.stripPrefix "export" s1 of
                Just r | Just (c, _) <- T.uncons r, hspace c -> T.dropWhile hspace r
                _ -> s1
          (key, s3) <- parseKey s2
          let s4 = T.dropWhile hspace s3
          (value, s5) <- case T.uncons s4 of
            Just ('=', r) -> do
              let r1 = T.dropWhile hspace r
              (v, r2) <- parseValue r1
              pure (Just v, r2)
            _ -> pure (Nothing, s4)
          s6 <- comment s5
          s7 <- endOfLine s6
          pure (fmap (\k -> (k, value)) key, s7)
    parseKey s = case T.uncons s of
      Just ('#', _) -> Just (Nothing, s)
      Just ('\'', r) ->
        let (k, rest) = T.break (== '\'') r
         in if T.null k || T.null rest then Nothing else Just (Just k, T.drop 1 rest)
      _ ->
        let (k, rest) = T.span (\c -> c /= '=' && c /= '#' && not (isSpace c)) s
         in if T.null k then Nothing else Just (Just k, rest)
    parseValue s = case T.uncons s of
      Just ('\'', r) -> do
        (v, rest) <- quoted '\'' r
        pure (unescape "\\'" v, rest)
      Just ('"', r) -> do
        (v, rest) <- quoted '"' r
        pure (unescape "\\'\"abfnrtv" v, rest)
      Nothing -> Just ("", s)
      Just (c, _)
        | c == '\n' || c == '\r' -> Just ("", s)
        | otherwise ->
            let (part, rest) = T.break (\x -> x == '\n' || x == '\r') s
             in Just (T.stripEnd (dropComment part), rest)
    -- @re.sub(r"\s+#.*", "", part)@
    dropComment part = case T.breakOn "#" part of
      (_, "") -> part
      (before, after)
        | not (T.null before) && isSpace (T.last before) -> T.dropWhileEnd isSpace before
        | otherwise -> before <> "#" <> dropComment (T.drop 1 after)
    quoted q s = scan s ""
      where
        scan t acc = case T.uncons t of
          Nothing -> Nothing
          Just ('\\', r) -> case T.uncons r of
            Just (c, r') -> scan r' (acc <> T.pack ['\\', c])
            Nothing -> Nothing
          Just (c, r)
            | c == q -> Just (acc, r)
            | otherwise -> scan r (acc <> T.singleton c)
    unescape allowed t = case T.breakOn "\\" t of
      (a, "") -> a
      (a, b) -> case T.uncons (T.drop 1 b) of
        Just (c, r) | c `elem` (allowed :: String) -> a <> T.singleton (decode c) <> unescape allowed r
        _ -> a <> "\\" <> unescape allowed (T.drop 1 b)
    decode = \case
      'a' -> '\a'
      'b' -> '\b'
      'f' -> '\f'
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      'v' -> '\v'
      c -> c
    comment s =
      let s' = T.dropWhile hspace s
       in Just $ case T.uncons s' of
            Just ('#', _) -> T.dropWhile (\c -> c /= '\n' && c /= '\r') s'
            _ -> s
    endOfLine s =
      let s' = T.dropWhile hspace s
       in if T.null s' || "\n" `T.isPrefixOf` s' || "\r" `T.isPrefixOf` s' then Just (dropNewline s') else Nothing

-- | @resolve_variables(values, override=True)@: @${NAME}@ and
-- @${NAME:-default}@ expand against the values read so far, then the
-- environment.
resolveVariables :: Sys -> [(Text, Maybe Text)] -> IO [(Text, Maybe Text)]
resolveVariables sys = go Map.empty
  where
    go _ [] = pure []
    go seen ((k, mv) : rest) = do
      r <- traverse (expand seen) mv
      ((k, r) :) <$> go (Map.insert k r seen) rest
    expand seen v = case T.breakOn "${" v of
      (a, "") -> pure a
      (a, b) ->
        let inner = T.drop 2 b
         in case T.breakOn "}" inner of
              (_, "") -> pure v
              (spec, after) -> do
                let (name, dflt) = case T.breakOn ":-" spec of
                      (n, "") -> (n, Nothing)
                      (n, d) -> (n, Just (T.drop 2 d))
                if T.any (== ':') name && dflt == Nothing
                  then (\r -> a <> "${" <> r) <$> expand seen inner
                  else do
                    val <- case Map.lookup name seen of
                      Just x -> pure (maybe "" id x)
                      Nothing -> maybe (maybe "" id dflt) id <$> sysGetEnv sys name
                    ((a <> val) <>) <$> expand seen (T.drop 1 after)

-- | @get_env_value(key)@: the environment (an empty value counts), then
-- the home's @.env@ as @load_env@ reads it.
getEnvValue :: Sys -> Text -> IO (Maybe Text)
getEnvValue sys k =
  sysGetEnv sys k >>= \case
    Just v -> pure (Just v)
    Nothing -> Map.lookup k <$> loadEnvAt (sysFisherHome sys)

-- | @save_env_value(key, value)@: the file (see 'saveEnvValueAt'), then
-- @os.environ[key] = value@.
saveEnvValue :: Sys -> Text -> Text -> IO ()
saveEnvValue sys k v = do
  if validEnvName k then pure () else throwIO (pyError "ValueError" ("Invalid environment variable name: '" <> k <> "'"))
  saveEnvFileAt (sysFisherHome sys) k v
  sysSetEnv sys k (T.filter (\c -> c /= '\n' && c /= '\r') v)

saveAnthropicOauthToken :: Sys -> Text -> IO ()
saveAnthropicOauthToken sys v = saveEnvValue sys "ANTHROPIC_TOKEN" v >> saveEnvValue sys "ANTHROPIC_API_KEY" ""

saveAnthropicApiKey :: Sys -> Text -> IO ()
saveAnthropicApiKey sys v = saveEnvValue sys "ANTHROPIC_API_KEY" v >> saveEnvValue sys "ANTHROPIC_TOKEN" ""

useAnthropicClaudeCodeCredentials :: Sys -> IO ()
useAnthropicClaudeCodeCredentials sys = saveEnvValue sys "ANTHROPIC_TOKEN" "" >> saveEnvValue sys "ANTHROPIC_API_KEY" ""
