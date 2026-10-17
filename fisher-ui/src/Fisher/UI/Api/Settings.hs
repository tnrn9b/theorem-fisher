-- | @GET@ and @POST /api/settings@: the settings a run started from the
-- page uses, read and written as @fisher config show@ and @fisher config
-- set@ do.
--
--   * @provider@ is @model.provider@ (default @auto@), @model@ is
--     @model.default@ (or @model@ when it is a plain string),
--     @autoformalizeModel@ is @gauss.autoformalize.model@ (empty: each
--     backend's own default) and @contextStrategy@ is @fisher.context@
--     (@compress@, @clm@ or @rlm@). They are written to config.yaml through
--     'setConfigValueAt' (the user's own file, its key order kept), and only
--     the ones that changed.
--   * @keys@ are the credentials in @.env@: each one's name and its last
--     four characters only (none for a value under 8 characters). @plain@
--     are the other variables 'setTarget' sends to @.env@ that hold no
--     secret (a host, a user, a URL, a project id: no @_KEY@, @_TOKEN@ at
--     the end; no @PASS@, @PASSWD@, @PASSPHRASE@, @PASSWORD@, @SECRET@ or
--     @CREDENTIAL@ in the name), each with its whole value, the
--     known ones listed set or not. Both are written through @keys@. A new
--     value is written with 'saveEnvFileAt': the line replaced in place
--     (or appended), every other line kept in its order, a temporary file
--     renamed over it, mode 0600. A value never leaves the server, in a
--     response or on its output.
--   * @auto@ is the search pre-pass: Fisher has no config keys for it, so
--     it is shown as the flag defaults of @fisher search@ (@settable@
--     false) and a POST ignores it.
module Fisher.UI.Api.Settings
  ( settingsApi
  , settingsView
  , mainKeys
  , lastFour
  , isSecret
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either (partitionEithers)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Network.HTTP.Types (badRequest400, conflict409, methodGet, methodNotAllowed405, methodPost, ok200, internalServerError500)
import Network.Wai (Request (..), Response)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

import Fisher.Config (getPath, loadConfigAt)
import Fisher.Config.Set (Coerced (..), SetTarget (..), coerceValue, setConfigValueAt, setTarget)
import Fisher.Env (loadEnvAt, saveEnvFileAt, validEnvName)
import Fisher.Paths (configPathIn)
import Fisher.Search.Auto (AutoConfig (..), defaultAutoConfig)
import Fisher.UI.Api.Live (bad, jsonBodyWith)
import Fisher.UI.Response (Api, jsonResponse)

-- | The provider keys always listed, set or not.
mainKeys :: [Text]
mainKeys = ["OPENROUTER_API_KEY", "OPENAI_API_KEY", "ANTHROPIC_API_KEY"]

-- | The last four characters of a value of at least 8 (a shorter one would
-- give away too much of itself).
lastFour :: Text -> Maybe Text
lastFour v
  | T.length v >= 8 = Just (T.takeEnd 4 v)
  | otherwise = Nothing

-- | The variables that are not secrets listed even when unset.
plainKnown :: [Text]
plainKnown = ["TERMINAL_SSH_HOST", "TERMINAL_SSH_USER", "FIRECRAWL_API_URL", "BROWSERBASE_PROJECT_ID"]

-- | Whether a .env name holds a secret (its value is shown by its last
-- four characters only).
isSecret :: Text -> Bool
isSecret n =
  any (`T.isSuffixOf` u) ["_KEY", "_TOKEN"]
    || any (`T.isInfixOf` u) ["PASSWORD", "PASSWD", "PASSPHRASE", "PASS", "SECRET", "CREDENTIAL"]
  where
    u = T.toUpper n

strategies :: [Text]
strategies = ["compress", "clm", "rlm"]

-- | One settings write at a time.
writeLock :: MVar ()
writeLock = unsafePerformIO (newMVar ())
{-# NOINLINE writeLock #-}

settingsApi :: FilePath -> Api
settingsApi home r = case pathInfo r of
  ["api", "settings"]
    | requestMethod r == methodGet -> Just . jsonResponse ok200 <$> settingsView home
    | requestMethod r == methodPost -> Just <$> post home r
    | otherwise -> pure (Just (bad methodNotAllowed405 "GET or POST"))
  _ -> pure Nothing

data Current = Current
  { cProvider :: Text
  , cModel :: Text
  , cAuto :: Text
  , cStrategy :: Text
  , cWarnings :: [Text]
  }

current :: FilePath -> IO Current
current home = do
  (cfg, warnings) <- loadConfigAt home
  let text p = case getPath p cfg of
        Just (A.String t) -> Just (T.strip t)
        _ -> Nothing
      model = case getPath ["model"] cfg of
        Just (A.String m) -> T.strip m
        _ -> fromMaybe "" (text ["model", "default"])
      provider = case text ["model", "provider"] of
        Just p | not (T.null p) -> p
        _ -> "auto"
      strategy = case text ["fisher", "context"] of
        Just s | s `elem` strategies -> s
        _ -> "compress"
  pure (Current provider model (fromMaybe "" (text ["gauss", "autoformalize", "model"])) strategy warnings)

-- | The GET body.
settingsView :: FilePath -> IO A.Value
settingsView home = do
  c <- current home
  env <- either (\(_ :: SomeException) -> Map.empty) id <$> try (loadEnvAt home)
  let credential n = setTarget n == ToEnv n
      others = sort [n | n <- Map.keys env, credential n, isSecret n, n `notElem` mainKeys]
      plainNames = plainKnown <> sort [n | n <- Map.keys env, credential n, not (isSecret n), n `notElem` plainKnown]
      row n =
        let v = T.strip (Map.findWithDefault "" n env)
         in A.object ["name" A..= n, "set" A..= not (T.null v), "last4" A..= (if T.null v then Nothing else lastFour v)]
      plainRow n = A.object ["name" A..= n, "value" A..= T.strip (Map.findWithDefault "" n env)]
      ac = defaultAutoConfig
  pure $
    A.object
      [ "provider" A..= cProvider c
      , "model" A..= cModel c
      , "autoformalizeModel" A..= cAuto c
      , "contextStrategy" A..= cStrategy c
      , "strategies" A..= strategies
      , "auto" A..= A.object
          [ "tactics" A..= acTactics ac, "seconds" A..= acSeconds ac
          , "repair" A..= acRepair ac, "disprove" A..= acDisprove ac
          , "settable" A..= False ]
      , "keys" A..= map row (mainKeys <> others)
      , "plain" A..= map plainRow plainNames
      , "warnings" A..= cWarnings c
      ]

-- | A config value the page may write: text that stays text in YAML.
configText :: Bool -> Text -> Text -> Either Text Text
configText mayBeEmpty field raw =
  let v = T.strip raw
   in if T.any (`elem` ['\n', '\r', '\0']) v
        then Left (field <> " has a line break")
        else if T.null v && not mayBeEmpty
          then Left (field <> " is empty")
          else case coerceValue v of
            CText _ -> Right v
            _ -> Left (field <> " would be read as a number or a boolean")

-- | A key to write: a credential name, a value of one line. The messages
-- never name the value.
keyWrite :: A.Value -> Either Text (Text, Text)
keyWrite = \case
  A.Object o
    | Just (A.String n) <- KM.lookup "name" o
    , Just (A.String raw) <- KM.lookup "value" o ->
        let v = T.strip raw
         in if not (validEnvName n) || setTarget n /= ToEnv n
              then Left "a key's name is not a credential name (such as OPENROUTER_API_KEY)"
              else if T.null v
                then Left (n <> ": the value is empty")
                else if T.any (`elem` ['\n', '\r', '\0']) v
                  then Left (n <> ": the value has a line break")
                  else Right (n, v)
  _ -> Left "each key is {name, value}"

post :: FilePath -> Request -> IO Response
post home r =
  jsonBodyWith 65536 r >>= \case
    Nothing -> pure (bad badRequest400 "send the settings as a JSON object")
    Just o -> withMVar writeLock $ \() -> do
      c <- current home
      let field k = case KM.lookup (K.fromText k) o of
            Just (A.String t) -> Just (Right t)
            Just A.Null -> Nothing
            Nothing -> Nothing
            Just _ -> Just (Left (k <> " is not text"))
          checked k mayBeEmpty = fmap (>>= configText mayBeEmpty k) (field k)
          strategy = flip fmap (field "contextStrategy") $ \e -> e >>= \s ->
            if T.strip s `elem` strategies then Right (T.strip s) else Left "contextStrategy is one of compress, clm, rlm"
          fields =
            [ ("model", checked "model" False), ("provider", checked "provider" False)
            , ("autoformalizeModel", checked "autoformalizeModel" True), ("contextStrategy", strategy) ]
          keysIn = case KM.lookup "keys" o of
            Nothing -> Right []
            Just A.Null -> Right []
            Just (A.Array xs) -> Right (foldr (:) [] xs)
            Just _ -> Left "keys is a list of {name, value}"
          (errs, given) = partitionEithers [either Left (Right . (,) k) e | (k, Just e) <- fields]
      case (errs, keysIn >>= traverse keyWrite) of
        (e : _, _) -> pure (bad badRequest400 e)
        (_, Left e) -> pure (bad badRequest400 e)
        (_, Right keys) -> do
          let new k = lookup (k :: Text) given
              changed k old = maybe False (/= old) (new k)
              model = fromMaybe (cModel c) (new "model")
              -- model.provider beside a plain-string model would replace
              -- it, so model.default is written first whenever either moves
              writes =
                [ ("model.default", model) | changed "model" (cModel c) || changed "provider" (cProvider c) ]
                  <> [ ("model.provider", p) | changed "provider" (cProvider c), Just p <- [new "provider"] ]
                  <> [ ("gauss.autoformalize.model", a) | changed "autoformalizeModel" (cAuto c), Just a <- [new "autoformalizeModel"] ]
                  <> [ ("fisher.context", s) | changed "contextStrategy" (cStrategy c), Just s <- [new "contextStrategy"] ]
          ok <- if null writes then pure True else userFileIsMapping home
          if not ok
            then pure (bad conflict409 "config.yaml does not hold a mapping; fix it with fisher config edit")
            else do
              rs <- forM writes $ \(k, v) -> setConfigValueAt home k v
              case [e | Left e <- rs] of
                (e : _) -> pure (bad conflict409 e)
                [] -> do
                  kr <- try (forM_ keys (uncurry (saveEnvFileAt home)))
                  case kr of
                    Left (_ :: SomeException) -> pure (bad internalServerError500 "could not write .env")
                    Right () -> do
                      v <- settingsView home
                      let saved = map fst writes <> map fst keys
                      pure (jsonResponse ok200 (addSaved saved v))
  where
    addSaved s = \case
      A.Object x -> A.Object (KM.insert "saved" (A.toJSON s) x)
      v -> v

-- | Whether the user's config.yaml (if any) is a mapping or empty, so that
-- a write keeps what it holds.
userFileIsMapping :: FilePath -> IO Bool
userFileIsMapping home = do
  let p = configPathIn home
  here <- doesFileExist p
  if not here
    then pure True
    else do
      d <- Y.decodeFileEither p
      pure $ case d of
        Right (A.Object _) -> True
        Right A.Null -> True
        _ -> False
