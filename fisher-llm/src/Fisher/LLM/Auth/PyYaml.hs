-- | YAML as PyYAML's @safe_load@ and @safe_dump(sort_keys=False)@ see it,
-- over 'PyVal': mappings keep document order, and ints, floats, bools and
-- nulls resolve by YAML 1.1's implicit rules (so @3.0@ stays a float and is
-- written back as @3.0@). On top of it, @load_config@ and @save_config@ of
-- @gauss_cli/config.py@ with the dict exactly as Python holds it: the
-- defaults' keys first, the user's own appended, later changes in place.
module Fisher.LLM.Auth.PyYaml
  ( parsePyYaml
  , dumpPyYaml
  , deepMergePy
  , loadConfigOrdered
  , saveConfigOrdered
  , foldLegacyPy
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Conduit (runConduitRes, (.|))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Yaml.Builder as B
import Data.Yaml.Parser (RawDoc (..), YamlValue (..), sinkRawDoc)
import Numeric (readHex, readOct)
import System.Directory (doesFileExist)
import qualified Text.Libyaml as L

import Fisher.Atomic (atomicWriteFile, secureFile)
import Fisher.Config (defaultConfigYaml, loadConfigAt, normalizeMaxTurns, renderConfigWith)
import qualified Fisher.Config as Config
import Fisher.LLM.Sys (Sys (..), outLn)
import Fisher.Paths (configPathIn, ensureFisherHomeAt)
import Fisher.Py.Json (pyFloatRepr)
import Fisher.Py.Value
import Fisher.Yaml.Ordered (encodeOrdered)

-- | @yaml.safe_load@ of one document; Left where PyYAML raises.
parsePyYaml :: BS.ByteString -> IO (Either String PyVal)
parsePyYaml bs
  | BC.all (`elem` (" \t\r\n" :: String)) bs = pure (Right PNull)
  | otherwise = do
      r <- try (runConduitRes (L.decode bs .| sinkRawDoc)) :: IO (Either SomeException RawDoc)
      pure $ case r of
        Left e -> Left (show e)
        Right (RawDoc v anchors) -> Right (convert anchors v)

convert :: Map.Map String YamlValue -> YamlValue -> PyVal
convert anchors = go
  where
    go = \case
      Mapping kvs _ -> PDict (foldl (\acc (k, v) -> setKey k (go v) acc) [] kvs)
      Sequence xs _ -> PList (map go xs)
      Scalar raw tag style _ -> scalar (TE.decodeUtf8With TEE.lenientDecode raw) tag style
      Alias n -> maybe PNull go (Map.lookup n anchors)
    setKey k v acc
      | any ((== k) . fst) acc = [(k', if k' == k then v else v') | (k', v') <- acc]
      | otherwise = acc <> [(k, v)]

scalar :: Text -> L.Tag -> L.Style -> PyVal
scalar t tag style = case tag of
  L.StrTag -> PStr t
  L.NullTag -> PNull
  L.BoolTag -> maybe (PStr t) PBool (boolOf t)
  L.IntTag -> maybe (PStr t) PInt (intOf t)
  L.FloatTag -> maybe (PStr t) PFloat (floatOf t)
  _
    | style /= L.Plain && style /= L.PlainNoTag && style /= L.Any -> PStr t
    | t `elem` ["", "~", "null", "Null", "NULL"] -> PNull
    | Just b <- boolOf t -> PBool b
    | Just n <- intOf t -> PInt n
    | Just d <- floatOf t -> PFloat d
    | otherwise -> PStr t

boolOf :: Text -> Maybe Bool
boolOf t
  | t `elem` ["yes", "Yes", "YES", "true", "True", "TRUE", "on", "On", "ON"] = Just True
  | t `elem` ["no", "No", "NO", "false", "False", "FALSE", "off", "Off", "OFF"] = Just False
  | otherwise = Nothing

-- | PyYAML's int resolver (without the sexagesimal form).
intOf :: Text -> Maybe Integer
intOf t0 =
  let (sign, t) = case T.uncons t0 of
        Just ('-', r) -> (-1, r)
        Just ('+', r) -> (1, r)
        _ -> (1, t0)
      clean = T.filter (/= '_')
   in fmap (sign *) $ case T.unpack t of
        '0' : 'b' : ds | not (null ds), all (`elem` ("01_" :: String)) ds -> Just (foldl (\a c -> a * 2 + (if c == '1' then 1 else 0)) 0 (filter (/= '_') ds))
        '0' : 'x' : ds | not (null ds), all (\c -> isHexDigit c || c == '_') ds -> Just (fst (head (readHex (filter (/= '_') ds))))
        "0" -> Just 0
        '0' : ds | all (\c -> isOctDigit c || c == '_') ds -> Just (case readOct (filter (/= '_') ds) of ((v, _) : _) -> v; _ -> 0)
        c : ds | c `elem` ['1' .. '9'], all (\x -> isDigit x || x == '_') ds -> Just (read (T.unpack (clean t)))
        _ -> Nothing

-- | PyYAML's float resolver: a dot is required (@1e5@ is a string).
floatOf :: Text -> Maybe Double
floatOf t0
  | t0 `elem` [".inf", ".Inf", ".INF", "+.inf", "+.Inf", "+.INF"] = Just (1 / 0)
  | t0 `elem` ["-.inf", "-.Inf", "-.INF"] = Just (-1 / 0)
  | t0 `elem` [".nan", ".NaN", ".NAN"] = Just (0 / 0)
  | otherwise =
      let (sign, t) = case T.uncons t0 of
            Just ('-', r) -> (-1, r)
            Just ('+', r) -> (1, r)
            _ -> (1, t0)
          (mant, ex) = T.break (`elem` ("eE" :: String)) t
          (ip, fp) = T.breakOn "." mant
          digitsOk s = T.all (\c -> isDigit c || c == '_') s
          expOk = T.null ex || case T.unpack (T.drop 1 ex) of
            s : ds | s `elem` ("+-" :: String), not (null ds), all isDigit ds -> True
            _ -> False
          ok =
            not (T.null fp) && digitsOk ip && digitsOk (T.drop 1 fp) && expOk
              && (if T.null ip then T.length fp > 1 && not ("_" `T.isPrefixOf` T.drop 1 fp) else isDigit (T.head ip))
          num = T.filter (/= '_') (if T.null ip then "0" else ip) <> "." <> T.filter (/= '_') (T.drop 1 fp) <> "0" <> ex
       in if ok then Just (sign * read (T.unpack num)) else Nothing

-- | @yaml.safe_dump(v, sort_keys=False)@ (block style).
dumpPyYaml :: PyVal -> BS.ByteString
dumpPyYaml = B.toByteString . go
  where
    go = \case
      PDict kvs -> B.mapping [(k, go v) | (k, v) <- kvs]
      PList xs -> B.array (map go xs)
      PStr s -> B.string s
      PInt n -> B.scientific (fromInteger n)
      PBool b -> B.bool b
      PNull -> B.null
      PFloat d -> plain (floatText d)
    plain t = B.YamlBuilder (L.EventScalar (TE.encodeUtf8 t) L.NoTag L.PlainNoTag Nothing :)
    floatText d
      | isNaN d = ".nan"
      | isInfinite d = if d > 0 then ".inf" else "-.inf"
      | otherwise =
          let r = T.toLower (pyFloatRepr d)
           in if not (T.any (== '.') r) && T.any (== 'e') r then T.replace "e" ".0e" r else r

-- | @_deep_merge(base, override)@: the override's keys win, mappings on
-- both sides merge, new keys go at the end.
deepMergePy :: PyVal -> PyVal -> PyVal
deepMergePy (PDict base) (PDict over) = PDict (foldl step base over)
  where
    step acc (k, v) = case lookup k acc of
      Just b@(PDict _) | PDict _ <- v -> [(k', if k' == k then deepMergePy b v else x) | (k', x) <- acc]
      Just _ -> [(k', if k' == k then v else x) | (k', x) <- acc]
      Nothing -> acc <> [(k, v)]
deepMergePy _ over = over

-- | @_normalize_max_turns_config@, keys left where they are.
normalizeMaxTurnsPy :: PyVal -> PyVal
normalizeMaxTurnsPy cfg =
  let agent0 = case get "agent" cfg of
        Just a@(PDict _) -> a
        _ -> PDict []
      agent1 = case get "max_turns" cfg of
        Just mt | not (has "max_turns" agent0) -> set "max_turns" mt agent0
        _ -> agent0
      dflt = maybe (PInt 90) fromAeson (Config.getPath ["agent", "max_turns"] Config.defaultConfig)
      agent2 = if has "max_turns" agent1 then agent1 else set "max_turns" dflt agent1
   in delete "max_turns" (set "agent" agent2 cfg)

-- | 'Fisher.Compat.normalizeConfigCompat' on a 'PyVal': the @gauss@ section
-- folded into @fisher@ (@fisher@ winning leaf by leaf; @fisher@ keeps its
-- place, or takes @gauss@'s when it had none) and @gauss@ dropped.
foldLegacyPy :: PyVal -> PyVal
foldLegacyPy cfg = case get "gauss" cfg of
  Nothing -> cfg
  Just g ->
    let merged = maybe g (deepMergePy g) (get "fisher" cfg)
     in if has "fisher" cfg
          then delete "gauss" (set "fisher" merged cfg)
          else case cfg of
            PDict kvs -> PDict [if k == "gauss" then ("fisher", merged) else (k, v) | (k, v) <- kvs]
            _ -> cfg

-- | The @gauss@ section of the configuration file at a path, as PyYAML
-- reads it, when there is one.
legacySectionPy :: FilePath -> IO (Maybe PyVal)
legacySectionPy path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else either (const Nothing) (get "gauss") <$> (BS.readFile path >>= parsePyYaml)

-- | @load_config()@: the home made sure of, the user file merged over the
-- defaults in Python's order. A @gauss@ section in the file is read as
-- @fisher@ first ('foldLegacyPy'), so a user's @gauss.*@ setting beats a
-- @fisher.*@ default. A file that does not load warns and gives
-- the defaults, as upstream.
loadConfigOrdered :: Sys -> IO PyVal
loadConfigOrdered sys = do
  let home = sysFisherHome sys
      path = configPathIn home
  ensureFisherHomeAt home
  defaults <- either (const (PDict [])) id <$> parsePyYaml defaultConfigYaml
  exists <- doesFileExist path
  if not exists
    then pure (normalizeWith defaults defaults)
    else do
      bytes <- BS.readFile path
      parsePyYaml bytes >>= \case
        Right v | not (truthy v) -> pure (normalizeWith defaults defaults)
        Right user@(PDict _) -> do
          let user' = case get "max_turns" user of
                Nothing -> user
                Just mt ->
                  let agent = case get "agent" user of
                        Just a@(PDict _) -> a
                        _ -> PDict []
                      agent' = case get "max_turns" agent of
                        Just v | v /= PNull -> agent
                        _ -> set "max_turns" mt agent
                   in delete "max_turns" (set "agent" agent' user)
          pure (normalizeWith defaults (deepMergePy defaults (foldLegacyPy user')))
        _ -> do
          -- Upstream's own message for the failure; Fisher's loader words it.
          (_, warnings) <- loadConfigAt home
          mapM_ (outLn sys) warnings
          pure (normalizeWith defaults defaults)
  where
    normalizeWith defaults cfg =
      let agent0 = case get "agent" cfg of
            Just a@(PDict _) -> a
            _ -> PDict []
          agent1 = case get "max_turns" cfg of
            Just mt | not (has "max_turns" agent0) -> set "max_turns" mt agent0
            _ -> agent0
          dflt = maybe (PInt 90) id (getPath ["agent", "max_turns"] defaults)
          agent2 = if has "max_turns" agent1 then agent1 else set "max_turns" dflt agent1
       in delete "max_turns" (set "agent" agent2 cfg)

-- | @save_config(config)@: the dict dumped in its own order, the
-- commented-out security and fallback sections after it when those are
-- not configured, written atomically at 0600. Settings go under @fisher@;
-- the file's existing @gauss@ section, if any, is written back unchanged
-- (at the end, as a key the defaults lack).
saveConfigOrdered :: Sys -> PyVal -> IO ()
saveConfigOrdered sys cfg = do
  let home = sysFisherHome sys
      path = configPathIn home
  legacy <- legacySectionPy path
  let normalized0 = normalizeMaxTurnsPy (foldLegacyPy cfg)
      normalized = maybe normalized0 (\g -> set "gauss" g normalized0) legacy
      -- The comments depend on security and fallback_model only; the dict
      -- without the kept gauss section renders the same body both ways.
      av = normalizeMaxTurns (toAeson normalized0)
      comments = BS.drop (BS.length (encodeOrdered [] av)) (renderConfigWith [] av)
  ensureFisherHomeAt home
  atomicWriteFile ".config_.tmp" path (dumpPyYaml normalized <> comments)
  secureFile path
