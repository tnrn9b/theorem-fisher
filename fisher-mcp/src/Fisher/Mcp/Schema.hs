-- | What the MCP SDK does with a tool's @outputSchema@: validate the
-- structured result with @jsonschema@ and, on failure, raise its message
-- (best match, "Failed validating ...", @pprint@ of schema and instance).
-- The common keywords are covered (type, properties, required, items,
-- additionalProperties, enum, const, minimum, maximum and their exclusive
-- forms, minLength, maxLength, minItems, maxItems); others are ignored.
module Fisher.Mcp.Schema
  ( validateSchema
  , pformat
  ) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Value (PyVal (..), pyRepr)

data VError = VError
  { veMessage :: Text
  , veValidator :: Text
  , veSchemaPath :: [PyVal]
  -- ^ relative schema path including the keyword
  , vePath :: [PyVal]
  , veSchema :: PyVal
  , veInstance :: PyVal
  }

-- | 'Nothing' when the instance is valid; otherwise jsonschema's
-- @str(ValidationError)@ for the best match.
validateSchema :: PyVal -> PyVal -> Maybe Text
validateSchema schema inst =
  case errorsOf schema inst [] [] of
    [] -> Nothing
    es -> Just (render (bestOf es))
  where
    bestOf es =
      -- max by relevance; the first of equal ones wins
      let keyed = zip [0 :: Int ..] es
          key (_, e) = (Down (length (vePath e)), map pathKey (vePath e), veValidator e `notElem` ["anyOf", "oneOf"])
       in snd (head (sortOn (\(i, e) -> (Down (key (i, e)), i)) keyed))
    pathKey = \case
      PStr s -> Right s
      PInt n -> Left n
      v -> Right (pyRepr v)

render :: VError -> Text
render e =
  veMessage e
    <> "\n\nFailed validating "
    <> pyRepr (PStr (veValidator e))
    <> " in "
    <> index "schema" (init' (veSchemaPath e))
    <> ":\n    "
    <> indentRest (pformat (veSchema e))
    <> "\n\nOn "
    <> index "instance" (vePath e)
    <> ":\n    "
    <> indentRest (pformat (veInstance e))
  where
    init' xs = if null xs then [] else init xs
    index c [] = c
    index c is = c <> "[" <> T.intercalate "][" (map pyRepr is) <> "]"
    indentRest t = T.intercalate "\n    " (T.splitOn "\n" t)

typeMatches :: Text -> PyVal -> Bool
typeMatches t v = case (t, v) of
  ("object", PDict _) -> True
  ("array", PList _) -> True
  ("string", PStr _) -> True
  ("boolean", PBool _) -> True
  ("null", PNull) -> True
  ("integer", PInt _) -> True
  ("integer", PFloat d) -> d == fromIntegral (truncate d :: Integer)
  ("number", PInt _) -> True
  ("number", PFloat _) -> True
  _ -> False

num :: PyVal -> Maybe Double
num = \case
  PInt n -> Just (fromIntegral n)
  PFloat d -> Just d
  _ -> Nothing

errorsOf :: PyVal -> PyVal -> [PyVal] -> [PyVal] -> [VError]
errorsOf schema inst path spath = case schema of
  PDict kvs -> concatMap (keyword kvs) kvs
  _ -> []
  where
    err kw msg = VError msg kw (spath <> [PStr kw]) path schema inst
    keyword kvs (kw, val) = case (kw, val) of
      ("type", PStr t) -> [err kw (pyRepr inst <> " is not of type " <> pyRepr (PStr t)) | not (typeMatches t inst)]
      ("type", PList ts) ->
        [err kw (pyRepr inst <> " is not of type " <> T.intercalate ", " (map pyRepr ts)) | not (any (\case PStr t -> typeMatches t inst; _ -> False) ts)]
      ("properties", PDict props) -> case inst of
        PDict ikvs -> concat [errorsOf sub iv (path <> [PStr p]) (spath <> [PStr "properties", PStr p]) | (p, sub) <- props, Just iv <- [lookup p ikvs]]
        _ -> []
      ("required", PList req) -> case inst of
        PDict ikvs -> [err kw (pyRepr r <> " is a required property") | r@(PStr p) <- req, p `notElem` map fst ikvs]
        _ -> []
      ("items", sub@(PDict _)) -> case inst of
        PList xs ->
          let skip = maybe 0 (\case PList ps -> length ps; _ -> 0) (lookup "prefixItems" kvs)
           in concat [errorsOf sub x (path <> [PInt (fromIntegral i)]) (spath <> [PStr "items"]) | (i, x) <- drop skip (zip [0 :: Int ..] xs)]
        _ -> []
      ("additionalProperties", PBool False) -> case inst of
        PDict ikvs ->
          let known = maybe [] (\case PDict ps -> map fst ps; _ -> []) (lookup "properties" kvs)
              extras = [k | (k, _) <- ikvs, k `notElem` known]
           in [ err kw ("Additional properties are not allowed (" <> T.intercalate ", " (map (pyRepr . PStr) extras) <> (if length extras == 1 then " was" else " were") <> " unexpected)")
              | not (null extras)
              ]
        _ -> []
      ("additionalProperties", sub@(PDict _)) -> case inst of
        PDict ikvs ->
          let known = maybe [] (\case PDict ps -> map fst ps; _ -> []) (lookup "properties" kvs)
           in concat [errorsOf sub v (path <> [PStr k]) (spath <> [PStr "additionalProperties"]) | (k, v) <- ikvs, k `notElem` known]
        _ -> []
      ("enum", PList opts) -> [err kw (pyRepr inst <> " is not one of " <> pyRepr (PList opts)) | inst `notElem` opts]
      ("const", c) -> [err kw (pyRepr c <> " was expected") | c /= inst]
      ("minimum", m) | Just lo <- num m, Just x <- num inst -> [err kw (pyRepr inst <> " is less than the minimum of " <> pyRepr m) | x < lo]
      ("maximum", m) | Just hi <- num m, Just x <- num inst -> [err kw (pyRepr inst <> " is greater than the maximum of " <> pyRepr m) | x > hi]
      ("exclusiveMinimum", m) | Just lo <- num m, Just x <- num inst -> [err kw (pyRepr inst <> " is less than or equal to the minimum of " <> pyRepr m) | x <= lo]
      ("exclusiveMaximum", m) | Just hi <- num m, Just x <- num inst -> [err kw (pyRepr inst <> " is greater than or equal to the maximum of " <> pyRepr m) | x >= hi]
      ("minLength", PInt n) | PStr s <- inst -> [err kw (if n == 1 then pyRepr inst <> " should be non-empty" else pyRepr inst <> " is too short") | fromIntegral (T.length s) < n]
      ("maxLength", PInt n) | PStr s <- inst -> [err kw (pyRepr inst <> " is too long") | fromIntegral (T.length s) > n]
      ("minItems", PInt n) | PList xs <- inst -> [err kw (if n == 1 then pyRepr inst <> " should be non-empty" else pyRepr inst <> " is too short") | fromIntegral (length xs) < n]
      ("maxItems", PInt n) | PList xs <- inst -> [err kw (pyRepr inst <> " is too long") | fromIntegral (length xs) > n]
      _ -> []

-- | @pprint.pformat(v, width=72, sort_dicts=False)@ for JSON values.
pformat :: PyVal -> Text
pformat v = fmt v 0 0
  where
    width = 72 :: Int
    fmt x ind allowance =
      let rep = pyRepr x
       in if T.length rep > width - ind - allowance
            then case x of
              PDict kvs | not (null kvs) -> "{" <> dictItems kvs (ind + 1) (allowance + 1) <> "}"
              PList xs | not (null xs) -> "[" <> listItems xs (ind + 1) (allowance + 1) <> "]"
              _ -> rep
            else rep
    dictItems kvs ind allowance =
      let n = length kvs
          one i (k, e) =
            let kr = pyRepr (PStr k)
                lastOne = i == n - 1
             in kr <> ": " <> fmt e (ind + T.length kr + 2) (if lastOne then allowance else 1)
       in T.intercalate (",\n" <> T.replicate ind " ") (zipWith one [0 ..] kvs)
    listItems xs ind allowance =
      let n = length xs
       in T.intercalate (",\n" <> T.replicate ind " ") [fmt e ind (if i == n - 1 then allowance else 1) | (i, e) <- zip [0 :: Int ..] xs]

