-- | How lean-lsp-mcp's tools take arguments and give results (the MCP SDK's
-- @func_metadata@ and @convert_result@ around each tool):
--
-- * arguments validated in pydantic's lax mode (strings that parse as JSON
--   lists, objects or null replace themselves for non-str fields; ints from
--   integral floats and numeric strings; extra keys ignored), with
--   pydantic's error texts;
-- * a result as text (@json.dumps(indent=2, ensure_ascii=False)@ of the
--   model, nulls kept) plus @structuredContent@ (the same dict, wrapped in
--   @{"result": …}@ for a union return);
-- * any failure as @"Error executing tool <name>: <message>"@ with isError.
module Fisher.Lean.Tool
  ( Ty (..)
  , Field (..)
  , field
  , opt
  , nullable
  , ge
  , le
  , Args
  , arg
  , argText
  , argInt
  , argMaybeInt
  , argDouble
  , argMaybeDouble
  , argBool
  , argList
  , validate
  , ToolOut (..)
  , ToolResult (..)
  , Tool (..)
  , runTool
  , resultPy
  , modelText
  , exceptionText
  ) where

import Control.Exception (Exception (..), SomeException, fromException, try)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Lean.Error (LeanError, errorText)
import Fisher.Lean.Session (Session, ToolError (..))
import Fisher.Py.Value
import Fisher.Tools.Py (pyFloat, pyInt, pyStrip)

data Ty
  = TStr
  | TInt
  | TFloat
  | TBool
  | TLit [Text]
  | TList
  deriving (Eq, Show)

data Field = Field
  { fName :: Text
  , fTy :: Ty
  , fNullable :: Bool
  , fDefault :: Maybe PyVal
  -- ^ Nothing: required
  , fGe :: Maybe Text
  , fLe :: Maybe Text
  -- ^ bounds as pydantic prints them (@1@, @5.0@)
  }

field :: Text -> Ty -> Field
field n t = Field n t False Nothing Nothing Nothing

opt :: Text -> Ty -> PyVal -> Field
opt n t d = Field n t False (Just d) Nothing Nothing

-- | @T | None = None@.
nullable :: Text -> Ty -> PyVal -> Field
nullable n t d = Field n t True (Just d) Nothing Nothing

ge :: Text -> Field -> Field
ge b f = f {fGe = Just b}

le :: Text -> Field -> Field
le b f = f {fLe = Just b}

type Args = M.Map Text PyVal

arg :: Args -> Text -> PyVal
arg a k = fromMaybe PNull (M.lookup k a)

argText :: Args -> Text -> Text
argText a k = case arg a k of PStr s -> s; _ -> ""

argInt :: Args -> Text -> Int
argInt a k = case arg a k of PInt n -> fromIntegral n; _ -> 0

argMaybeInt :: Args -> Text -> Maybe Int
argMaybeInt a k = case arg a k of PInt n -> Just (fromIntegral n); _ -> Nothing

argDouble :: Args -> Text -> Double
argDouble a k = case arg a k of PFloat d -> d; PInt n -> fromIntegral n; _ -> 0

argMaybeDouble :: Args -> Text -> Maybe Double
argMaybeDouble a k = case arg a k of PFloat d -> Just d; PInt n -> Just (fromIntegral n); _ -> Nothing

argBool :: Args -> Text -> Bool
argBool a k = arg a k == PBool True

argList :: Args -> Text -> [PyVal]
argList a k = case arg a k of PList xs -> xs; _ -> []

pyTypeOf :: PyVal -> Text
pyTypeOf = \case
  PNull -> "NoneType"
  PBool _ -> "bool"
  PInt _ -> "int"
  PFloat _ -> "float"
  PStr _ -> "str"
  PList _ -> "list"
  PDict _ -> "dict"

data VErr = VErr Text Text Text
-- ^ loc, message, type

coerce :: Ty -> PyVal -> Either (Text, Text) PyVal
coerce ty v = case (ty, v) of
  (TStr, PStr _) -> Right v
  (TStr, _) -> Left ("Input should be a valid string", "string_type")
  (TInt, PBool b) -> Right (PInt (if b then 1 else 0))
  (TInt, PInt _) -> Right v
  (TInt, PFloat d)
    | isNaN d || isInfinite d -> Left ("Input should be a finite number", "finite_number")
    | fromIntegral (truncate d :: Integer) == d -> Right (PInt (truncate d))
    | otherwise -> Left ("Input should be a valid integer, got a number with a fractional part", "int_from_float")
  (TInt, PStr s) -> case pyInt (pyStrip s) of
    Just n -> Right (PInt n)
    Nothing -> case pyFloat s of
      Just d | fromIntegral (truncate d :: Integer) == d -> Right (PInt (truncate d))
      Just _ -> Left ("Input should be a valid integer, got a number with a fractional part", "int_from_float")
      Nothing -> Left ("Input should be a valid integer, unable to parse string as an integer", "int_parsing")
  (TInt, _) -> Left ("Input should be a valid integer", "int_type")
  (TFloat, PBool b) -> Right (PFloat (if b then 1 else 0))
  (TFloat, PInt n) -> Right (PFloat (fromIntegral n))
  (TFloat, PFloat _) -> Right v
  (TFloat, PStr s) -> maybe (Left ("Input should be a valid number, unable to parse string as a number", "float_parsing")) (Right . PFloat) (pyFloat s)
  (TFloat, _) -> Left ("Input should be a valid number", "float_type")
  (TBool, PBool _) -> Right v
  (TBool, PInt n) | n == 0 || n == 1 -> Right (PBool (n == 1))
  (TBool, PFloat d) | d == 0 || d == 1 -> Right (PBool (d == 1))
  (TBool, PStr s)
    | T.toLower (pyStrip s) `elem` ["0", "off", "f", "false", "n", "no"] -> Right (PBool False)
    | T.toLower (pyStrip s) `elem` ["1", "on", "t", "true", "y", "yes"] -> Right (PBool True)
    | otherwise -> Left ("Input should be a valid boolean, unable to interpret input", "bool_parsing")
  (TBool, PInt _) -> Left ("Input should be a valid boolean, unable to interpret input", "bool_parsing")
  (TBool, _) -> Left ("Input should be a valid boolean", "bool_type")
  (TLit opts, PStr s) | s `elem` opts -> Right v
  (TLit opts, _) -> Left ("Input should be " <> litList opts, "literal_error")
  (TList, PList _) -> Right v
  (TList, _) -> Left ("Input should be a valid list", "list_type")
  where
    litList opts = case map (\o -> "'" <> o <> "'") opts of
      [a] -> a
      xs -> T.intercalate ", " (init xs) <> " or " <> last xs

-- | @pre_parse_json@ then the model: the arguments with defaults filled in,
-- or pydantic's ValidationError text.
validate :: Text -> [Field] -> PyVal -> Either Text Args
validate model fields raw =
  let given = case raw of PDict kvs -> kvs; _ -> []
      pre f v = case (fTy f, v) of
        (TStr, _) -> v
        (_, PStr s) -> case loads (TE.encodeUtf8 s) of
          Right p@(PList _) -> p
          Right p@(PDict _) -> p
          Right PNull -> PNull
          _ -> v
        _ -> v
      check f = case lookup (fName f) given of
        Nothing -> case fDefault f of
          Just d -> Right (fName f, d)
          Nothing -> Left (VErr (fName f) "Field required" "missing", raw)
        Just v0 ->
          let v = pre f v0
           in if v == PNull && fNullable f
                then Right (fName f, PNull)
                else case coerce (fTy f) v of
                  Left (m, ty) -> Left (VErr (fName f) m ty, v)
                  Right c -> case bounds f c of
                    Just (m, ty) -> Left (VErr (fName f) m ty, c)
                    Nothing -> Right (fName f, c)
      results = map check fields
      errs = [e | Left e <- results]
   in if null errs
        then Right (M.fromList [r | Right r <- results])
        else Left (render errs)
  where
    num = \case
      PInt n -> Just (fromIntegral n :: Double)
      PFloat d -> Just d
      _ -> Nothing
    bounds f c = case num c of
      Nothing -> Nothing
      Just x
        | Just g <- fGe f, Just gv <- pyFloat g, x < gv -> Just ("Input should be greater than or equal to " <> g, "greater_than_equal")
        | Just l <- fLe f, Just lv <- pyFloat l, x > lv -> Just ("Input should be less than or equal to " <> l, "less_than_equal")
        | otherwise -> Nothing
    render errs =
      let n = length errs
          header = T.pack (show n) <> " validation error" <> (if n == 1 then "" else "s") <> " for " <> model
          one (VErr loc m ty, v) =
            loc <> "\n  " <> m <> " [type=" <> ty <> ", input_value=" <> pyRepr v <> ", input_type=" <> pyTypeOf v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/" <> ty
       in T.intercalate "\n" (header : map one errs)

-- | What a tool returns: its model as a dict, or (a union return type) the
-- model the SDK wraps in @{"result": …}@.
data ToolOut
  = Model PyVal
  | Wrapped PyVal

data ToolResult = ToolResult
  { trText :: Text
  , trStructured :: Maybe PyVal
  , trIsError :: Bool
  }
  deriving (Eq, Show)

data Tool = Tool
  { toolName :: Text
  , toolModel :: Text
  -- ^ the arguments model's title (@goalArguments@)
  , toolFields :: [Field]
  , toolRun :: Session -> Args -> IO ToolOut
  }

-- | @pydantic_core.to_json(model, indent=2)@: Python's
-- @json.dumps(indent=2, ensure_ascii=False)@ with floats as repr.
modelText :: PyVal -> Text
modelText = dumpsIndent False 2

runTool :: Tool -> Session -> PyVal -> IO ToolResult
runTool t s raw = case validate (toolModel t) (toolFields t) raw of
  Left e -> pure (failure e)
  Right args -> do
    r <- try (toolRun t s args)
    pure $ case r of
      Right (Model m) -> ToolResult (modelText m) (Just m) False
      Right (Wrapped m) -> ToolResult (modelText m) (Just (PDict [("result", m)])) False
      Left e -> failure (exceptionText e)
  where
    failure msg = ToolResult ("Error executing tool " <> toolName t <> ": " <> msg) Nothing True

-- | @str(exc)@ as the tool reports it.
exceptionText :: SomeException -> Text
exceptionText e
  | Just (ToolError m) <- fromException e = m
  | Just (lerr :: LeanError) <- fromException e = errorText lerr
  | otherwise = T.dropWhileEnd isSpace (T.pack (displayException e))

-- | The CallToolResult as lean-lsp-mcp sends it (content, isError,
-- structuredContent).
resultPy :: ToolResult -> PyVal
resultPy r =
  PDict $
    [("content", PList [PDict [("text", PStr (trText r)), ("type", PStr "text")]]), ("isError", PBool (trIsError r))]
      <> maybe [] (\s -> [("structuredContent", s)]) (trStructured r)
