-- | Pieces several lean-lsp-mcp tools share: the @file_path@ field, LSP
-- range access, and diagnostics as the tools report them (@utils.py@
-- @_diagnostic_message@, the position and line-range filters).
module Fisher.Lean.Tools.Common
  ( fileField
  , tshow
  , intAt
  , rangeOf
  , diagnosticMessage
  , filterByPosition
  , filterByLineRange
  , hasErrors
  , pyAttrError
  , pyKeyError
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Session (boundOutput, toolError)
import Fisher.Lean.Tool
import Fisher.Py.Value

fileField :: Field
fileField = field "file_path" TStr

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | An integer under a key path, if there is one.
intAt :: [Text] -> PyVal -> Maybe Int
intAt ks v = case getPath ks v of
  Just (PInt n) -> Just (fromIntegral n)
  _ -> Nothing

-- | @d.get(a) or d.get(b)@ (a falsy first value falls through).
rangeOf :: Text -> Text -> PyVal -> Maybe PyVal
rangeOf a b d = case get a d of
  Just r | truthy r -> Just r
  _ -> case get b d of
    Just r | truthy r -> Just r
    _ -> Nothing

-- | @_diagnostic_message(d)@ with the output budget: Nothing when the
-- diagnostic has no range.
diagnosticMessage :: Int -> PyVal -> Maybe PyVal
diagnosticMessage limit d =
  let rng = case get "fullRange" d of
        Just r -> r
        Nothing -> maybe PNull id (get "range" d)
   in case rng of
        PNull -> Nothing
        _ ->
          let sevName = case maybe (PInt 1) id (get "severity" d) of
                PInt 1 -> "error"
                PInt 2 -> "warning"
                PInt 3 -> "info"
                PInt 4 -> "hint"
                v -> "unknown(" <> pyStrOf v <> ")"
              message = case get "message" d of Just (PStr m) -> m; Just v -> pyStrOf v; Nothing -> ""
              tags = case get "leanTags" d of
                Just (PList ts) | not (null ts) -> PList (map tagName ts)
                _ -> PNull
              tagName = \case
                PInt 1 -> PStr "unsolvedGoals"
                PInt 2 -> PStr "goalsAccomplished"
                v -> PStr (pyStrOf v)
              line = maybe 0 id (intAt ["start", "line"] rng)
              col = maybe 0 id (intAt ["start", "character"] rng)
           in Just $
                PDict
                  [ ("severity", PStr sevName)
                  , ("message", PStr (boundOutput limit message))
                  , ("line", PInt (fromIntegral (line + 1)))
                  , ("column", PInt (fromIntegral (col + 1)))
                  , ("lean_tags", tags)
                  , ("category", PStr (category sevName message))
                  , ("hint", maybe PNull PStr (hint sevName message)) ]

category :: Text -> Text -> Text
category sev m
  | "declaration uses 'sorry'" `T.isInfixOf` m || "declaration uses `sorry`" `T.isInfixOf` m = "sorry"
  | "Try this:" `T.isInfixOf` m = "suggestion"
  | sev == "warning" && any (`T.isInfixOf` m) ["this linter can be disabled", "unused variable", "automatically included section variable"] = "linter"
  | otherwise = "diagnostic"

hint :: Text -> Text -> Maybe Text
hint sev m
  | sev /= "error" = Nothing
  | "failed to synthesize instance of type class\n  DecidableEq " `T.isInfixOf` m = Just "add `classical` locally, or supply the `DecidableEq` instance"
  | "synthesized type class instance is not definitionally equal to expression inferred by typing rules" `T.isInfixOf` m =
      Just "construct both expressions under the same local instance; introduce `classical` before either one when decidability is involved"
  | "elaboration function for `Mathlib.Tactic.subscriptTerm` has not been implemented" `T.isInfixOf` m =
      Just "this notation is not active; open its scoped notation or use the named declaration"
  | otherwise =
      let marker = "invalid binder annotation, type is not a class instance"
       in case T.breakOn marker m of
            (_, r) | not (T.null r) ->
              case T.words (T.drop (T.length marker) r) of
                (w : _) | "?m." `T.isPrefixOf` w ->
                  Just "the binder type is unresolved; check that its class name resolves here (imports, namespace, shadowing) before changing the binder"
                _ -> Nothing
            _ -> Nothing

-- | @filter_diagnostics_by_position(diags, line, column)@ (0-based).
filterByPosition :: [PyVal] -> Int -> Maybe Int -> [PyVal]
filterByPosition ds line mcol = filter keep ds
  where
    keep d = case rangeOf "range" "fullRange" d of
      Nothing -> False
      Just rng -> case (intAt ["start", "line"] rng, intAt ["end", "line"] rng) of
        (Just sl, Just el)
          | line < sl || line > el -> False
          | otherwise ->
              let msc = intAt ["start", "character"] rng
                  mec = intAt ["end", "character"] rng
               in case mcol of
                    Nothing -> not (line == el && line /= sl && mec == Just 0)
                    Just column ->
                      let sc = maybe 0 id msc
                          ec = maybe (column + 1) id mec
                       in if sl == el && sc == ec
                            then column == sc
                            else not (line == sl && column < sc) && not (line == el && column >= ec)
        _ -> False

-- | @filter_diagnostics_by_line_range(diags, start, end)@ (0-based,
-- inclusive).
filterByLineRange :: [PyVal] -> Int -> Int -> [PyVal]
filterByLineRange ds s e = filter keep ds
  where
    keep d = case rangeOf "range" "fullRange" d of
      Nothing -> False
      Just rng -> case (intAt ["start", "line"] rng, intAt ["end", "line"] rng) of
        (Just sl, Just el) -> el >= s && sl <= e
        _ -> False

-- | @report.has_errors@.
hasErrors :: [PyVal] -> Bool
hasErrors = any (\d -> get "severity" d == Just (PInt 1))

-- | Python's @AttributeError@ text for @x.get@ on a non-dict.
pyAttrError :: PyVal -> Text -> IO a
pyAttrError v attr = toolError ("'" <> ty <> "' object has no attribute '" <> attr <> "'")
  where
    ty = case v of
      PStr _ -> "str"
      PList _ -> "list"
      PInt _ -> "int"
      PFloat _ -> "float"
      PBool _ -> "bool"
      PNull -> "NoneType"
      PDict _ -> "dict"

-- | Python's @KeyError@ text.
pyKeyError :: Text -> IO a
pyKeyError k = toolError ("'" <> k <> "'")
