-- | lean-lsp-mcp @tools/navigation.py@ and @tools/widgets.py@:
-- @lean_hover_info@, @lean_completions@, @lean_declaration_file@,
-- @lean_references@, @lean_get_widgets@, @lean_get_widget_source@.
module Fisher.Lean.Tools.Navigation
  ( hoverTool
  , completionsTool
  , declarationFileTool
  , referencesTool
  , widgetsTool
  , widgetSourceTool
  , extractRange
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, throwIO, try)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesPathExist)
import System.FilePath (isAbsolute, (</>))

import Fisher.Lean.Client
import Fisher.Lean.Session
import Fisher.Lean.Text (readFileContents, splitLines, splitLinesKeep)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common
import Fisher.Py.Value

-- | @extract_range(content, range)@ (code points).
extractRange :: Text -> Maybe PyVal -> Text
extractRange txt mrng = case mrng of
  Just rng | truthy rng ->
    let ls = case splitLinesKeep txt of [] -> [""]; xs -> xs
        offsets = scanl (+) 0 (map T.length ls)
        toOff l c
          | l == length ls && c == 0 = Just (T.length txt)
          | l < 0 || l >= length ls || c < 0 || c > T.length (ls !! l) = Nothing
          | otherwise = Just (offsets !! l + c)
        pos k = do
          l <- intAt [k, "line"] rng
          c <- intAt [k, "character"] rng
          toOff l c
     in case (pos "start", pos "end") of
          (Just s, Just e) | s <= e -> T.take (e - s) (T.drop s txt)
          _ -> "Range out of bounds"
  _ -> ""

hoverTool :: Tool
hoverTool =
  Tool "lean_hover_info" "hoverArguments" [fileField, ge "1" (field "line" TInt), ge "1" (field "column" TInt)] run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      fileContent <- content c rel
      let line = argInt a "line"
          col = argInt a "column"
      info0 <- hover c rel (line - 1) (col - 1) True
      case info0 of
        PNull -> toolError ("No hover information at line " <> tshow line <> ", column " <> tshow col)
        _ -> pure ()
      let symbol = extractRange fileContent (get "range" info0)
      contents <- maybe (pyKeyError "contents") pure (get "contents" info0)
      raw <- case contents of
        PDict _ -> pure (maybe (PStr "No hover information available.") id (get "value" contents))
        v -> pyAttrError v "get"
      let info = T.strip (T.replace "\n```" "" (T.replace "```lean\n" "" (pyStrOf raw)))
      report <- diagnostics c rel False Nothing False
      limit <- maxOutputChars s
      let msgs = [m | Just m <- map (diagnosticMessage limit) (filterByPosition (drItems report) (line - 1) (Just (col - 1)))]
      pure (Model (PDict [("symbol", PStr symbol), ("info", PStr info), ("diagnostics", PList msgs)]))

completionKind :: Integer -> Maybe Text
completionKind k =
  lookup k (zip [1 ..] ["text", "method", "function", "constructor", "field", "variable", "class", "interface", "module", "property", "unit", "value", "enum", "keyword", "snippet", "color", "file", "reference", "folder", "enum_member", "constant", "struct", "theorem", "operator", "type_parameter"])

completionsTool :: Tool
completionsTool =
  Tool "lean_completions" "completionsArguments"
    [ fileField, ge "1" (field "line" TInt), ge "1" (field "column" TInt)
    , ge "1" (opt "max_completions" TInt (PInt 32)), ge "0" (opt "resolve_details" TInt (PInt 8)) ]
    run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      fileContent <- content c rel
      let line = argInt a "line"
          col = argInt a "column"
      raw0 <- completions c rel (line - 1) (col - 1) False
      let raw = [x | x <- raw0, has "label" x]
          lower = T.toLower . labelOf
          labelOf x = case get "label" x of Just (PStr l) -> l; Just v -> pyStrOf v; Nothing -> ""
          sorted
            | any (has "sortText") raw = sortOn (\x -> case get "sortText" x of Just (PStr t) -> t; Just v -> pyStrOf v; Nothing -> "\xffff") raw
            | otherwise =
                let prefix = completionPrefix fileContent line col
                 in if T.null prefix
                      then sortOn lower raw
                      else sortOn (\x -> let l = lower x in (if prefix `T.isPrefixOf` l then 0 :: Int else if prefix `T.isInfixOf` l then 1 else 2, l)) raw
          top = take (argInt a "max_completions") sorted
          n = argInt a "resolve_details"
      items <-
        if null raw
          then pure []
          else
            if n <= 0
              then pure top
              else do
                let (first, rest) = splitAt n top
                resolved <- mapConcurrently (resolveOne c) first
                pure (resolved <> rest)
      let item x =
            PDict
              [ ("label", PStr (labelOf x))
              , ("kind", case get "kind" x of Just (PInt k) | k /= 0 -> maybe PNull PStr (completionKind k); _ -> PNull)
              , ("detail", case get "detail" x of Just d@(PStr _) -> d; _ -> PNull) ]
      pure (Model (PDict [("items", PList (map item items))]))
    resolveOne c x
      | maybe False truthy (get "detail" x) = pure x
      | otherwise = do
          r <- try @SomeException (completionResolve c x 15)
          pure (either (const x) id r)

-- | @_completion_prefix(content, line, column)@.
completionPrefix :: Text -> Int -> Int -> Text
completionPrefix txt line col =
  let ls = splitLines txt
   in if line < 1 || line > length ls
        then ""
        else
          let before = if col > 0 then T.take (col - 1) (ls !! (line - 1)) else ""
           in if "." `T.isSuffixOf` before
                then ""
                else T.toLower (T.takeWhileEnd (not . sep) before)
  where
    sep ch = isSpace ch || ch `elem` ("()[]{},:;." :: String) || ch `elem` ("\x1c\x1d\x1e\x1f" :: String)

declarationFileTool :: Tool
declarationFileTool =
  Tool "lean_declaration_file" "declaration_fileArguments"
    [fileField, field "symbol" TStr, ge "0" (opt "context_lines" TInt (PInt 20)), opt "full_file" TBool (PBool False)]
    run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      orig <- content c rel
      let symbol = argText a "symbol"
          hits = [(i, T.length before) | (i, l) <- zip [0 :: Int ..] (splitLines orig), let (before, after) = T.breakOn symbol l, T.null symbol || not (T.null after)]
      (pl, pc) <- case hits of
        (h : _) -> pure h
        [] -> toolError ("Symbol `" <> symbol <> "` (case sensitive) not found in file. Add it first.")
      locations <- goto c "declaration" rel pl pc True
      decl <- case locations of
        (d : _) -> pure d
        [] -> toolError ("No declaration available for `" <> symbol <> "`.")
      let localPath = T.unpack (case get "path" decl of Just (PStr p) -> p; _ -> "")
      policy <- sessionPolicy s
      absPath <- validatePath policy (if isAbsolute localPath then localPath else clientProject c </> localPath)
      ex <- doesPathExist absPath
      if not ex
        then toolError ("Could not open declaration file `" <> T.pack absPath <> "` for `" <> symbol <> "`.")
        else pure ()
      fileContent <- readFileContents absPath
      display <- maybe "" id <$> displayPath policy absPath
      let fileLines = splitLines fileContent
          total = length fileLines
          ctx = argInt a "context_lines"
          model body st en = Model (PDict [("file_path", PStr display), ("content", PStr body), ("start_line", PInt (fromIntegral st)), ("end_line", PInt (fromIntegral en)), ("total_lines", PInt (fromIntegral total))])
      if argBool a "full_file"
        then pure (model fileContent (1 :: Int) total)
        else do
          let tr = maybe (PDict []) id (rangeOf "targetRange" "range" decl)
              ds = maybe 0 id (intAt ["start", "line"] tr)
              de = maybe ds id (intAt ["end", "line"] tr)
              st = max 0 (ds - ctx)
              en = min total (de + 1 + ctx)
          pure (model (T.intercalate "\n" (take (en - st) (drop st fileLines))) (st + 1) en)

referencesTool :: Tool
referencesTool =
  Tool "lean_references" "referencesArguments"
    [fileField, ge "1" (field "line" TInt), ge "1" (field "column" TInt), ge "1" (nullable "max_results" TInt (PInt 50))]
    run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") (Just "Invalid Lean file path: Unable to start LSP server or load file")
      c <- getClient s
      openSynced s rel False
      r <- try @SomeException (references c rel (argInt a "line" - 1) (argInt a "column" - 1) True Nothing True)
      refs0 <- case r of
        Right x -> pure x
        Left e -> toolError ("Failed to get references: " <> exceptionText e)
      let total = length refs0
          refs = maybe refs0 (`take` refs0) (argMaybeInt a "max_results")
      policy <- sessionPolicy s
      items <- fmap concat . mapM (one c policy) $ refs
      pure (Model (PDict [("items", PList items), ("total", PInt (fromIntegral total))]))
    one c policy ref = do
      let rng = maybe (PDict []) id (get "range" ref)
          p = case get "path" ref of Just (PStr x) -> x; _ -> ""
          n k1 k2 = PInt (fromIntegral (maybe 0 id (intAt [k1, k2] rng) + 1))
          loc d = [PDict [("file_path", PStr d), ("line", n "start" "line"), ("column", n "start" "character"), ("end_line", n "end" "line"), ("end_column", n "end" "character")]]
      if T.null p
        then pure (loc p)
        else do
          let cand = if isAbsolute (T.unpack p) then T.unpack p else clientProject c </> T.unpack p
          md <- displayPath policy cand
          pure (maybe [] loc md)

widgetsTool :: Tool
widgetsTool =
  Tool "lean_get_widgets" "get_widgetsArguments" [fileField, ge "1" (field "line" TInt), ge "1" (field "column" TInt)] run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      ws <- getWidgets c rel (argInt a "line" - 1) (argInt a "column" - 1) True
      pure (Model (PDict [("widgets", PList ws)]))

widgetSourceTool :: Tool
widgetSourceTool =
  Tool "lean_get_widget_source" "get_widget_sourceArguments" [fileField, field "javascript_hash" TStr] run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel True
      let hash = argText a "javascript_hash"
      src <- getWidgetSource c rel 0 0 hash
      case src of
        PNull -> toolError ("Widget source not found for hash `" <> hash <> "`.")
        PDict _ -> pure (Model (PDict [("source", src)]))
        v -> throwIO (ToolError ("1 validation error for WidgetSourceResult\nsource\n  Input should be a valid dictionary [type=dict_type, input_value=" <> pyRepr v <> ", input_type=" <> pyTypeName' v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/dict_type"))

pyTypeName' :: PyVal -> Text
pyTypeName' = \case
  PStr _ -> "str"
  PList _ -> "list"
  PInt _ -> "int"
  PFloat _ -> "float"
  PBool _ -> "bool"
  _ -> "NoneType"
