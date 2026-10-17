-- | lean-lsp-mcp @tools/diagnostics.py@: @lean_diagnostic_messages@ and
-- @lean_code_actions@ (@lean_file_outline@ is in "Fisher.Lean.Tools.Outline").
module Fisher.Lean.Tools.Diagnostics
  ( diagnosticsTool
  , codeActionsTool
  , processDiagnostics
  , isBuildStderr
  , failedDependencyPaths
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Data.Char (isDigit, isSpace)
import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Client
import Fisher.Lean.Session
import Fisher.Lean.Text (splitLines)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common
import Fisher.Lean.Utf16 (rangeFromUtf16)
import Fisher.Py.Value

diagnosticsTool :: Tool
diagnosticsTool =
  Tool "lean_diagnostic_messages" "diagnostic_messagesArguments"
    [ fileField
    , ge "1" (nullable "start_line" TInt PNull)
    , ge "1" (nullable "end_line" TInt PNull)
    , nullable "declaration_name" TStr PNull
    , opt "interactive" TBool (PBool False)
    , ge "1" (nullable "timeout_s" TFloat PNull)
    , nullable "severity" (TLit ["error", "warning", "info", "hint"]) PNull ]
    run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      let declName = argText a "declaration_name"
      (startLine, endLine) <-
        if T.null declName
          then pure (argMaybeInt a "start_line", argMaybeInt a "end_line")
          else do
            r <- declarationRange c rel declName
            case r of
              Nothing -> toolError ("Declaration '" <> declName <> "' not found in file.")
              Just (x, y) -> pure (Just x, Just y)
      let start0 = subtract 1 <$> startLine
          end0 = subtract 1 <$> endLine
          big = 1073741824 :: Int
      if argBool a "interactive"
        then do
          let params = case (start0, end0) of
                (Nothing, Nothing) -> PDict []
                _ -> PDict [("lineRange", PDict [("start", PInt (fromIntegral (maybe 0 id start0))), ("end", PInt (fromIntegral (maybe big (+ 1) end0)))])]
          res <- rpcCall c rel (maybe 0 id start0) 0 "Lean.Widget.getInteractiveDiagnostics" params 20
          pure (Wrapped (PDict [("diagnostics", if truthy res then res else PList [])]))
        else do
          report <- diagnostics c rel True (argMaybeDouble a "timeout_s") True
          let processing = [PList [PInt (fromIntegral (l + 1)), PInt (fromIntegral (e + 1))] | rng <- drProcessing report, Just l <- [intAt ["start", "line"] rng], Just e <- [intAt ["end", "line"] rng]]
              items = case (start0, end0) of
                (Nothing, Nothing) -> drItems report
                _ -> filterByLineRange (drItems report) (maybe 0 id start0) (maybe big id end0)
              sev = case arg a "severity" of PStr x -> Just x; _ -> Nothing
          limit <- maxOutputChars s
          pure . Wrapped $
            processDiagnostics limit items (not (hasErrors (drItems report)) && not (drFatal report)) sev (drPartial report) (drPartial report)
              (if null processing then PNull else PList processing)

-- | @get_declaration_range(client, rel, name)@: 1-based start and end
-- lines of the first symbol so named (pre-order), any failure as Nothing.
declarationRange :: Client -> Text -> Text -> IO (Maybe (Int, Int))
declarationRange c rel name = do
  r <- try @SomeException (documentSymbols c rel True)
  pure $ case r of
    Left _ -> Nothing
    Right syms -> case search syms of
      Just sym -> case get "range" sym of
        Just rng -> (,) <$> ((+ 1) <$> intAt ["start", "line"] rng) <*> ((+ 1) <$> intAt ["end", "line"] rng)
        Nothing -> Nothing
      Nothing -> Nothing
  where
    search [] = Nothing
    search (x : xs)
      | get "name" x == Just (PStr name) = Just x
      | otherwise = case search [ch | Just (PList chs) <- [get "children" x], ch <- chs] of
          Just y -> Just y
          Nothing -> search xs

-- | @process_diagnostics(...)@.
processDiagnostics :: Int -> [PyVal] -> Bool -> Maybe Text -> Bool -> Bool -> PyVal -> PyVal
processDiagnostics limit ds buildSuccess severity timedOut partial processingLines =
  let step (acc, failed, seen) d = case diagnosticMessage limit d of
        Nothing -> (acc, failed, seen)
        Just m ->
          let full = case get "message" d of Just (PStr x) -> x; Just v -> pyStrOf v; Nothing -> ""
              mline = get "line" m
              mcol = get "column" m
              msev = case get "severity" m of Just (PStr x) -> x; _ -> ""
           in if mline == Just (PInt 1) && mcol == Just (PInt 1) && isBuildStderr full
                then (acc, map PStr (failedDependencyPaths full), seen)
                else
                  if maybe False (/= msev) severity
                    then (acc, failed, seen)
                    else
                      let ident = (msev, pyStrOf' mline, pyStrOf' mcol, full)
                       in if Set.member ident seen then (acc, failed, seen) else (acc <> [m], failed, Set.insert ident seen)
      pyStrOf' = maybe "" pyStrOf
      (items, failedDeps, _) = foldl step ([], [], Set.empty) ds
      result p stil ok to its =
        PDict
          [ ("partial", PBool p), ("still_elaborating_lines", stil), ("success", PBool ok), ("timed_out", PBool to)
          , ("items", PList its), ("failed_dependencies", PList failedDeps) ]
   in if partial
        then result True processingLines False True items
        else
          let items'
                | (not buildSuccess || timedOut) && null items =
                    let reason = if timedOut then "diagnostics_timed_out" else "diagnostics_unavailable" :: Text
                     in [ PDict
                            [ ("severity", PStr "error"), ("message", PStr (reason <> ": Lean did not finish; the file is not known clean."))
                            , ("line", PInt 1), ("column", PInt 1), ("lean_tags", PNull), ("category", PStr "diagnostic"), ("hint", PNull) ] ]
                | otherwise = items
           in result False PNull buildSuccess timedOut items'

-- | @is_build_stderr(m)@.
isBuildStderr :: Text -> Bool
isBuildStderr m = "lake setup-file" `T.isInfixOf` m || not (null (buildErrorFiles m))

-- | @extract_failed_dependency_paths(m)@.
failedDependencyPaths :: Text -> [Text]
failedDependencyPaths = sort . nub . buildErrorFiles

-- | Group 1 of each match of
-- @^(?:error|warning):\\s*([^\\s:]+\\.lean):\\d+:\\d+:@ (multiline,
-- ignore case). Line starts follow Python's @^@ in MULTILINE mode: after
-- @\\n@ only.
buildErrorFiles :: Text -> [Text]
buildErrorFiles m = [f | l <- T.splitOn "\n" m, Just f <- [lineMatch l]]
  where
    lineMatch l = do
      rest <- prefixCI "error:" l `orElse` prefixCI "warning:" l
      let r1 = T.dropWhile isSpace rest
          (name, r2) = T.span (\ch -> not (isSpace ch) && ch /= ':') r1
      -- [^\s:]+ cannot cross the colon, so the group is all of the name
      if T.length name > 5 && ".lean" `T.isSuffixOf` T.toLower name && digitsColon r2 then Just name else Nothing
    digitsColon t = case T.uncons t of
      Just (':', r) ->
        let (d1, r') = T.span isDigit r
         in not (T.null d1) && case T.uncons r' of
              Just (':', r'') -> let (d2, r3) = T.span isDigit r'' in not (T.null d2) && T.take 1 r3 == ":"
              _ -> False
      _ -> False
    prefixCI p l = if T.toLower (T.take (T.length p) l) == p then Just (T.drop (T.length p) l) else Nothing
    orElse (Just x) _ = Just x
    orElse Nothing y = y

codeActionsTool :: Tool
codeActionsTool =
  Tool "lean_code_actions" "code_actionsArguments" [fileField, ge "1" (field "line" TInt)] run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      let line = argInt a "line"
      report <- diagnostics c rel True Nothing False
      let lineDiags = filterByLineRange (drItems report) (line - 1) (line - 1)
          ranges =
            [ (sl, sc, el, ec)
            | d <- lineDiags
            , let rng = case get "fullRange" d of Just r -> r; Nothing -> maybe PNull id (get "range" d)
            , truthy rng
            , Just sl <- [intAt ["start", "line"] rng], Just sc <- [intAt ["start", "character"] rng]
            , Just el <- [intAt ["end", "line"] rng], Just ec <- [intAt ["end", "character"] rng] ]
          collect rs = do
            (_, acts) <- foldM (\(seen, acc) (sl, sc, el, ec) -> do
                                 as <- codeActions c rel sl sc el ec True
                                 pure (foldl (\(sn, ac) act -> let t = titleOf act in if Set.member t sn then (sn, ac) else (Set.insert t sn, ac <> [act])) (seen, acc) as))
                               (Set.empty, []) rs
            pure acts
      raw0 <- collect ranges
      raw <-
        if not (null raw0)
          then pure raw0
          else do
            ls <- splitLines <$> content c rel
            let lineStr = if line > 0 && line <= length ls then ls !! (line - 1) else ""
            if T.null lineStr then pure [] else collect [(line - 1, 0, line - 1, T.length lineStr)]
      docLs <- splitLines <$> content c rel
      actions <- fmap concat . mapM (resolveOne c docLs) $ raw
      pure (Model (PDict [("actions", PList actions)]))
    titleOf act = case get "title" act of Just (PStr t) -> t; Just v -> pyStrOf v; Nothing -> ""
    resolveOne c docLs raw = do
      r <- if has "edit" raw then pure (Right raw) else try @SomeException (codeActionResolve c raw 30)
      case r of
        Left _ -> pure []
        Right resolved -> do
          let changes = case get "edit" resolved of
                Just e | truthy e -> case get "documentChanges" e of Just (PList xs) -> xs; _ -> []
                _ -> []
              edits = [ed | ch <- changes, Just (PList eds) <- [get "edits" ch], ed <- eds]
          converted <- mapM (convertEdit docLs) edits
          pure
            [ PDict
                [ ("title", PStr (titleOf raw))
                , ("is_preferred", maybe (PBool False) id (get "isPreferred" raw))
                , ("edits", PList converted) ] ]
    convertEdit docLs ed = do
      newText <- maybe (pyKeyError "newText") pure (get "newText" ed)
      rng <- maybe (pyKeyError "range") pure (get "range" ed)
      let r = rangeFromUtf16 docLs rng
          n k1 k2 = PInt (fromIntegral (maybe 0 id (intAt [k1, k2] r) + 1))
      pure (PDict [("new_text", newText), ("start_line", n "start" "line"), ("start_column", n "start" "character"), ("end_line", n "end" "line"), ("end_column", n "end" "character")])
