-- | The four file tools the agent sees (read_file, write_file, patch,
-- search_files), as @tools.file_tools@ defines them: argument defaults,
-- the read/search loop guard per task, secret redaction of what is read,
-- and the hints appended to some results.
module Fisher.Tools.File
  ( FileTools (..)
  , newFileTools
  , registerFileTools
  , readFileTool
  , writeFileTool
  , patchTool
  , searchTool
  , notifyOtherToolCall
  , readFilesSummary
  , clearReadTracker
  , setContextPaths
  ) where

import Control.Exception (Exception, SomeException, displayException, fromException, throwIO, try)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Value (PyVal (..), dumpsAscii, dumpsUnicode, get, pyStrOf, truthy)
import Fisher.Tools.File.Schemas
import Fisher.Tools.FileOps
import Fisher.Tools.Py (argInt, pyTypeName)
import Fisher.Tools.Redact (redactEnabled, redactSensitiveText)
import Fisher.Tools.Registry
import Fisher.Tools.V4a (applyV4a, parseV4a)

data Tracker = Tracker
  { tLastKey :: Maybe [PyVal]
  , tConsecutive :: Int
  , tHistory :: S.Set (Text, Integer, Integer)
  }

data FileTools = FileTools
  { ftOps :: Text -> IO (Either Text FileOps)
  -- ^ the task's file operations (its sandbox created on first use)
  , ftRedactSetting :: IO (Maybe Text)
  -- ^ @FISHER_REDACT_SECRETS@
  , ftCheck :: IO Bool
  -- ^ @check_file_requirements@
  , ftTracker :: IORef (M.Map Text Tracker)
  , ftContext :: IORef (S.Set Text)
  -- ^ Fisher only: CLM's context file and its backup, which the loop guard
  -- leaves alone and @patch@ matches exactly
  }

newFileTools :: (Text -> IO (Either Text FileOps)) -> IO (Maybe Text) -> IO Bool -> IO FileTools
newFileTools ops redact check = FileTools ops redact check <$> newIORef M.empty <*> newIORef S.empty

-- | The paths of CLM's context file and backup (docs/clm.md section 3).
setContextPaths :: FileTools -> [Text] -> IO ()
setContextPaths ft ps = writeIORef (ftContext ft) (S.fromList ps)

isContextPath :: FileTools -> PyVal -> IO Bool
isContextPath ft = \case
  PStr p -> S.member p <$> readIORef (ftContext ft)
  _ -> pure False

newtype PyFail = PyFail Text
  deriving (Show)

instance Exception PyFail

failWith :: Text -> IO a
failWith = throwIO . PyFail

-- | Run a tool body; any failure becomes @{"error": str(e)}@.
guarded :: IO Text -> IO Text
guarded body = do
  r <- try body
  pure $ case r of
    Right t -> t
    Left (e :: SomeException) -> dumpsUnicode (PDict [("error", PStr (message e))])
  where
    message e = case fromException e of
      Just (PyFail m) -> m
      Nothing -> T.pack (displayException e)

opsFor :: FileTools -> Text -> IO FileOps
opsFor ft tid = ftOps ft tid >>= either failWith pure

redactOn :: FileTools -> IO Bool
redactOn ft = redactEnabled <$> ftRedactSetting ft

-- | Python's view of a path argument before it reaches the shell.
pathArg :: PyVal -> IO Text
pathArg = \case
  PStr s -> pure s
  v
    | truthy v -> failWith ("'" <> pyTypeName v <> "' object has no attribute 'startswith'")
    | otherwise -> failWith ("'" <> pyTypeName v <> "' object has no attribute 'replace'")

intArg :: PyVal -> IO Integer
intArg v = maybe (failWith ("'<' not supported between instances of 'int' and '" <> pyTypeName v <> "'")) pure (argInt v)

strArg :: PyVal -> IO Text
strArg = \case
  PStr s -> pure s
  v -> failWith ("must be str, not " <> pyTypeName v)

bump :: FileTools -> Text -> [PyVal] -> Maybe (Text, Integer, Integer) -> IO Int
bump ft tid key hist = atomicModifyIORef' (ftTracker ft) $ \m ->
  let t0 = M.findWithDefault (Tracker Nothing 0 S.empty) tid m
      t1 = t0 {tHistory = maybe (tHistory t0) (`S.insert` tHistory t0) hist}
      t2 = if tLastKey t1 == Just key then t1 {tConsecutive = tConsecutive t1 + 1} else t1 {tLastKey = Just key, tConsecutive = 1}
   in (M.insert tid t2 m, tConsecutive t2)

-- | @read_file_tool(path, offset, limit, task_id)@.
readFileTool :: FileTools -> Text -> PyVal -> PyVal -> PyVal -> IO Text
readFileTool ft tid pathV offsetV limitV = guarded $ do
  ops <- opsFor ft tid
  path <- pathArg pathV
  offset <- intArg offsetV
  limit <- intArg limitV
  r0 <- readFileOp ops path offset limit
  red <- redactOn ft
  let r = if red && not (T.null (rrContent r0)) then r0 {rrContent = redactSensitiveText (rrContent r0)} else r0
  ctxFile <- isContextPath ft pathV
  count <- if ctxFile then pure 0 else bump ft tid [PStr "read", pathV, offsetV, limitV] (Just (path, offset, limit))
  pure $
    if count >= 4
      then
        dumpsUnicode $
          PDict
            [ ("error", PStr ("BLOCKED: You have read this exact file region " <> tshow count <> " times in a row. The content has NOT changed. You already have this information. STOP re-reading and proceed with your task."))
            , ("path", pathV)
            , ("already_read", PInt (fromIntegral count))
            ]
      else
        dumpsUnicode . PDict $
          readResultDict r
            <> [ ( "_warning"
                 , PStr ("You have read this exact file region " <> tshow count <> " times consecutively. The content has not changed since your last read. Use the information you already have. If you are stuck in a loop, stop reading and proceed with writing or responding.")
                 )
               | count >= 3
               ]

-- | @write_file_tool(path, content, task_id)@.
writeFileTool :: FileTools -> Text -> PyVal -> PyVal -> IO Text
writeFileTool ft tid pathV contentV = guarded $ do
  ops <- opsFor ft tid
  path <- pathArg pathV
  let content = case contentV of
        PStr s -> s
        v -> pyStrOf v
  w <- writeFileOp ops path content
  pure (dumpsUnicode (PDict (writeResultDict w)))

-- | @patch_tool(mode, path, old_string, new_string, replace_all, patch, task_id)@.
patchTool :: FileTools -> Text -> PyVal -> PyVal -> PyVal -> PyVal -> PyVal -> PyVal -> IO Text
patchTool ft tid modeV pathV oldV newV replaceAllV patchV = guarded $ do
  ops <- opsFor ft tid
  res <- case modeV of
    PStr "replace" ->
      if not (truthy pathV)
        then pure (Left "path required")
        else
          if oldV == PNull || newV == PNull
            then pure (Left "old_string and new_string required")
            else do
              path <- pathArg pathV
              old <- strArg oldV
              new <- strArg newV
              ctxFile <- isContextPath ft pathV
              Right <$> (if ctxFile then patchReplaceExact else patchReplace) ops path old new (truthy replaceAllV)
    PStr "patch" ->
      if not (truthy patchV)
        then pure (Left "patch content required")
        else do
          p <- strArg patchV
          Right <$> applyV4a ops (parseV4a p)
    other -> pure (Left ("Unknown mode: " <> pyStrOf other))
  pure $ case res of
    Left e -> dumpsAscii (PDict [("error", PStr e)])
    Right pr ->
      let j = dumpsUnicode (PDict (patchResultDict pr))
       in case prError pr of
            Just e | "Could not find" `T.isInfixOf` e -> j <> "\n\n[Hint: old_string not found. Use read_file to verify the current content, or search_files to locate the text.]"
            _ -> j

-- | @search_tool(pattern, target, path, file_glob, limit, offset, output_mode, context, task_id)@.
searchTool :: FileTools -> Text -> PyVal -> Text -> PyVal -> PyVal -> PyVal -> PyVal -> PyVal -> PyVal -> IO Text
searchTool ft tid patternV target pathV globV limitV offsetV modeV contextV = guarded $ do
  let globKey = if truthy globV then globV else PStr ""
  count <- bump ft tid [PStr "search", patternV, PStr target, PStr (pyStrOf pathV), globKey] Nothing
  if count >= 4
    then
      pure . dumpsUnicode $
        PDict
          [ ("error", PStr ("BLOCKED: You have run this exact search " <> tshow count <> " times in a row. The results have NOT changed. You already have this information. STOP re-searching and proceed with your task."))
          , ("pattern", patternV)
          , ("already_searched", PInt (fromIntegral count))
          ]
    else do
      ops <- opsFor ft tid
      pattern <- strArg patternV
      path <- pathArg pathV
      limit <- intArg limitV
      offset <- intArg offsetV
      context <- intArg contextV
      glob <- case globV of
        PStr g -> pure (Just g)
        PNull -> pure Nothing
        v -> Just <$> strArg v
      let mode = pyStrOf modeV
      r0 <- searchOp ops pattern path target glob limit offset mode context
      red <- redactOn ft
      let r = if red then r0 {srMatches = [m {smContent = if T.null (smContent m) then smContent m else redactSensitiveText (smContent m)} | m <- srMatches r0]} else r0
          warn = [("_warning", PStr ("You have run this exact search " <> tshow count <> " times consecutively. The results have not changed. Use the information you already have.")) | count >= 3]
          j = dumpsUnicode (PDict (searchResultDict r <> warn))
      pure $
        if srTruncated r
          then j <> "\n\n[Hint: Results truncated. Use offset=" <> tshow (offset + limit) <> " to see more, or narrow with a more specific pattern or file_glob.]"
          else j

-- | @notify_other_tool_call(task_id)@.
notifyOtherToolCall :: FileTools -> Text -> IO ()
notifyOtherToolCall ft tid = modifyIORef' (ftTracker ft) (M.adjust (\t -> t {tLastKey = Nothing, tConsecutive = 0}) tid)

-- | @get_read_files_summary(task_id)@.
readFilesSummary :: FileTools -> Text -> IO PyVal
readFilesSummary ft tid = do
  m <- readIORef (ftTracker ft)
  let hist = maybe [] (S.toList . tHistory) (M.lookup tid m)
      byPath = M.fromListWith (flip (<>)) [(p, [region o l]) | (p, o, l) <- hist]
      region o l = PStr ("lines " <> tshow o <> "-" <> tshow (o + l - 1))
  pure (PList [PDict [("path", PStr p), ("regions", PList rs)] | (p, rs) <- M.toAscList byPath])

clearReadTracker :: FileTools -> Maybe Text -> IO ()
clearReadTracker ft = \case
  Just tid -> modifyIORef' (ftTracker ft) (M.delete tid)
  Nothing -> writeIORef (ftTracker ft) M.empty

arg :: Text -> PyVal -> PyVal -> PyVal
arg k d args = maybe d id (get k args)

-- | Register the four tools (toolset "file") and hook the loop guard's
-- reset into the registry's dispatcher.
registerFileTools :: Registry -> FileTools -> IO ()
registerFileTools r ft = do
  let def name schema emoji h = register r (ToolDef name "file" schema h (Just (ftCheck ft)) [] emoji)
  def "read_file" readFileSchema "\128214" $ \ctx a ->
    readFileTool ft (tcTaskId ctx) (arg "path" (PStr "") a) (arg "offset" (PInt 1) a) (arg "limit" (PInt 500) a)
  def "write_file" writeFileSchema "\9997\65039" $ \ctx a ->
    writeFileTool ft (tcTaskId ctx) (arg "path" (PStr "") a) (arg "content" (PStr "") a)
  def "patch" patchSchema "\128295" $ \ctx a ->
    patchTool ft (tcTaskId ctx) (arg "mode" (PStr "replace") a) (arg "path" PNull a) (arg "old_string" PNull a) (arg "new_string" PNull a) (arg "replace_all" (PBool False) a) (arg "patch" PNull a)
  def "search_files" searchFilesSchema "\128270" $ \ctx a ->
    let rawTarget = arg "target" (PStr "content") a
        target = case rawTarget of
          PStr "grep" -> "content"
          PStr "find" -> "files"
          v -> pyStrOf v
     in searchTool ft (tcTaskId ctx) (arg "pattern" (PStr "") a) target (arg "path" (PStr ".") a) (arg "file_glob" PNull a) (arg "limit" (PInt 50) a) (arg "offset" (PInt 0) a) (arg "output_mode" (PStr "content") a) (arg "context" (PInt 0) a)
  onOtherToolCall r (notifyOtherToolCall ft)

tshow :: Show a => a -> Text
tshow = T.pack . show
