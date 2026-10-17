-- | The @memory@ tool (@tools/memory_tool.py@): MEMORY.md and USER.md
-- under @$GAUSS_HOME/memories@, entries split on @\\n§\\n@, character
-- limits, the injection scan, and the snapshot frozen into the system
-- prompt when the store loads.
module Fisher.Agent.Tools.Memory
  ( MemoryStore
  , msDir
  , newMemoryStore
  , loadFromDisk
  , frozenSnapshot
  , memoryEntries
  , scanMemoryContent
  , memoryTool
  , memoryToolArgs
  , memoryToolDef
  , defaultMemoryLimits
  ) where

import Control.Exception (SomeException, displayException, throwIO, try)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)

import Fisher.Agent.Prompt (MemorySnapshot (..), commasI, memoryPct, renderMemoryBlock)
import Fisher.Agent.Tools.Todo (PyError (..))
import Fisher.Agent.Tools.Web (toolSchemas)
import qualified Fisher.Py.Re as Re
import Fisher.Py.Value
import Fisher.Tools.Py (pyStrip, pyTypeName)
import Fisher.Tools.Registry (ToolDef (..))

data MemoryStore = MemoryStore
  { msDir :: FilePath
  , msMemLimit :: Int
  , msUserLimit :: Int
  , mstMemory :: IORef [Text]
  , mstUser :: IORef [Text]
  , msSnap :: IORef MemorySnapshot
  }

defaultMemoryLimits :: (Int, Int)
defaultMemoryLimits = (2200, 1375)

-- | @MemoryStore(memory_char_limit, user_char_limit)@ over a memories
-- directory (not loaded yet).
newMemoryStore :: FilePath -> Int -> Int -> IO MemoryStore
newMemoryStore dir ml ul = MemoryStore dir ml ul <$> newIORef [] <*> newIORef [] <*> newIORef (MemorySnapshot "" "")

delim :: Text
delim = "\n§\n"

-- | @load_from_disk()@: the live entries and the frozen snapshot.
loadFromDisk :: MemoryStore -> IO ()
loadFromDisk st = do
  createDirectoryIfMissing True (msDir st)
  m <- dedupe <$> readEntries (msDir st </> "MEMORY.md")
  u <- dedupe <$> readEntries (msDir st </> "USER.md")
  writeIORef (mstMemory st) m
  writeIORef (mstUser st) u
  writeIORef (msSnap st) (MemorySnapshot (renderMemoryBlock "memory" (msMemLimit st) m) (renderMemoryBlock "user" (msUserLimit st) u))
  where
    dedupe = foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) []

-- | @_read_file(path)@ (text mode: universal newlines; bad UTF-8 raises).
readEntries :: FilePath -> IO [Text]
readEntries p = do
  e <- doesFileExist p
  if not e
    then pure []
    else do
      r <- try @SomeException (BS.readFile p)
      case r of
        Left _ -> pure []
        Right b -> case TE.decodeUtf8' b of
          Left err -> throwIO (PyError "UnicodeDecodeError" (T.pack (displayException err)))
          Right raw0 -> do
            let raw = T.replace "\r" "\n" (T.replace "\r\n" "\n" raw0)
            pure $
              if T.null (pyStrip raw)
                then []
                else [x | x <- map pyStrip (T.splitOn delim raw), not (T.null x)]

-- | @_write_file(path, entries)@: a temp file in the same directory,
-- renamed over the old one.
writeEntries :: FilePath -> FilePath -> [Text] -> IO ()
writeEntries dir path es = do
  r <- try @SomeException $ do
    (tmp, h) <- openBinaryTempFile dir ".mem_.tmp"
    w <- try @SomeException (BS.hPut h (TE.encodeUtf8 (T.intercalate delim es)) >> hClose h >> renameFile tmp path)
    case w of
      Right () -> pure ()
      Left e -> do
        _ <- try @SomeException (hClose h)
        _ <- try @SomeException (removeFile tmp)
        throwIO e
  case r of
    Right () -> pure ()
    Left e -> throwIO (PyError "RuntimeError" ("Failed to write memory file " <> T.pack path <> ": " <> T.pack (displayException e)))

-- | The frozen snapshot (@format_for_system_prompt@ for both targets).
frozenSnapshot :: MemoryStore -> IO MemorySnapshot
frozenSnapshot = readIORef . msSnap

-- | The live entries of a target.
memoryEntries :: MemoryStore -> Text -> IO [Text]
memoryEntries st t = readIORef (ref st t)

ref :: MemoryStore -> Text -> IORef [Text]
ref st t = if t == "user" then mstUser st else mstMemory st

limitOf :: MemoryStore -> Text -> Int
limitOf st t = if t == "user" then msUserLimit st else msMemLimit st

saveToDisk :: MemoryStore -> Text -> IO ()
saveToDisk st t = do
  createDirectoryIfMissing True (msDir st)
  es <- readIORef (ref st t)
  case t of
    "memory" -> writeEntries (msDir st) (msDir st </> "MEMORY.md") es
    "user" -> writeEntries (msDir st) (msDir st </> "USER.md") es
    _ -> pure ()

-- ---------------------------------------------------------------- scan

threats :: [(Re.Regex, Text)]
threats =
  [ (rx "ignore\\s+(previous|all|above|prior)\\s+instructions", "prompt_injection")
  , (rx "you\\s+are\\s+now\\s+", "role_hijack")
  , (rx "do\\s+not\\s+tell\\s+the\\s+user", "deception_hide")
  , (rx "system\\s+prompt\\s+override", "sys_prompt_override")
  , (rx "disregard\\s+(your|all|any)\\s+(instructions|rules|guidelines)", "disregard_rules")
  , (rx "act\\s+as\\s+(if|though)\\s+you\\s+(have\\s+no|don't\\s+have)\\s+(restrictions|limits|rules)", "bypass_restrictions")
  , (rx "curl\\s+[^\\n]*\\$\\{?\\w*(KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL|API)", "exfil_curl")
  , (rx "wget\\s+[^\\n]*\\$\\{?\\w*(KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL|API)", "exfil_wget")
  , (rx "cat\\s+[^\\n]*(\\.env|credentials|\\.netrc|\\.pgpass|\\.npmrc|\\.pypirc)", "read_secrets")
  , (rx "authorized_keys", "ssh_backdoor")
  , (rx "\\$HOME/\\.ssh|~/\\.ssh", "ssh_access")
  , (rx "\\$HOME/\\.gauss/\\.env|~/\\.gauss/\\.env", "gauss_env")
  ]
  where
    rx = Re.compileOrDie [Re.IgnoreCase]

-- | Upstream iterates a set (its order follows string hashing, which
-- varies by process); Fisher reports the first in code-point order.
invisible :: [Char]
invisible = sort "\x200b\x200c\x200d\x2060\xfeff\x202a\x202b\x202c\x202d\x202e"

-- | @_scan_memory_content(content)@
scanMemoryContent :: Text -> Maybe Text
scanMemoryContent c =
  case [ch | ch <- invisible, T.singleton ch `T.isInfixOf` c] of
    (ch : _) -> Just ("Blocked: content contains invisible unicode character U+" <> hex4 ch <> " (possible injection).")
    [] -> case [pid | (r, pid) <- threats, isJust (Re.search r c)] of
      (pid : _) -> Just ("Blocked: content matches threat pattern '" <> pid <> "'. Memory entries are injected into the system prompt and must not contain injection or exfiltration payloads.")
      [] -> Nothing
  where
    hex4 ch = T.justifyRight 4 '0' (T.toUpper (T.pack (showHex (fromEnum ch) "")))

-- ---------------------------------------------------------------- operations

failure :: Text -> PyVal
failure e = PDict [("success", PBool False), ("error", PStr e)]

charCount :: [Text] -> Int
charCount [] = 0
charCount es = T.length (T.intercalate delim es)

successResponse :: MemoryStore -> Text -> Text -> IO PyVal
successResponse st t msg = do
  es <- readIORef (ref st t)
  let cur = charCount es
      lim = limitOf st t
  pure $
    PDict
      [ ("success", PBool True), ("target", PStr t), ("entries", PList (map PStr es))
      , ("usage", PStr (tshow (memoryPct cur lim) <> "% — " <> commasI cur <> "/" <> commasI lim <> " chars"))
      , ("entry_count", PInt (fromIntegral (length es))), ("message", PStr msg) ]

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @.strip()@ on an argument: only a str has one.
stripArg :: PyVal -> IO Text
stripArg = \case
  PStr s -> pure (pyStrip s)
  v -> throwIO (PyError "AttributeError" ("'" <> pyTypeName v <> "' object has no attribute 'strip'"))

addEntry :: MemoryStore -> Text -> PyVal -> IO PyVal
addEntry st t contentV = do
  content <- stripArg contentV
  if T.null content
    then pure (failure "Content cannot be empty.")
    else case scanMemoryContent content of
      Just e -> pure (failure e)
      Nothing -> do
        es <- readIORef (ref st t)
        let lim = limitOf st t
        if content `elem` es
          then successResponse st t "Entry already exists (no duplicate added)."
          else
            if charCount (es <> [content]) > lim
              then do
                let cur = charCount es
                pure $
                  PDict
                    [ ("success", PBool False)
                    , ("error", PStr ("Memory at " <> commasI cur <> "/" <> commasI lim <> " chars. Adding this entry (" <> tshow (T.length content) <> " chars) would exceed the limit. Replace or remove existing entries first."))
                    , ("current_entries", PList (map PStr es)), ("usage", PStr (commasI cur <> "/" <> commasI lim)) ]
              else do
                writeIORef (ref st t) (es <> [content])
                saveToDisk st t
                successResponse st t "Entry added."

-- | The entry @old_text@ picks out: its index, or the error.
matchOne :: [Text] -> Text -> Either PyVal Int
matchOne es old =
  case [(i, e) | (i, e) <- zip [0 ..] es, old `T.isInfixOf` e] of
    [] -> Left (failure ("No entry matched '" <> old <> "'."))
    ms@((i, _) : _)
      | length (foldl (\acc (_, e) -> if e `elem` acc then acc else acc <> [e]) [] ms) > 1 ->
          Left
            ( PDict
                [ ("success", PBool False), ("error", PStr ("Multiple entries matched '" <> old <> "'. Be more specific."))
                , ("matches", PList [PStr (T.take 80 e <> (if T.length e > 80 then "..." else "")) | (_, e) <- ms]) ] )
      | otherwise -> Right i

replaceEntry :: MemoryStore -> Text -> PyVal -> PyVal -> IO PyVal
replaceEntry st t oldV newV = do
  old <- stripArg oldV
  new <- stripArg newV
  if T.null old
    then pure (failure "old_text cannot be empty.")
    else
      if T.null new
        then pure (failure "new_content cannot be empty. Use 'remove' to delete entries.")
        else case scanMemoryContent new of
          Just e -> pure (failure e)
          Nothing -> do
            es <- readIORef (ref st t)
            case matchOne es old of
              Left r -> pure r
              Right i -> do
                let es' = [if j == i then new else e | (j, e) <- zip [0 ..] es]
                    total = charCount es'
                    lim = limitOf st t
                if total > lim
                  then pure (failure ("Replacement would put memory at " <> commasI total <> "/" <> commasI lim <> " chars. Shorten the new content or remove other entries first."))
                  else do
                    writeIORef (ref st t) es'
                    saveToDisk st t
                    successResponse st t "Entry replaced."

removeEntry :: MemoryStore -> Text -> PyVal -> IO PyVal
removeEntry st t oldV = do
  old <- stripArg oldV
  if T.null old
    then pure (failure "old_text cannot be empty.")
    else do
      es <- readIORef (ref st t)
      case matchOne es old of
        Left r -> pure r
        Right i -> do
          writeIORef (ref st t) [e | (j, e) <- zip [0 ..] es, j /= i]
          saveToDisk st t
          successResponse st t "Entry removed."

-- | @memory_tool(action, target, content, old_text, store)@
memoryTool :: Maybe MemoryStore -> PyVal -> PyVal -> PyVal -> PyVal -> IO Text
memoryTool Nothing _ _ _ _ = pure (dumpsUnicode (failure "Memory is not available. It may be disabled in config or this environment."))
memoryTool (Just st) action targetV content oldText = dumpsUnicode <$> go
  where
    go = case targetV of
      PStr t | t `elem` ["memory", "user"] -> case action of
        PStr "add"
          | not (truthy content) -> pure (failure "Content is required for 'add' action.")
          | otherwise -> addEntry st t content
        PStr "replace"
          | not (truthy oldText) -> pure (failure "old_text is required for 'replace' action.")
          | not (truthy content) -> pure (failure "content is required for 'replace' action.")
          | otherwise -> replaceEntry st t oldText content
        PStr "remove"
          | not (truthy oldText) -> pure (failure "old_text is required for 'remove' action.")
          | otherwise -> removeEntry st t oldText
        a -> pure (failure ("Unknown action '" <> pyStrOf a <> "'. Use: add, replace, remove"))
      t -> pure (failure ("Invalid target '" <> pyStrOf t <> "'. Use 'memory' or 'user'."))

-- | The tool from call arguments. The registry's handler defaults a
-- missing action to @""@; the agent loop's call passes @None@.
memoryToolArgs :: Bool -> Maybe MemoryStore -> PyVal -> IO Text
memoryToolArgs viaRegistry st args =
  memoryTool st (maybe (if viaRegistry then PStr "" else PNull) id (get "action" args)) (maybe (PStr "memory") id (get "target" args))
    (maybe PNull id (get "content" args)) (maybe PNull id (get "old_text" args))

-- | The registry entry; the loop runs the tool itself.
memoryToolDef :: ToolDef
memoryToolDef =
  ToolDef
    { tdName = "memory", tdToolset = "memory", tdSchema = maybe (PDict []) id (get "memory" toolSchemas >>= get "schema")
    , tdHandler = \_ args -> memoryToolArgs True Nothing args
    , tdCheck = Just (pure True), tdRequiresEnv = [], tdEmoji = "\129504" }
