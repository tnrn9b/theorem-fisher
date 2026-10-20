-- | The tool registry and toolsets: each tool is defined once (name,
-- toolset, the JSON schema the model sees, a handler, an availability
-- check) and the registry answers what upstream's @tools.registry@,
-- @toolsets@ and @model_tools@ answer: definitions for a set of toolsets,
-- dispatch with upstream's error strings, and the agent-loop tools it
-- refuses to run itself.
module Fisher.Tools.Registry
  ( ToolCtx (..)
  , defaultCtx
  , ToolDef (..)
  , Toolset (..)
  , Registry
  , newRegistry
  , register
  , lookupTool
  , allToolNames
  , toolsetForTool
  , toolToToolsetMap
  , emojiFor
  , onOtherToolCall
  , getDefinitions
  , dispatch
  , handleFunctionCall
  , agentLoopTools
    -- * Toolsets
  , builtinToolsets
  , legacyToolsets
  , toolsets
  , createCustomToolset
  , appendToToolsets
  , resolveToolset
  , validateToolset
  , toolsetInfo
  , getToolDefinitions
  , isToolsetAvailable
  , checkToolsetRequirements
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forM_, unless)
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (typeOf)

import Fisher.Py.Value (PyVal (..), dumpsAscii, dumpsUnicode)

-- | What a handler is told besides its arguments.
data ToolCtx = ToolCtx
  { tcTaskId :: Text
  , tcUserTask :: Maybe Text
  , tcEnabledTools :: Maybe [Text]
  }

defaultCtx :: ToolCtx
defaultCtx = ToolCtx "default" Nothing Nothing

data ToolDef = ToolDef
  { tdName :: Text
  , tdToolset :: Text
  , tdSchema :: PyVal
  -- ^ @{"name", "description", "parameters"}@, exactly as the model sees it
  , tdHandler :: ToolCtx -> PyVal -> IO Text
  , tdCheck :: Maybe (IO Bool)
  , tdRequiresEnv :: [Text]
  , tdEmoji :: Text
  }

data Toolset = Toolset
  { tsDescription :: Text
  , tsTools :: [Text]
  , tsIncludes :: [Text]
  }
  deriving (Eq, Show)

data Registry = Registry
  { rTools :: IORef (M.Map Text ToolDef)
  , rOrder :: IORef [Text]
  , rToolsetChecks :: IORef (M.Map Text (IO Bool))
  , rToolsets :: IORef [(Text, Toolset)]
  , rOther :: IORef [Text -> IO ()]
  }

newRegistry :: IO Registry
newRegistry = Registry <$> newIORef M.empty <*> newIORef [] <*> newIORef M.empty <*> newIORef builtinToolsets <*> newIORef []

-- | @registry.register(...)@: a later registration of a name replaces the
-- earlier one in place; a toolset's first check is its availability check.
register :: Registry -> ToolDef -> IO ()
register r td = do
  known <- M.member (tdName td) <$> readIORef (rTools r)
  modifyIORef' (rTools r) (M.insert (tdName td) td)
  unless known (modifyIORef' (rOrder r) (<> [tdName td]))
  forM_ (tdCheck td) $ \c -> modifyIORef' (rToolsetChecks r) (\m -> if M.member (tdToolset td) m then m else M.insert (tdToolset td) c m)

lookupTool :: Registry -> Text -> IO (Maybe ToolDef)
lookupTool r n = M.lookup n <$> readIORef (rTools r)

allToolNames :: Registry -> IO [Text]
allToolNames r = M.keys <$> readIORef (rTools r)

toolsetForTool :: Registry -> Text -> IO (Maybe Text)
toolsetForTool r n = fmap tdToolset <$> lookupTool r n

toolToToolsetMap :: Registry -> IO [(Text, Text)]
toolToToolsetMap r = do
  order <- readIORef (rOrder r)
  tools <- readIORef (rTools r)
  pure [(n, tdToolset td) | n <- order, Just td <- [M.lookup n tools]]

emojiFor :: Registry -> Text -> IO Text
emojiFor r n = maybe "\9889" (\td -> if T.null (tdEmoji td) then "\9889" else tdEmoji td) <$> lookupTool r n

-- | Called with the tool name whenever a tool other than read_file and
-- search_files is dispatched (the file tools' read-loop counter resets).
onOtherToolCall :: Registry -> (Text -> IO ()) -> IO ()
onOtherToolCall r f = modifyIORef' (rOther r) (<> [f])

checkOk :: ToolDef -> IO Bool
checkOk td = case tdCheck td of
  Nothing -> pure True
  Just c -> either (const False) id <$> (try c :: IO (Either SomeException Bool))

-- | @get_definitions(names)@: sorted by name, unknown names skipped, tools
-- whose check fails hidden.
getDefinitions :: Registry -> S.Set Text -> IO [PyVal]
getDefinitions r names = do
  tools <- readIORef (rTools r)
  fmap concat . forM (S.toAscList names) $ \n -> case M.lookup n tools of
    Nothing -> pure []
    Just td -> do
      ok <- checkOk td
      pure [PDict [("type", PStr "function"), ("function", tdSchema td)] | ok]

-- | @registry.dispatch(name, args)@.
dispatch :: Registry -> ToolCtx -> Text -> PyVal -> IO Text
dispatch r ctx name args = do
  mt <- lookupTool r name
  case mt of
    Nothing -> pure (dumpsAscii (PDict [("error", PStr ("Unknown tool: " <> name))]))
    Just td -> do
      res <- try (tdHandler td ctx args)
      pure $ case res of
        Right t -> t
        Left (e :: SomeException) -> dumpsAscii (PDict [("error", PStr ("Tool execution failed: " <> T.pack (show (typeOf e)) <> ": " <> T.pack (displayException e)))])

-- | Tools the agent loop runs itself (todo, memory, session_search,
-- delegate_task); dispatching them here is an error.
agentLoopTools :: [Text]
agentLoopTools = ["todo", "memory", "session_search", "delegate_task"]

-- | @handle_function_call(name, args, task_id, ...)@.
handleFunctionCall :: Registry -> ToolCtx -> Text -> PyVal -> IO Text
handleFunctionCall r ctx name args = do
  unless (name `elem` ["read_file", "search_files"]) $ do
    hooks <- readIORef (rOther r)
    forM_ hooks ($ tcTaskId ctx)
  res <- try $
    if name `elem` agentLoopTools
      then pure (dumpsAscii (PDict [("error", PStr (name <> " must be handled by the agent loop"))]))
      else dispatch r ctx name args
  pure $ case res of
    Right t -> t
    Left (e :: SomeException) -> dumpsUnicode (PDict [("error", PStr ("Error executing " <> name <> ": " <> T.pack (displayException e)))])

-- ---------------------------------------------------------------- toolsets

coreTools :: [Text]
coreTools =
  [ "read_file", "write_file", "patch", "search_files", "web_search"
  , "browser_navigate", "browser_snapshot", "browser_click", "browser_type", "browser_scroll", "browser_back"
  , "browser_press", "browser_close", "browser_get_images", "browser_vision"
  ]

-- | @TOOLSETS@ at the pinned commit, in its order, with upstream's
-- platform toolsets under Fisher's @fisher-*@ names.
builtinToolsets :: [(Text, Toolset)]
builtinToolsets =
  [ ("web", Toolset "Web research tools" ["web_search"] [])
  , ("search", Toolset "Web search only (no content extraction/scraping)" ["web_search"] [])
  , ( "browser"
    , Toolset
        "Browser automation for web interaction (navigate, click, type, scroll, iframes, hold-click) with web search for finding URLs"
        [ "browser_navigate", "browser_snapshot", "browser_click", "browser_type", "browser_scroll", "browser_back"
        , "browser_press", "browser_close", "browser_get_images", "browser_vision", "web_search"
        ]
        []
    )
  , ("file", Toolset "File manipulation tools: read, write, patch (with fuzzy matching), and search (content + files)" ["read_file", "write_file", "patch", "search_files"] [])
  , ("autoformalize", Toolset "Minimal Fisher workflow for papers, source files, and browser-driven research" [] ["web", "file", "browser"])
  , ("fisher-acp", Toolset "Editor integration (VS Code, Zed, JetBrains) \8212 minimal file, web, and browser tools for Fisher work" coreTools [])
  , ("fisher-cli", Toolset "Minimal interactive Fisher toolset for autoformalization workflows" coreTools [])
  , ("fisher-telegram", Toolset "Telegram bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ("fisher-discord", Toolset "Discord bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ("fisher-whatsapp", Toolset "WhatsApp bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ("fisher-slack", Toolset "Slack bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ("fisher-signal", Toolset "Signal bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ("fisher-homeassistant", Toolset "Home Assistant bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ("fisher-email", Toolset "Email bot toolset - minimal Fisher file, web, and browser access" coreTools [])
  , ( "fisher-gateway"
    , Toolset
        "Gateway toolset - union of the minimal messaging-platform tool surfaces"
        []
        ["fisher-telegram", "fisher-discord", "fisher-whatsapp", "fisher-slack", "fisher-signal", "fisher-homeassistant", "fisher-email"]
    )
  ]

-- | @_LEGACY_TOOLSET_MAP@.
legacyToolsets :: [(Text, [Text])]
legacyToolsets =
  [ ("web_tools", ["web_search"])
  , ( "browser_tools"
    , ["browser_navigate", "browser_snapshot", "browser_click", "browser_type", "browser_scroll", "browser_back", "browser_press", "browser_close", "browser_get_images", "browser_vision"]
    )
  , ("file_tools", ["read_file", "write_file", "patch", "search_files"])
  ]

toolsets :: Registry -> IO [(Text, Toolset)]
toolsets = readIORef . rToolsets

-- | @create_custom_toolset@ (a new name goes last; an existing one is
-- replaced where it stands).
createCustomToolset :: Registry -> Text -> Toolset -> IO ()
createCustomToolset r name ts = modifyIORef' (rToolsets r) $ \tss ->
  if any ((== name) . fst) tss then [(n, if n == name then ts else t) | (n, t) <- tss] else tss <> [(name, ts)]

-- | Append tool names to every toolset whose name starts with a prefix
-- (MCP adds its tools to each @fisher-*@ toolset).
appendToToolsets :: Registry -> Text -> [Text] -> IO ()
appendToToolsets r prefix names = modifyIORef' (rToolsets r) . map $ \(n, ts) ->
  if prefix `T.isPrefixOf` n then (n, ts {tsTools = foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) (tsTools ts) names}) else (n, ts)

-- | @resolve_toolset(name)@ (a set upstream; sorted here). Also returns the
-- cycle warnings it would print.
resolveToolset :: Registry -> Text -> IO [Text]
resolveToolset r name = do
  tss <- readIORef (rToolsets r)
  let go visited n
        | n `elem` ["all", "*"] = S.unions [go visited t | (t, _) <- tss]
        | n `S.member` visited = S.empty
        | otherwise = case lookup n tss of
            Nothing -> S.empty
            Just ts -> S.unions (S.fromList (tsTools ts) : [go (S.insert n visited) i | i <- tsIncludes ts])
  pure (S.toAscList (go S.empty name))

validateToolset :: Registry -> Text -> IO Bool
validateToolset r name
  | name `elem` ["all", "*"] = pure True
  | otherwise = any ((== name) . fst) <$> readIORef (rToolsets r)

-- | @get_toolset_info(name)@.
toolsetInfo :: Registry -> Text -> IO (Maybe PyVal)
toolsetInfo r name = do
  tss <- readIORef (rToolsets r)
  case lookup name tss of
    Nothing -> pure Nothing
    Just ts -> do
      resolved <- resolveToolset r name
      pure . Just $
        PDict
          [ ("name", PStr name)
          , ("description", PStr (tsDescription ts))
          , ("direct_tools", PList (map PStr (tsTools ts)))
          , ("includes", PList (map PStr (tsIncludes ts)))
          , ("resolved_tools", PList (map PStr resolved))
          , ("tool_count", PInt (fromIntegral (length resolved)))
          , ("is_composite", PBool (not (null (tsIncludes ts))))
          ]

-- | @get_tool_definitions(enabled_toolsets, disabled_toolsets, quiet)@:
-- the definitions and the lines upstream prints when not quiet.
getToolDefinitions :: Registry -> Maybe [Text] -> Maybe [Text] -> IO ([PyVal], [Text])
getToolDefinitions r enabled disabled = do
  tss0 <- readIORef (rToolsets r)
  let allNames = map fst tss0
  let listing xs = if null xs then "no tools" else T.intercalate ", " xs
  (include, msgs) <- case (enabled, disabled) of
    (Just en@(_ : _), _) -> do
      parts <- forM en $ \n -> do
        valid <- validateToolset r n
        if valid
          then do
            res <- resolveToolset r n
            pure (res, "\9989 Enabled toolset '" <> n <> "': " <> listing res)
          else case lookup n legacyToolsets of
            Just ls -> pure (ls, "\9989 Enabled legacy toolset '" <> n <> "': " <> T.intercalate ", " ls)
            Nothing -> pure ([], "\9888\65039  Unknown toolset: " <> n)
      pure (S.fromList (concatMap fst parts), map snd parts)
    (_, Just dis@(_ : _)) -> do
      base <- S.fromList . concat <$> mapM (resolveToolset r) allNames
      foldl
        ( \acc n -> do
            (s, ms) <- acc
            valid <- validateToolset r n
            if valid
              then do
                res <- resolveToolset r n
                pure (s `S.difference` S.fromList res, ms <> ["\128683 Disabled toolset '" <> n <> "': " <> listing res])
              else case lookup n legacyToolsets of
                Just ls -> pure (s `S.difference` S.fromList ls, ms <> ["\128683 Disabled legacy toolset '" <> n <> "': " <> T.intercalate ", " ls])
                Nothing -> pure (s, ms <> ["\9888\65039  Unknown toolset: " <> n])
        )
        (pure (base, []))
        dis
    _ -> do
      base <- S.fromList . concat <$> mapM (resolveToolset r) allNames
      pure (base, [])
  defs <- getDefinitions r include
  let names = mapMaybe defName defs
      final =
        if null defs
          then "\128736\65039  No tools selected (all filtered out or unavailable)"
          else "\128736\65039  Final tool selection (" <> T.pack (show (length defs)) <> " tools): " <> T.intercalate ", " names
  pure (defs, msgs <> [final])
  where
    defName = \case
      PDict kvs | Just (PDict f) <- lookup "function" kvs, Just (PStr n) <- lookup "name" f -> Just n
      _ -> Nothing

isToolsetAvailable :: Registry -> Text -> IO Bool
isToolsetAvailable r ts = do
  checks <- readIORef (rToolsetChecks r)
  case M.lookup ts checks of
    Nothing -> pure True
    Just c -> either (const False) id <$> (try c :: IO (Either SomeException Bool))

-- | @check_toolset_requirements()@: every registered tool's toolset, sorted.
checkToolsetRequirements :: Registry -> IO [(Text, Bool)]
checkToolsetRequirements r = do
  tools <- M.elems <$> readIORef (rTools r)
  let tss = sort (S.toList (S.fromList (map tdToolset tools)))
  forM tss $ \t -> (,) t <$> isToolsetAvailable r t

