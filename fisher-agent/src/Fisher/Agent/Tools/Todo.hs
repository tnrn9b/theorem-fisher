-- | The @todo@ tool (@tools/todo_tool.py@): the agent's in-memory task
-- list, the tool that reads and writes it, the post-compression
-- injection, and @AIAgent._hydrate_todo_store@, which rebuilds the list
-- from the latest todo result in a conversation's history.
module Fisher.Agent.Tools.Todo
  ( TodoItem (..)
  , TodoStore
  , newTodoStore
  , todoWrite
  , todoRead
  , todoHasItems
  , formatForInjection
  , todoTool
  , todoToolArgs
  , hydrateTodo
  , todoToolDef
  , PyError (..)
  ) where

import Control.Exception (Exception (..), throwIO)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Agent.Tools.Web (toolSchemas)
import Fisher.Py.Value
import Fisher.Tools.Py (pyStrip, pyTypeName)
import Fisher.Tools.Registry (ToolDef (..))

-- | A Python exception upstream would raise: its class and message.
data PyError = PyError Text Text
  deriving (Show)

instance Exception PyError where
  displayException (PyError c m) = T.unpack (c <> ": " <> m)

data TodoItem = TodoItem
  { tiId :: Text
  , tiContent :: Text
  , tiStatus :: Text
  }
  deriving (Eq, Show)

newtype TodoStore = TodoStore (IORef [TodoItem])

newTodoStore :: IO TodoStore
newTodoStore = TodoStore <$> newIORef []

validStatuses :: [Text]
validStatuses = ["pending", "in_progress", "completed", "cancelled"]

-- | @item.get(k, d)@ on what the model sent: anything but a dict raises.
itemGet :: PyVal -> Text -> PyVal -> IO PyVal
itemGet item k d = case item of
  PDict kvs -> pure (maybe d id (lookup k kvs))
  v -> throwIO (PyError "AttributeError" ("'" <> pyTypeName v <> "' object has no attribute 'get'"))

-- | @TodoStore._validate(item)@
validate :: PyVal -> IO TodoItem
validate item = do
  i <- pyStrip . pyStrOf <$> itemGet item "id" (PStr "")
  c <- pyStrip . pyStrOf <$> itemGet item "content" (PStr "")
  s <- T.toLower . pyStrip . pyStrOf <$> itemGet item "status" (PStr "pending")
  pure
    TodoItem
      { tiId = if T.null i then "?" else i
      , tiContent = if T.null c then "(no description)" else c
      , tiStatus = if s `elem` validStatuses then s else "pending" }

-- | Iterating the @todos@ argument as Python would.
iterTodos :: PyVal -> IO [PyVal]
iterTodos = \case
  PList xs -> pure xs
  PStr s -> pure (map (PStr . T.singleton) (T.unpack s))
  PDict kvs -> pure (map (PStr . fst) kvs)
  v -> throwIO (PyError "TypeError" ("'" <> pyTypeName v <> "' object is not iterable"))

-- | @TodoStore.write(todos, merge)@: the list after writing.
todoWrite :: TodoStore -> PyVal -> Bool -> IO [TodoItem]
todoWrite st@(TodoStore ref) todos merge = do
  ts <- iterTodos todos
  if not merge
    then mapM validate ts >>= writeIORef ref
    else do
      items0 <- readIORef ref
      let existing0 = M.fromList [(tiId x, x) | x <- items0]
          step (m, order) t = do
            rawId <- case t of
              PDict kvs -> pure (maybe (PStr "") id (lookup "id" kvs))
              v -> throwIO (PyError "AttributeError" ("'" <> pyTypeName v <> "' object has no attribute 'get'"))
            let itemId = pyStrip (pyStrOf rawId)
                field k = case t of
                  PDict kvs -> lookup k kvs
                  _ -> Nothing
            if T.null itemId
              then pure (m, order)
              else case M.lookup itemId m of
                Just cur -> do
                  let cur1 = case field "content" of
                        Just c | truthy c -> cur {tiContent = pyStrip (pyStrOf c)}
                        _ -> cur
                      cur2 = case field "status" of
                        Just s | truthy s, let s' = T.toLower (pyStrip (pyStrOf s)), s' `elem` validStatuses -> cur1 {tiStatus = s'}
                        _ -> cur1
                  pure (M.insert itemId cur2 m, order)
                Nothing -> do
                  v <- validate t
                  pure (M.insert (tiId v) v m, order <> [tiId v])
      (m, order) <- foldl (\acc t -> acc >>= \a -> step a t) (pure (existing0, map tiId items0)) ts
      let dedupe _ [] = []
          dedupe seen (x : xs)
            | x `S.member` seen = dedupe seen xs
            | otherwise = x : dedupe (S.insert x seen) xs
      writeIORef ref [x | i <- dedupe S.empty order, Just x <- [M.lookup i m]]
  todoRead st

todoRead :: TodoStore -> IO [TodoItem]
todoRead (TodoStore ref) = readIORef ref

todoHasItems :: TodoStore -> IO Bool
todoHasItems (TodoStore ref) = not . null <$> readIORef ref

-- | @TodoStore.format_for_injection()@
formatForInjection :: TodoStore -> IO (Maybe Text)
formatForInjection (TodoStore ref) = do
  items <- readIORef ref
  let active = [x | x <- items, tiStatus x `elem` ["pending", "in_progress"]]
      marker = \case
        "completed" -> "[x]"
        "in_progress" -> "[>]"
        "pending" -> "[ ]"
        "cancelled" -> "[~]"
        _ -> "[?]"
  pure $
    if null active
      then Nothing
      else
        Just . T.intercalate "\n" $
          "[Your active task list was preserved across context compression]"
            : ["- " <> marker (tiStatus x) <> " " <> tiId x <> ". " <> tiContent x <> " (" <> tiStatus x <> ")" | x <- active]

itemVal :: TodoItem -> PyVal
itemVal x = PDict [("id", PStr (tiId x)), ("content", PStr (tiContent x)), ("status", PStr (tiStatus x))]

-- | @todo_tool(todos, merge, store)@
todoTool :: Maybe TodoStore -> PyVal -> Bool -> IO Text
todoTool Nothing _ _ = pure (dumpsUnicode (PDict [("error", PStr "TodoStore not initialized")]))
todoTool (Just st) todos merge = do
  items <- case todos of
    PNull -> todoRead st
    _ -> todoWrite st todos merge
  let count s = PInt (fromIntegral (length [() | x <- items, tiStatus x == s]))
  pure . dumpsUnicode $
    PDict
      [ ("todos", PList (map itemVal items))
      , ( "summary"
        , PDict
            [ ("total", PInt (fromIntegral (length items))), ("pending", count "pending"), ("in_progress", count "in_progress")
            , ("completed", count "completed"), ("cancelled", count "cancelled") ] ) ]

-- | The tool from its call arguments: @todos=args.get("todos"),
-- merge=args.get("merge", False)@.
todoToolArgs :: Maybe TodoStore -> PyVal -> IO Text
todoToolArgs st args = todoTool st (maybe PNull id (get "todos" args)) (maybe False truthy (get "merge" args))

-- | @AIAgent._hydrate_todo_store(history)@: the number of items restored
-- (the caller prints the notice), or the exception upstream raises.
hydrateTodo :: TodoStore -> [PyVal] -> IO (Maybe Int)
hydrateTodo st history = do
  found <- search (reverse history)
  case found of
    Just xs@(_ : _) -> do
      _ <- todoWrite st (PList xs) False
      pure (Just (length xs))
    _ -> pure Nothing
  where
    search [] = pure Nothing
    search (m : rest)
      | get "role" m /= Just (PStr "tool") = search rest
      | otherwise = do
          hit <- containsTodos (maybe (PStr "") id (get "content" m))
          case hit of
            Nothing -> search rest
            Just xs -> pure (Just xs)
    -- @'"todos"' in content@, then @json.loads(content)@
    containsTodos = \case
      PStr s
        | not ("\"todos\"" `T.isInfixOf` s) -> pure Nothing
        | otherwise -> pure $ case loads (TE.encodeUtf8 s) of
            Right (PDict kvs) | Just (PList xs) <- lookup "todos" kvs -> Just xs
            _ -> Nothing
      PList _ -> pure Nothing
      -- a dict holding the key: json.loads(dict) is a TypeError, caught
      PDict _ -> pure Nothing
      v -> throwIO (PyError "TypeError" ("argument of type '" <> pyTypeName v <> "' is not iterable"))

-- | The registry entry (@registry.register(name="todo", ...)@); the loop
-- runs the tool itself, so the handler is upstream's with no store.
todoToolDef :: ToolDef
todoToolDef =
  ToolDef
    { tdName = "todo", tdToolset = "todo", tdSchema = schema
    , tdHandler = \_ args -> todoToolArgs Nothing args
    , tdCheck = Just (pure True), tdRequiresEnv = [], tdEmoji = "\128203" }
  where
    schema = maybe (PDict []) id (get "todo" toolSchemas >>= get "schema")
