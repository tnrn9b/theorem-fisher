-- | 'Fisher.Agent.Tools.Todo' against @tools/todo_tool.py@ and
-- @AIAgent._hydrate_todo_store@ (@test/golden/b4/todo.json@).
module Fisher.Agent.TodoSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM, forM_)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.TestUtil
import Fisher.Agent.Tools.Todo
import Fisher.Py.Value
import Fisher.Tools.Registry

itemVal :: TodoItem -> PyVal
itemVal x = PDict [("id", PStr (tiId x)), ("content", PStr (tiContent x)), ("status", PStr (tiStatus x))]

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b4/todo.json")
  describe "tools/todo_tool.py" $ do
    forM_ (list "cases" doc) $ \c -> it (T.unpack (txt "name" c)) $ do
      st <- newTodoStore
      rs <- forM (list "calls" c) (todoToolArgs (Just st))
      map PStr rs `shouldBe` list "results" c
      inj <- formatForInjection st
      maybe PNull PStr inj `shouldBe` field "injection" c
    it "no store" $ do
      r <- todoToolArgs Nothing (PDict [])
      PStr r `shouldBe` field "no_store" doc
    it "the registry entry" $ do
      reg <- newRegistry
      register reg todoToolDef
      r <- dispatch reg defaultCtx "todo" (PDict [])
      PStr r `shouldBe` field "via_registry" doc
      let e = field "entry" doc
      tdSchema todoToolDef `shouldBe` field "schema" e
      PStr (tdToolset todoToolDef) `shouldBe` field "toolset" e
      PStr (tdEmoji todoToolDef) `shouldBe` field "emoji" e
  describe "AIAgent._hydrate_todo_store" $
    forM_ (list "hydration" doc) $ \h -> it (T.unpack (txt "name" h)) $ do
      st <- newTodoStore
      r <- try (hydrateTodo st (list "history" h))
      items <- todoRead st
      PList (map itemVal items) `shouldBe` field "items" h
      case r of
        Left (PyError cls m) -> do
          PStr (cls <> ": " <> m) `shouldBe` field "raised" h
          list "printed" h `shouldBe` []
        Right n -> do
          field "raised" h `shouldBe` PNull
          [PStr ("📋 Restored " <> T.pack (show k) <> " todo item(s) from history") | Just k <- [n]] `shouldBe` list "printed" h
