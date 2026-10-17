-- | Schemas, toolsets, get_tool_definitions and the dispatcher's fixed
-- answers against upstream (registry.json). Upstream was recorded with
-- its browser and web tools switched off, since Fisher does not port them
-- (B4); its terminal tool is registered but in no toolset, and Fisher
-- leaves it to B4 as well.
module Fisher.Tools.RegistrySpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Py.Value
import Fisher.Tools.File
import Fisher.Tools.Registry
import Fisher.Tools.TestUtil

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b2/registry.json")
  let fresh = do
        reg <- newRegistry
        ft <- newFileTools (const (pure (Left "no sandbox in this test"))) (pure Nothing) (pure True)
        registerFileTools reg ft
        pure reg
  describe "tool schemas and emoji" $ do
    forM_ ["read_file", "write_file", "patch", "search_files"] $ \n -> it (T.unpack n) $ do
      reg <- fresh
      Just td <- lookupTool reg n
      tdSchema td `shouldBe` field n (field "schemas" doc)
    it "emoji" $ do
      reg <- fresh
      es <- mapM (\n -> (,) n <$> emojiFor reg n) ["read_file", "write_file", "patch", "search_files", "nope"]
      PDict [(k, PStr v) | (k, v) <- es] `shouldBe` field "emoji" doc
  describe "toolsets" $ do
    it "TOOLSETS" $
      PDict [(k, PDict [("description", PStr (tsDescription t)), ("tools", PList (map PStr (tsTools t))), ("includes", PList (map PStr (tsIncludes t)))]) | (k, t) <- builtinToolsets]
        `shouldBe` field "toolsets" doc
    forM_ (case field "resolved" doc of PDict kvs -> kvs; _ -> []) $ \(k, v) -> it ("resolve " <> T.unpack k) $ do
      reg <- fresh
      r <- resolveToolset reg k
      PList (map PStr r) `shouldBe` v
    forM_ (case field "info" doc of PDict kvs -> kvs; _ -> []) $ \(k, v) -> it ("info " <> T.unpack k) $ do
      reg <- fresh
      i <- toolsetInfo reg k
      i `shouldBe` Just v
  describe "get_tool_definitions" $
    forM_ (listOf (field "definitions" doc)) $ \c -> it (show (field "enabled" c) <> " / " <> show (field "disabled" c)) $ do
      reg <- fresh
      let names = \case PList xs -> Just [s | PStr s <- xs]; _ -> Nothing
      (defs, printed) <- getToolDefinitions reg (names (field "enabled" c)) (names (field "disabled" c))
      [n | PDict kvs <- defs, Just (PDict f) <- [lookup "function" kvs], Just (PStr n) <- [lookup "name" f]] `shouldBe` strList (field "names" c)
      map normalize printed `shouldBe` map normalize (T.lines (txt "printed" c))
  describe "dispatch" $ do
    it "unknown tool" $ do
      reg <- fresh
      dispatch reg defaultCtx "nope" (PDict []) >>= (`shouldBe` txt "unknown" (field "dispatch" doc))
    forM_ agentLoopTools $ \n -> it (T.unpack n) $ do
      reg <- fresh
      handleFunctionCall reg defaultCtx n (PDict []) >>= (`shouldBe` txt n (field "dispatch" doc))
    it "toolset availability (ported toolsets)" $ do
      reg <- fresh
      av <- checkToolsetRequirements reg
      av `shouldBe` [(k, b) | (k, PBool b) <- case field "availability" doc of PDict kvs -> kvs; _ -> [], k `notElem` ["browser", "web", "terminal"]]
  where
    -- upstream prints a set, so its order is arbitrary: compare as sorted
    normalize l
      | ("\9989 Enabled toolset" `T.isPrefixOf` l || "\128683 Disabled toolset" `T.isPrefixOf` l)
      , (pre, rest) <- T.breakOn "': " l
      , not (T.null rest) =
          pre <> "': " <> T.intercalate ", " (sort (T.splitOn ", " (T.drop 3 rest)))
      | otherwise = l
