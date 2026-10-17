-- | 'Fisher.Agent.Display' against @agent/display.py@
-- (@test/golden/b3/display.json@): previews, emojis, failure detection and
-- completion lines for every tool name and a spread of arguments.
module Fisher.Agent.DisplaySpec (spec) where

import Control.Monad (forM, forM_)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory)
import System.IO.Temp (createTempDirectory)
import Test.Hspec

import Fisher.Agent.Display
import Fisher.Agent.TestUtil
import Fisher.Py.Value
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (emojiFor, lookupTool)

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b3/display.json")
  -- the emojis come from Fisher's own registry, as the CLI's do; a tool
  -- Fisher does not have yet is pending, not checked against itself. The
  -- recording imported only model_tools, so terminal, process, todo and
  -- memory were not registered there (their entries are checked against
  -- the B4 goldens) and stay out of this registry too.
  registered <- runIO $ do
    tmp <- getTemporaryDirectory >>= (`createTempDirectory` "fisher-display")
    reg <- atRegistry <$> newAgentTools tmp
    registerOfflineTools reg tmp
    fmap concat . forM [txt "name" c | c <- cases, txt "kind" c == "emoji"] $ \n -> do
      known <- isJust <$> lookupTool reg n
      if known then (\e -> [(n, e)]) <$> emojiFor reg n else pure []
  describe "agent/display.py" $
    forM_ (zip [1 :: Int ..] cases) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "name" c)) $
      case txt "kind" c of
        "emoji"
          | isJust (lookup (txt "name" c) registered) || "unknown_" `T.isPrefixOf` txt "name" c ->
              toolEmoji defaultSkin (`lookup` registered) (txt "name" c) `shouldBe` txt "emoji" c
          | otherwise -> pendingWith "not a Fisher tool yet"
        _ -> do
          let name = txt "name" c
              args = field "args" c
              result = case field "result" c of
                PStr r -> Just r
                _ -> Nothing
              dur = case field "duration" c of
                PFloat d -> d
                PInt n -> fromIntegral n
                _ -> 0
          maybe PNull id (buildToolPreviewV name args) `shouldBe` field "preview" c
          PStr (cuteToolMessage defaultSkin name args dur result) `shouldBe` field "cute" c
          let (f, sfx) = detectToolFailure name result
          PList [PBool f, PStr sfx] `shouldBe` field "failure" c
