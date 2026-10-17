-- | How a typed line is rewritten before dispatch, against upstream's
-- commands.py and autoformalize.py (@dispatch.json@).
module Fisher.DispatchSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Test.Hspec

import Fisher.A3Golden
import Fisher.Shell.Data (fisherCommands)
import Fisher.Shell.Rewrite
import Fisher.Workflow.Command (rewriteForgivingCommand)

spec :: Spec
spec = do
  cases <- runIO (a3Cases "dispatch.json")
  describe "friendly and forgiving rewrites" $
    forM_ cases $ \c -> do
      let input = str "input" c
      it (show input) $ do
        rewriteFriendlyEntry input `shouldBe` mstr "entry" c
        rewriteFriendlySlash fisherCommands input `shouldBe` mstr "slash" c
        rewriteForgivingCommand input `shouldBe` mstr "forgiving" c
  where
    _ = T.empty
