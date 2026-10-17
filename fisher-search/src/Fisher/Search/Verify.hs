-- | One attempt's proof, judged: spliced into the base, then checked by
-- the kernel.
module Fisher.Search.Verify
  ( goalTypes
  , Outcome (..)
  , verifyAttempt
  , recheck
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Search.Check
import Fisher.Search.Splice

-- | The type each goal has in the original file (one compile for all).
goalTypes :: LeanRun -> FilePath -> FilePath -> Text -> [Text] -> IO (Either Text (Map.Map Text Text))
goalTypes lr root rel text names = do
  pr <- probe lr root rel text names
  pure $ case prErrors pr of
    (e : _) -> Left ("the original file does not compile: " <> e)
    [] -> case [n | n <- names, Map.notMember n (prTypes pr)] of
      [] -> Right (prTypes pr)
      missing -> Left ("no type printed for " <> T.intercalate ", " missing)

data Outcome
  = NotSpliced SpliceError
  | Judged Verdict Text [Text]
  -- ^ the verdict, the merged file and the helpers it added
  deriving (Eq, Show)

-- | Splice the agent's proof of @g@ into @base@ and check it.
verifyAttempt :: LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text -> Text -> Text -> Text -> IO Outcome
verifyAttempt lr pol root rel start agent base g expected = case splice start agent base g of
  Left e -> pure (NotSpliced e)
  Right sp -> do
    pr <- probe lr root rel (spText sp) [g]
    pure (Judged (verdict pol (spText sp) g expected pr) (spText sp) (spHelpers sp))

-- | Check goals already in the base again (those waiting on others).
recheck :: LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Map.Map Text Text -> [Text] -> IO [(Text, Verdict)]
recheck _ _ _ _ _ _ [] = pure []
recheck lr pol root rel base types gs = do
  pr <- probe lr root rel base gs
  pure [(g, verdict pol base g (Map.findWithDefault "" g types) pr) | g <- gs]
