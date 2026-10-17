-- | Settings for recursive language model runs: the @fisher.rlm@ section of
-- config.yaml. The defaults follow the reference implementation
-- (alexzhang13/rlm at d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT; see
-- NOTICE), with Fisher's own root model.
module Fisher.RLM.Config
  ( RlmConfig (..)
  , defaultRlmConfig
  , rlmConfigFromPy
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Fisher.Py.Value (PyVal (..))

data RlmConfig = RlmConfig
  { rcMaxDepth :: Int
  , rcMaxIterations :: Int
  , rcMaxConcurrent :: Int
  , rcTruncate :: Int
  -- ^ characters of REPL output shown to the model
  , rcBlockTimeout :: Int
  -- ^ seconds of Python time allowed per block (callbacks do not count)
  , rcMaxBudget :: Maybe Double
  , rcMaxTimeout :: Maybe Double
  , rcMaxTokens :: Maybe Integer
  , rcMaxErrors :: Maybe Int
  , rcModel :: Text
  , rcSubModel :: Maybe Text
  -- ^ 'Nothing' means sub-calls use 'rcModel'
  , rcMinOffload :: Double
  -- ^ the agent only (@--context rlm@): the least part of the room under
  -- the threshold that an offload must move, in (0, 1]
  , rcOffloadGap :: Int
  -- ^ the agent only: the tool batches that must pass after an offload
  -- before the next one
  }
  deriving (Eq, Show)

defaultRlmConfig :: RlmConfig
defaultRlmConfig =
  RlmConfig
    { rcMaxDepth = 1
    , rcMaxIterations = 30
    , rcMaxConcurrent = 4
    , rcTruncate = 20000
    , rcBlockTimeout = 300
    , rcMaxBudget = Nothing
    , rcMaxTimeout = Nothing
    , rcMaxTokens = Nothing
    , rcMaxErrors = Nothing
    , rcModel = "anthropic/claude-sonnet-5.5"
    , rcSubModel = Nothing
    , rcMinOffload = 0.25
    , rcOffloadGap = 3
    }

-- | Read the @fisher.rlm@ section. Absent or empty gives the defaults; unknown
-- keys and non-positive limits are refused by name. @min_offload@ and
-- @offload_gap@ are read here but used only by the agent's RLM strategy.
rlmConfigFromPy :: Maybe PyVal -> Either Text RlmConfig
rlmConfigFromPy Nothing = Right defaultRlmConfig
rlmConfigFromPy (Just (PDict kvs)) = foldl step (Right defaultRlmConfig) kvs
  where
    step acc (k, v) = acc >>= \c -> apply k v c
rlmConfigFromPy (Just _) = Left "fisher.rlm: expected a mapping"

apply :: Text -> PyVal -> RlmConfig -> Either Text RlmConfig
apply k v c = case k of
  "max_depth" -> (\n -> c {rcMaxDepth = n}) <$> posInt
  "max_iterations" -> (\n -> c {rcMaxIterations = n}) <$> posInt
  "max_concurrent_subcalls" -> (\n -> c {rcMaxConcurrent = n}) <$> posInt
  "truncate" -> (\n -> c {rcTruncate = n}) <$> posInt
  "block_timeout" -> (\n -> c {rcBlockTimeout = n}) <$> posInt
  "max_errors" -> (\n -> c {rcMaxErrors = Just n}) <$> posInt
  "max_tokens" -> (\n -> c {rcMaxTokens = Just n}) <$> posInteger
  "max_budget" -> (\x -> c {rcMaxBudget = Just x}) <$> posNum
  "max_timeout" -> (\x -> c {rcMaxTimeout = Just x}) <$> posNum
  "model" -> (\t -> c {rcModel = t}) <$> text
  "sub_model" -> (\t -> c {rcSubModel = Just t}) <$> text
  "min_offload" -> (\x -> c {rcMinOffload = x}) <$> fraction
  "offload_gap" -> (\n -> c {rcOffloadGap = n}) <$> natInt
  _ -> Left ("fisher.rlm: unknown key " <> k)
  where
    bad what = Left ("fisher.rlm: " <> k <> " must be " <> what)
    posInteger = case v of
      PInt n | n > 0 -> Right n
      _ -> bad "a positive integer"
    posInt = do
      n <- posInteger
      if n > fromIntegral (maxBound :: Int) then bad "a smaller positive integer" else Right (fromIntegral n)
    posNum = case v of
      PInt n | n > 0 -> Right (fromIntegral n)
      PFloat x | x > 0 -> Right x
      _ -> bad "a positive number"
    fraction = case v of
      PInt n | n > 0, n <= 1 -> Right (fromIntegral n)
      PFloat x | x > 0, x <= 1 -> Right x
      _ -> bad "a number in (0, 1]"
    natInt = case v of
      PInt n | n >= 0, n <= fromIntegral (maxBound :: Int) -> Right (fromIntegral n)
      _ -> bad "a non-negative integer"
    text = case v of
      PStr t | not (T.null t) -> Right t
      _ -> bad "a non-empty string"
