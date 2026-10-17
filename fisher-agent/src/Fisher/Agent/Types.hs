-- | Small shared pieces: the auxiliary-model call as the agent's helpers
-- make it (compression summaries, memory flushes), and Python's number
-- formatting in printed lines.
module Fisher.Agent.Types
  ( AuxCall (..)
  , auxCall
  , AuxError (..)
  , AuxLLM
  , commas
  , fmtF
  , responseContent
  , responseRefusal
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Json (pyFormatFixed)
import Fisher.Py.Value

-- | @call_llm(task=..., messages=..., ...)@ keyword arguments.
data AuxCall = AuxCall
  { axTask :: Maybe Text
  , axProvider :: Maybe Text
  , axModel :: Maybe Text
  , axBaseUrl :: Maybe Text
  , axApiKey :: Maybe Text
  , axMessages :: [PyVal]
  , axTemperature :: Maybe Double
  , axMaxTokens :: Maybe Integer
  , axTools :: [PyVal]
  , axTimeout :: Double
  , axExtraBody :: [(Text, PyVal)]
  }
  deriving (Eq, Show)

auxCall :: Text -> [PyVal] -> AuxCall
auxCall task msgs = AuxCall (Just task) Nothing Nothing Nothing Nothing msgs Nothing Nothing [] 30 []

-- | How a call failed: no provider could be resolved (upstream's
-- @RuntimeError@), or the call itself raised (its message).
data AuxError
  = AuxNoProvider Text
  | AuxFailed Text
  deriving (Eq, Show)

-- | The auxiliary client: the decoded OpenAI-shaped response.
type AuxLLM = AuxCall -> IO (Either AuxError PyVal)

-- | @f"{n:,}"@
commas :: Integer -> Text
commas n
  | n < 0 = "-" <> commas (negate n)
  | otherwise = T.reverse (T.intercalate "," (T.chunksOf 3 (T.reverse (T.pack (show n)))))

-- | @f"{x:.Nf}"@
fmtF :: Int -> Double -> Text
fmtF = pyFormatFixed

-- | @response.choices[0].message.content@
responseContent :: PyVal -> PyVal
responseContent r = case get "choices" r of
  Just (PList (c : _)) -> maybe PNull id (get "message" c >>= get "content")
  _ -> PNull

-- | Fisher only: the provider's refusal in a chat response
-- (@choices[0].message.refusal@), when it gave one.
responseRefusal :: PyVal -> Maybe Text
responseRefusal v = case get "choices" v of
  Just (PList (c : _)) | Just (PStr r) <- get "message" c >>= get "refusal", not (T.null (T.strip r)) -> Just (T.strip r)
  _ -> Nothing
