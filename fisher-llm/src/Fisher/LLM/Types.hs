-- | The shapes shared by the three wire formats.
module Fisher.LLM.Types
  ( ApiMode (..)
  , apiModeName
  , parseApiMode
  , Routing (..)
  , noRouting
  , Settings (..)
  , defaultSettings
  , Env (..)
  , HttpReq (..)
  , counterUuids
  ) where

import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

import Fisher.Py.Value (PyVal)

data ApiMode = ChatCompletions | CodexResponses | AnthropicMessages
  deriving (Eq, Show)

apiModeName :: ApiMode -> Text
apiModeName = \case
  ChatCompletions -> "chat_completions"
  CodexResponses -> "codex_responses"
  AnthropicMessages -> "anthropic_messages"

parseApiMode :: Text -> Maybe ApiMode
parseApiMode = \case
  "chat_completions" -> Just ChatCompletions
  "codex_responses" -> Just CodexResponses
  "anthropic_messages" -> Just AnthropicMessages
  _ -> Nothing

-- | OpenRouter provider preferences (@config.provider_routing@).
data Routing = Routing
  { rOnly :: [Text]
  , rIgnore :: [Text]
  , rOrder :: [Text]
  , rSort :: Maybe Text
  , rRequireParameters :: Bool
  , rDataCollection :: Maybe Text
  }
  deriving (Eq, Show)

noRouting :: Routing
noRouting = Routing [] [] [] Nothing False Nothing

-- | What the agent holds about the model it talks to.
data Settings = Settings
  { sMode :: ApiMode
  , sBaseUrl :: Text
  , sApiKey :: Text
  , sModel :: Text
  , sProvider :: Text
  , sMaxTokens :: Maybe Integer
  , sReasoning :: Maybe PyVal
  -- ^ @reasoning_config@: a dict such as @{"enabled": true, "effort": "high"}@
  , sRouting :: Routing
  , sSessionId :: Text
  , sTools :: Maybe [PyVal]
  -- ^ chat-completions tool schemas; 'Nothing' and @[]@ both mean none
  }

defaultSettings :: Settings
defaultSettings =
  Settings ChatCompletions "https://openrouter.ai/api/v1" "" "anthropic/claude-opus-4.6" "openrouter" Nothing Nothing noRouting "" Nothing

-- | Effects the pure-looking parts need: fresh ids where upstream calls
-- @uuid4()@, and the picture-to-text fallback for Anthropic (upstream asks
-- its vision tool).
data Env = Env
  { eUuidHex :: IO Text
  -- ^ 32 lowercase hex digits, as @uuid.uuid4().hex@
  , eDescribeImage :: Text -> Text -> IO Text
  -- ^ image URL, role → the note that replaces it
  }

-- | A request as it goes on the wire.
data HttpReq = HttpReq
  { hrMethod :: Text
  , hrUrl :: Text
  , hrHeaders :: [(Text, Text)]
  , hrBody :: Maybe PyVal
  , hrStream :: Bool
  }
  deriving (Eq, Show)

-- | @uuid.UUID(int=n).hex@ for n = 1, 2, ...: the corpus's stand-in for
-- uuid4, so generated ids can be compared.
counterUuids :: IO (IO Text)
counterUuids = do
  r <- newIORef (0 :: Integer)
  pure $ do
    n <- atomicModifyIORef' r (\x -> (x + 1, x + 1))
    pure (T.justifyRight 32 '0' (T.pack (showHex n "")))
