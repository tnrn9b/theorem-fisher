-- | leanclient.aio's exception hierarchy (@errors.py@). 'show'/'displayException'
-- give exactly Python's @str(e)@, which is what reaches a tool error.
module Fisher.Lean.Error
  ( LeanError (..)
  , errorText
  ) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Value (PyVal)

data LeanError
  = TransportError Text Text
  -- ^ message, stderr tail
  | WorkerCrashed Text Text
  -- ^ message, uri
  | RequestTimeout Text
  | RequestCancelled Text
  | RpcError Integer Text PyVal
  -- ^ code, message, data
  | FileNotOpen Text
  | UnsupportedVersion Text
  | ClientError Text
  -- ^ LeanClientError itself (and anything else surfaced as one)
  deriving (Show)

errorText :: LeanError -> Text
errorText = \case
  TransportError m _ -> m
  WorkerCrashed m _ -> m
  RequestTimeout m -> m
  RequestCancelled m -> m
  RpcError c m _ -> "LSP error " <> T.pack (show c) <> ": " <> m
  FileNotOpen m -> m
  UnsupportedVersion m -> m
  ClientError m -> m

instance Exception LeanError where
  displayException = T.unpack . errorText
