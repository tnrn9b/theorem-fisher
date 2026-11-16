-- | One attempt's child process: run in its own process group with its
-- output in files, killed (the whole group) at its time limit or when the
-- attempt is cancelled, and read back for turns and tokens.
module Fisher.Search.Child
  ( ChildStats (..)
  , runChild
  , streamStats
  ) where

import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, Value (..), decodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process
import System.Timeout (timeout)

data ChildStats = ChildStats
  { csExit :: Maybe Int
  -- ^ Nothing when it was killed at the limit
  , csWall :: Double
  , csTurns :: Maybe Int
  , csInputTokens :: Maybe Int
  , csOutputTokens :: Maybe Int
  , csCostUsd :: Maybe Double
  , csResult :: Maybe Text
  , csTools :: Map.Map Text Int
  }
  deriving (Eq, Show, Generic)

-- | Generic, with the field names as they are.
instance ToJSON ChildStats

instance FromJSON ChildStats

-- | Run @argv@ in @cwd@ with @env@, stdout to @dir/stream.jsonl@ and
-- stderr to @dir/stderr.txt@, for at most @secs@ seconds. An exception
-- (cancellation) kills the process group before it propagates.
runChild :: [String] -> FilePath -> [(String, String)] -> Int -> FilePath -> IO ChildStats
runChild argv cwd' env secs dir = do
  t0 <- now
  code <- withFile (dir </> "stream.jsonl") WriteMode $ \out -> withFile (dir </> "stderr.txt") WriteMode $ \err ->
    case argv of
      [] -> pure (Just 127)
      exe : args ->
        bracketOnError
          (createProcess (proc exe args) {cwd = Just cwd', env = Just env, std_in = NoStream, std_out = UseHandle out, std_err = UseHandle err, create_group = True})
          (\(_, _, _, ph) -> killGroup ph)
          (\(_, _, _, ph) -> do
              r <- timeout (secs * 1000000) (waitForProcess ph)
              killGroup ph
              case r of
                Nothing -> void (waitForProcess ph) >> pure Nothing
                Just ExitSuccess -> pure (Just 0)
                Just (ExitFailure c) -> pure (Just c))
  t1 <- now
  stats <- streamStats <$> BS.readFile (dir </> "stream.jsonl")
  pure stats {csExit = code, csWall = t1 - t0}
  where
    now = realToFrac <$> getPOSIXTime
    killGroup ph = getPid ph >>= mapM_ (\p -> try (signalProcessGroup sigKILL p) :: IO (Either SomeException ()))

-- | Turns, tokens, cost and tool counts from a stream-json transcript
-- (Claude Code's shapes, which Fisher's native backend also writes).
streamStats :: BS.ByteString -> ChildStats
streamStats bs =
  let events = [o | l <- BC.lines bs, Just (Object o) <- [decodeStrict l]]
      result = case [o | o <- events, str "type" o == Just "result"] of
        [] -> Nothing
        rs -> Just (last rs)
      tools = Map.fromListWith (+)
        [ (n, 1) | o <- events, Just (Object m) <- [KM.lookup "message" o], Just (Array cs) <- [KM.lookup "content" m]
        , Object c <- foldr (:) [] cs, str "type" c == Just "tool_use", Just n <- [str "name" c] ]
      usage = result >>= \r -> case KM.lookup "usage" r of
        Just (Object u) -> Just u
        _ -> Nothing
   in ChildStats Nothing 0 (result >>= int "num_turns") (usage >>= int "input_tokens") (usage >>= int "output_tokens")
        (result >>= dbl "total_cost_usd") (result >>= str "result") tools
  where
    str k o = case KM.lookup (K.fromText k) o of
      Just (String s) -> Just s
      _ -> Nothing
    int k o = case KM.lookup (K.fromText k) o of
      Just (Number n) -> toBoundedInteger n
      _ -> Nothing
    dbl k o = case KM.lookup (K.fromText k) o of
      Just (Number n) -> Just (realToFrac n)
      _ -> Nothing
