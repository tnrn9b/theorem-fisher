-- | The step log (Fisher UI): every file change the agent makes through
-- @write_file@ or @patch@, one JSON line per changed file in
-- @$GAUSS_HOME/fisher/steps/<session>.jsonl@: the step's number, the
-- tool call's id, the path, the SHA-256 of the file's bytes, the whole
-- file as the tool left it, the tool and the time. The UI server reads it
-- to show and check each step.
module Fisher.Agent.Steps
  ( Step (..)
  , stepsDir
  , stepsPath
  , appendStep
  , appendNextStep
  , lastSeq
  , readSteps
  , stepToPy
  , stepFromPy
  , fileHash
  , changedPaths
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (LockRequest (..), OpenMode (..), closeFd, creat, defaultFileFlags, openFd, waitToSetLock)
import System.IO (SeekMode (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Fisher.LLM.Auth.Sha256 (sha256Hex)
import Fisher.Py.Value

data Step = Step
  { stSeq :: Int
  , stCallId :: Text
  , stPath :: FilePath
  , stHash :: Text
  -- ^ SHA-256 of the file's bytes, lower-case hex
  , stText :: Text
  -- ^ the whole file after the change
  , stTool :: Text
  , stAt :: Double
  -- ^ seconds since the epoch
  }
  deriving (Eq, Show)

stepsDir :: FilePath -> FilePath
stepsDir gaussHome = gaussHome </> "fisher" </> "steps"

stepsPath :: FilePath -> Text -> FilePath
stepsPath gaussHome sid = stepsDir gaussHome </> T.unpack (sid <> ".jsonl")

stepToPy :: Step -> PyVal
stepToPy s =
  PDict
    [ ("seq", PInt (fromIntegral (stSeq s))), ("call_id", PStr (stCallId s)), ("path", PStr (T.pack (stPath s)))
    , ("hash", PStr (stHash s)), ("text", PStr (stText s)), ("tool", PStr (stTool s)), ("at", PFloat (stAt s)) ]

stepFromPy :: PyVal -> Maybe Step
stepFromPy v = do
  PInt n <- get "seq" v
  PStr c <- get "call_id" v
  PStr p <- get "path" v
  PStr h <- get "hash" v
  PStr t <- get "text" v
  PStr tool <- get "tool" v
  at <- case get "at" v of
    Just (PFloat d) -> Just d
    Just (PInt i) -> Just (fromIntegral i)
    _ -> Nothing
  pure (Step (fromIntegral n) c (T.unpack p) h t tool at)

-- | Append one step (the log's file is made on the first).
appendStep :: FilePath -> Step -> IO ()
appendStep path s = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.appendFile path (TE.encodeUtf8 (dumpsAscii (stepToPy s)) <> "\n")

-- | Number and append one step: the next number after the log's highest,
-- under an exclusive lock on @<log>.lock@ (an fcntl lock between
-- processes, the agent's and the UI server's, and one lock within this
-- process), so two writers never share a number or interleave a line.
appendNextStep :: FilePath -> (Int -> Step) -> IO Step
appendNextStep path mk =
  withMVar processLock $ \_ -> do
    createDirectoryIfMissing True (takeDirectory path)
    bracket (openFd (path <> ".lock") ReadWrite defaultFileFlags {creat = Just 0o644}) closeFd $ \fd -> do
      waitToSetLock fd (WriteLock, AbsoluteSeek, 0, 0)
      n <- lastSeq path
      let s = mk (n + 1)
      appendStep path s
      pure s

-- | Within one process the fcntl lock does not exclude (it is per
-- process), so the threads take this first.
processLock :: MVar ()
processLock = unsafePerformIO (newMVar ())
{-# NOINLINE processLock #-}

-- | The log's highest step number (0 with no log): from each line's
-- leading @{"seq": N@ as 'stepToPy' writes it, the line parsed only when
-- it does not start so.
lastSeq :: FilePath -> IO Int
lastSeq path = do
  ex <- doesFileExist path
  if not ex
    then pure 0
    else do
      raw <- BS.readFile path
      pure (maximum (0 : [n | l <- BC.lines raw, not (BS.null l), Just n <- [seqOf l]]))
  where
    seqOf l = case BS.stripPrefix "{\"seq\": " l of
      Just r | (ds, _) <- BC.span isDigit r, not (BS.null ds) -> Just (read (BC.unpack ds))
      _ -> case loads l of
        Right v -> stSeq <$> stepFromPy v
        Left _ -> Nothing

-- | The steps in the log, in order; none when there is no log. A line that
-- does not parse (a write cut short) is skipped.
readSteps :: FilePath -> IO [Step]
readSteps path = withMVar processLock $ \_ -> do
  -- (under this process's step lock: the runtime refuses a read of a file
  -- this process is appending to)
  ex <- doesFileExist path
  if not ex
    then pure []
    else do
      raw <- BS.readFile path
      pure [s | l <- BC.lines raw, not (BS.null l), Right v <- [loads l], Just s <- [stepFromPy v]]

fileHash :: BS.ByteString -> Text
fileHash = sha256Hex

-- | The files a successful @write_file@ or @patch@ changed, from its
-- arguments and result (none when it failed): write_file's path, a
-- replace's path, a V4A patch's modified and created files.
changedPaths :: Text -> PyVal -> Text -> [Text]
changedPaths tool args result =
  case loads (TE.encodeUtf8 (firstLine result)) of
    Right r@(PDict _)
      | tool == "write_file", Nothing <- get "error" r, Just (PStr p) <- get "path" args -> [p]
      | tool == "patch", get "success" r == Just (PBool True) ->
          let listed = [p | k <- ["files_modified", "files_created"], Just (PList ps) <- [get k r], PStr p <- ps]
           in case (listed, get "path" args) of
                ([], Just (PStr p)) | get "mode" args `elem` [Nothing, Just (PStr "replace")] -> [p]
                _ -> listed
    _ -> []
  where
    -- a patch's hint follows its JSON after a blank line
    firstLine t = fst (T.breakOn "\n\n[Hint:" t)
