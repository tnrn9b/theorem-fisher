-- | The inbox (Fisher UI): messages for a running session, one JSON line
-- each (@{"text": ...}@) in @<Fisher home>/fisher/inbox/<session>.jsonl@.
-- The UI server appends; the agent loop takes what is new at each tool
-- boundary and sends it as the next user turn. What has been taken is
-- kept as a byte offset beside the file (@<session>.offset@), so each line
-- is delivered once, also across a resume.
module Fisher.Agent.Inbox
  ( inboxDir
  , inboxPath
  , appendInbox
  , takeInbox
  , takeInboxWith
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (replaceExtension, takeDirectory, (</>))
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)

import Fisher.Py.Value

inboxDir :: FilePath -> FilePath
inboxDir fisherHome = fisherHome </> "fisher" </> "inbox"

inboxPath :: FilePath -> Text -> FilePath
inboxPath fisherHome sid = inboxDir fisherHome </> T.unpack (sid <> ".jsonl")

offsetPath :: FilePath -> FilePath
offsetPath p = replaceExtension p "offset"

-- | Append one message (the writer's side).
appendInbox :: FilePath -> Text -> IO ()
appendInbox path t = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.appendFile path (TE.encodeUtf8 (dumpsAscii (PDict [("text", PStr t)])) <> "\n")

-- | The messages appended since the last take, in order; the offset moves
-- past them. Only whole lines are taken (a line still being written waits
-- for the next take). A line that is not @{"text": ...}@ JSON is taken as
-- its own text; an empty one is skipped. A file shorter than the stored
-- offset (replaced or cut since) is read again from its start, with a note
-- on stderr.
takeInbox :: FilePath -> IO [Text]
takeInbox = takeInboxWith (TIO.hPutStrLn stderr)

-- | 'takeInbox' with the note for a file shorter than its offset going to
-- the given action.
takeInboxWith :: (Text -> IO ()) -> FilePath -> IO [Text]
takeInboxWith note path = do
  ex <- doesFileExist path
  if not ex
    then pure []
    else do
      off0 <- readOffset
      raw <- BS.readFile path
      off <-
        if off0 > BS.length raw
          then do
            note ("fisher: the inbox " <> T.pack path <> " is shorter (" <> T.pack (show (BS.length raw)) <> " bytes) than what was taken from it (" <> T.pack (show off0) <> "); reading it from the start")
            writeOffset (0 :: Int)
            pure 0
          else pure off0
      let rest = BS.drop off raw
          whole = case BC.elemIndexEnd '\n' rest of
            Just i -> BS.take (i + 1) rest
            Nothing -> BS.empty
      if BS.null whole
        then pure []
        else do
          writeOffset (off + BS.length whole)
          pure [t | l <- BC.lines whole, let t = textOf l, not (T.null (T.strip t))]
  where
    readOffset = do
      r <- try @SomeException (BS.readFile (offsetPath path))
      pure $ case r of
        Right b | Just n <- readMaybe (BC.unpack (BC.strip b)) -> max 0 n
        _ -> 0
    writeOffset n = do
      let tmp = offsetPath path <> ".tmp"
      BS.writeFile tmp (BC.pack (show n))
      renameFile tmp (offsetPath path)
    textOf l = case loads l of
      Right v | Just (PStr t) <- get "text" v -> t
      Right (PStr t) -> t
      _ -> TE.decodeUtf8With TEE.lenientDecode l
