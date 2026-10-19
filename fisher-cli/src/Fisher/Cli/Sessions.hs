-- | @fisher sessions list|export|delete|prune|rename|stats@: upstream's
-- @gauss sessions@ over the same @state.db@.
module Fisher.Cli.Sessions
  ( SessionsCmd (..)
  , SessionsEnv (..)
  , runSessions
  , relativeTime
  , sessionsParser
  ) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time (TimeZone, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist, getFileSize)

import Options.Applicative

import Fisher.Py.Json (dumps, pyFormatFixed)
import Fisher.Store

data SessionsCmd
  = SList (Maybe Text) Int
  | SExport FilePath (Maybe Text) (Maybe Text)
  | SDelete Text Bool
  | SPrune Int (Maybe Text) Bool
  | SStats
  | SRename Text [Text]
  deriving (Show)

-- | What the command needs from outside: where the store is, the clock,
-- the zone for absolute dates, how to print and how to ask.
data SessionsEnv = SessionsEnv
  { seStorePath :: FilePath
  , seNow :: Double
  , seZone :: TimeZone
  , sePut :: Text -> IO ()
  , seAsk :: Text -> IO (Maybe Text)
  }

-- | @_relative_time@.
relativeTime :: TimeZone -> Double -> Maybe Double -> Text
relativeTime tz now = \case
  Nothing -> "?"
  Just ts | ts == 0 -> "?"
  Just ts ->
    let delta = now - ts
     in if delta < 60
          then "just now"
          else if delta < 3600
            then T.pack (show (truncate (delta / 60) :: Int)) <> "m ago"
            else if delta < 86400
              then T.pack (show (truncate (delta / 3600) :: Int)) <> "h ago"
              else if delta < 172800
                then "yesterday"
                else if delta < 604800
                  then T.pack (show (truncate (delta / 86400) :: Int)) <> "d ago"
                  else T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (utcToLocalTime tz (posixSecondsToUTCTime (realToFrac ts))))

padTo :: Int -> Text -> Text
padTo n = T.justifyLeft n ' '

confirmed :: Maybe Text -> Bool
confirmed = maybe False ((`elem` ["y", "yes"]) . T.toLower)

runSessions :: SessionsEnv -> SessionsCmd -> IO ()
runSessions env cmd = withStore (seStorePath env) $ \st -> do
  let put = sePut env
  case cmd of
    SList source limit -> do
      ss <- listSessionsRich st source limit
      if null ss
        then put "No sessions found."
        else do
          let titled = any (maybe False (not . T.null) . rsTitle) ss
          if titled
            then put (padTo 22 "Title" <> " " <> padTo 40 "Preview" <> " " <> padTo 13 "Last Active" <> " ID") >> put (T.replicate 100 "─")
            else put (padTo 50 "Preview" <> " " <> padTo 13 "Last Active" <> " " <> padTo 6 "Src" <> " ID") >> put (T.replicate 90 "─")
          forM_ ss $ \s -> do
            let active = relativeTime (seZone env) (seNow env) (rsLastActive s)
                sid = T.take 20 (rsId s)
            if titled
              then
                let title = T.take 20 (case rsTitle s of Just t | not (T.null t) -> t; _ -> "—")
                 in put (padTo 22 title <> " " <> padTo 40 (T.take 38 (rsPreview s)) <> " " <> padTo 13 active <> " " <> sid)
              else put (padTo 50 (T.take 48 (rsPreview s)) <> " " <> padTo 13 active <> " " <> padTo 6 (rsSource s) <> " " <> sid)
    SExport out _ (Just given) ->
      resolveSessionId st given >>= \case
        Nothing -> put ("Session '" <> given <> "' not found.")
        Just sid ->
          exportSession st sid >>= \case
            Nothing -> put ("Session '" <> given <> "' not found.")
            Just v -> do
              TIO.writeFile out (dumps v <> "\n")
              put ("Exported 1 session to " <> T.pack out)
    SExport out source Nothing -> do
      vs <- exportAll st source
      TIO.writeFile out (T.concat [dumps v <> "\n" | v <- vs])
      put ("Exported " <> T.pack (show (length vs)) <> " sessions to " <> T.pack out)
    SDelete given yes ->
      resolveSessionId st given >>= \case
        Nothing -> put ("Session '" <> given <> "' not found.")
        Just sid -> do
          go <- if yes then pure True else confirmed <$> seAsk env ("Delete session '" <> sid <> "' and all its messages? [y/N] ")
          if not go
            then put "Cancelled."
            else do
              ok <- deleteSession st sid
              put (if ok then "Deleted session '" <> sid <> "'." else "Session '" <> given <> "' not found.")
    SPrune days source yes -> do
      let from = maybe "" (\s -> " from '" <> s <> "'") source
      go <- if yes then pure True else confirmed <$> seAsk env ("Delete all ended sessions older than " <> T.pack (show days) <> " days" <> from <> "? [y/N] ")
      if not go
        then put "Cancelled."
        else do
          n <- pruneSessions st (seNow env) days source
          put ("Pruned " <> T.pack (show n) <> " session(s).")
    SRename given words' ->
      resolveSessionId st given >>= \case
        Nothing -> put ("Session '" <> given <> "' not found.")
        Just sid -> do
          let title = T.unwords words'
          setSessionTitle st sid title >>= \case
            Left e -> put ("Error: " <> e)
            Right True -> put ("Session '" <> sid <> "' renamed to: " <> title)
            Right False -> put ("Session '" <> given <> "' not found.")
    SStats -> do
      total <- sessionCount st Nothing
      msgs <- messageCount st
      put ("Total sessions: " <> T.pack (show total))
      put ("Total messages: " <> T.pack (show msgs))
      forM_ ["cli", "telegram", "discord", "whatsapp", "slack"] $ \src -> do
        n <- sessionCount st (Just src)
        if n > 0 then put ("  " <> src <> ": " <> T.pack (show n) <> " sessions") else pure ()
      exists <- doesFileExist (seStorePath env)
      if exists
        then do
          size <- getFileSize (seStorePath env)
          put ("Database size: " <> pyFormatFixed 1 (fromIntegral size / (1024 * 1024)) <> " MB")
        else pure ()

-- | @fisher sessions@'s arguments.
sessionsParser :: Parser SessionsCmd
sessionsParser = hsubparser
  ( command "list" (info (SList <$> optional (T.pack <$> strOption (long "source" <> metavar "SRC"))
                                  <*> option auto (long "limit" <> value 20 <> metavar "N"))
                         (progDesc "List recent sessions"))
 <> command "export" (info (SExport <$> strArgument (metavar "OUTPUT")
                                    <*> optional (T.pack <$> strOption (long "source" <> metavar "SRC"))
                                    <*> optional (T.pack <$> strOption (long "session-id" <> metavar "ID")))
                           (progDesc "Export sessions to a JSONL file"))
 <> command "delete" (info (SDelete <$> (T.pack <$> strArgument (metavar "SESSION_ID")) <*> switch (long "yes" <> short 'y'))
                           (progDesc "Delete a specific session"))
 <> command "prune" (info (SPrune <$> option auto (long "older-than" <> value 90 <> metavar "DAYS")
                                  <*> optional (T.pack <$> strOption (long "source" <> metavar "SRC"))
                                  <*> switch (long "yes" <> short 'y'))
                          (progDesc "Delete old sessions"))
 <> command "stats" (info (pure SStats) (progDesc "Show session store statistics"))
 <> command "rename" (info (SRename <$> (T.pack <$> strArgument (metavar "SESSION_ID")) <*> some (T.pack <$> strArgument (metavar "TITLE...")))
                           (progDesc "Set or change a session's title")) )

