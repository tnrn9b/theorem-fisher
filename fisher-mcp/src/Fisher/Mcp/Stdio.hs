-- | The stdio transport: the server runs as a child in its own session
-- with a filtered environment, messages are newline-delimited JSON, its
-- stderr goes straight to ours, and shutdown closes its stdin, then
-- escalates to SIGTERM and SIGKILL on its process group.
module Fisher.Mcp.Stdio
  ( safeEnv
  , sdkDefaultEnv
  , resolveCommand
  , StdioSpawn (..)
  , spawnStdio
  , whichIn
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist, executable, getPermissions)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Process
import System.Timeout (timeout)

import Fisher.Mcp.Session (Incoming (..), McpFailure (..), Transport (..))
import Fisher.Py.Value (dumpsUnicode, loads)

-- | @_build_safe_env(user_env)@: PATH, HOME, USER, LANG, LC_ALL, TERM,
-- SHELL, TMPDIR and XDG_* from our environment, then the configured env.
safeEnv :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
safeEnv environ user =
  let keep = ["PATH", "HOME", "USER", "LANG", "LC_ALL", "TERM", "SHELL", "TMPDIR"]
      base = [(k, v) | (k, v) <- environ, k `elem` keep || "XDG_" `T.isPrefixOf` k]
   in foldl put base user
  where
    put acc (k, v) = if any ((== k) . fst) acc then [(k', if k' == k then v else v') | (k', v') <- acc] else acc <> [(k, v)]

-- | The SDK's @get_default_environment()@ (POSIX), which it puts under the
-- configured environment.
sdkDefaultEnv :: [(Text, Text)] -> [(Text, Text)]
sdkDefaultEnv environ =
  [(k, v) | k <- ["HOME", "LOGNAME", "PATH", "SHELL", "TERM", "USER"], Just v <- [lookup k environ], not ("()" `T.isPrefixOf` v)]

-- | @shutil.which(cmd, path=...)@.
whichIn :: Maybe Text -> Text -> IO (Maybe Text)
whichIn mpath cmd = go (maybe [] (filter (not . T.null) . T.splitOn ":") mpath)
  where
    go [] = pure Nothing
    go (d : ds) = do
      let p = T.unpack (d <> "/" <> cmd)
      ok <- doesFileExist p
      x <- if ok then executable <$> getPermissions p else pure False
      if x then pure (Just (T.pack p)) else go ds

-- | @_resolve_stdio_command(command, env)@: the command and the env with
-- the command's directory put first on PATH.
resolveCommand :: Text -> FilePath -> Text -> [(Text, Text)] -> IO (Text, [(Text, Text)])
resolveCommand home fisherHome command env = do
  let c0 = expandUser (T.strip command)
  c <-
    if "/" `T.isInfixOf` c0
      then pure c0
      else do
        hit <- whichIn (lookup "PATH" env) c0
        case hit of
          Just p -> pure p
          Nothing
            | c0 `elem` ["npx", "npm", "node"] -> do
                let cands = [T.pack fisherHome <> "/node/bin/" <> c0, home <> "/.local/bin/" <> c0]
                found <- firstExec cands
                pure (maybe c0 id found)
            | otherwise -> pure c0
  let dir = dirname c
      env' =
        if T.null dir
          then env
          else case lookup "PATH" env of
            Nothing -> env <> [("PATH", dir)]
            Just p ->
              let parts = filter (not . T.null) (T.splitOn ":" p)
               in if dir `elem` parts then env else [(k, if k == "PATH" then T.intercalate ":" (dir : parts) else v) | (k, v) <- env]
  pure (c, env')
  where
    expandUser t
      | t == "~" = home
      | "~/" `T.isPrefixOf` t = home <> T.drop 1 t
      | otherwise = t
    dirname t = let (h, _) = T.breakOnEnd "/" t in if T.null h then "" else (let d = T.dropWhileEnd (== '/') h in if T.null d then "/" else d)
    firstExec [] = pure Nothing
    firstExec (p : ps) = do
      ok <- doesFileExist (T.unpack p)
      x <- if ok then executable <$> getPermissions (T.unpack p) else pure False
      if x then pure (Just p) else firstExec ps

data StdioSpawn = StdioSpawn
  { ssCommand :: Text
  , ssArgs :: [Text]
  , ssEnv :: [(Text, Text)]
  , ssCwd :: Maybe FilePath
  -- ^ Fisher only: the directory the server starts in (else ours)
  }

-- | Start the server. A command that cannot be found is upstream's
-- "missing executable" failure.
spawnStdio :: StdioSpawn -> IO (Either McpFailure Transport)
spawnStdio sp = do
  let found = "/" `T.isInfixOf` ssCommand sp
  r <-
    if not found
      then pure (Left (McpFailure "FileNotFoundError" (ssCommand sp)))
      else do
        x <-
          try
            ( createProcess
                (proc (T.unpack (ssCommand sp)) (map T.unpack (ssArgs sp)))
                  { std_in = CreatePipe
                  , std_out = CreatePipe
                  , std_err = Inherit
                  , env = Just [(T.unpack k, T.unpack v) | (k, v) <- ssEnv sp]
                  , cwd = ssCwd sp
                  , new_session = True
                  }
            )
        pure $ case x of
          Left (e :: IOException)
            | isDoesNotExistError e -> Left (McpFailure "FileNotFoundError" (ssCommand sp))
            | otherwise -> Left (McpFailure "OSError" (T.pack (show e)))
          Right (Just hin, Just hout, _, ph) -> Right (hin, hout, ph)
          Right _ -> Left (McpFailure "OSError" "pipes")
  case r of
    Left e -> pure (Left e)
    Right (hin, hout, ph) -> do
      hSetBinaryMode hin True
      hSetBinaryMode hout True
      q <- newTQueueIO
      lock <- newTMVarIO ()
      _ <- forkIO $ do
        let loop = do
              l <- try (BS.hGetLine hout)
              case l of
                Left (_ :: IOException) -> atomically (writeTQueue q InClosed)
                Right line -> do
                  let line' = if BC.isSuffixOf "\r" line then BS.init line else line
                  unless (BS.null line') $
                    atomically . writeTQueue q $ case TE.decodeUtf8' line' of
                      Left _ -> InUnparsable
                      Right _ -> either (const InUnparsable) InMessage (loads line')
                  loop
        loop
      let send v = do
            atomically (takeTMVar lock)
            (BS.hPut hin (TE.encodeUtf8 (dumpsUnicode v) <> "\n") >> hFlush hin) `catch` \(e :: IOException) -> atomically (putTMVar lock ()) >> ioError e
            atomically (putTMVar lock ())
          close = do
            _ <- try (hClose hin) :: IO (Either SomeException ())
            done <- timeout 2000000 (waitForProcess ph)
            when (done == Nothing) $ do
              mpid <- getPid ph
              case mpid of
                Just pid -> do
                  signalProcessGroup sigTERM pid `catch` \(_ :: IOException) -> terminateProcess ph
                  gone <- timeout 2000000 (waitForProcess ph)
                  when (gone == Nothing) (signalProcessGroup sigKILL pid `catch` \(_ :: IOException) -> pure ())
                Nothing -> pure ()
      pure (Right (Transport send q close))
