-- | Running lake and the search's probe in a project, the yes or no
-- question, and turning SIGINT and SIGTERM into an interrupt: shared by @fisher project create@ and @build@
-- ("Fisher.Cli.Create") and @fisher project add smt@ ("Fisher.Cli.AddSmt").
module Fisher.Cli.LakeRun
  ( runIn
  , appendRun
  , lakeErrorLines
  , probeProblems
  , removeStaleProbes
  , askOn
  , interruptible
  , interruptibleWith
  ) where

import Control.Concurrent (forkIO, myThreadId, newEmptyMVar, putMVar, takeMVar, throwTo)
import Control.Exception (AsyncException (..), IOException, SomeException, bracket, onException, try)
import Control.Monad (forM_)
import Data.Char (isSpace, toLower)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicWriteIORef, newIORef)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle, hClose, hFlush, hGetLine, hIsEOF, stdout)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigKILL, sigTERM, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (..), createPipe, createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

import Fisher.Search.Check (LeanRun, Probe (..), lakeLeanRunWith, probe, standardAxioms)

-- | A command in a directory: stdin closed, output merged, in a process
-- group of its own that is killed at the limit (seconds) or on an exception.
runIn :: FilePath -> Int -> FilePath -> [String] -> IO (Int, Text)
runIn cmd secs dir args = do
  (readEnd, writeEnd) <- createPipe
  started <- try (createProcess (proc cmd args)
    { cwd = Just dir, std_in = NoStream, std_out = UseHandle writeEnd, std_err = UseHandle writeEnd, create_group = True })
  case started of
    Left (e :: SomeException) -> do
      mapM_ (\h -> try (hClose h) :: IO (Either SomeException ())) [readEnd, writeEnd]
      pure (127, T.pack (show e))
    Right (_, _, _, ph) -> do
      outVar <- newEmptyMVar
      _ <- forkIO (BS.hGetContents readEnd >>= putMVar outVar)
      let killGroup = getPid ph >>= mapM_ (\p -> try (signalProcessGroup sigKILL p) :: IO (Either SomeException ()))
      r <- timeout (secs * 1000000) (waitForProcess ph) `onException` killGroup
      code <- case r of
        Just c -> pure (case c of ExitSuccess -> 0; ExitFailure k -> k)
        Nothing -> killGroup >> waitForProcess ph >> pure (-1)
      out <- takeMVar outVar
      let txt = TE.decodeUtf8With TEE.lenientDecode out
      pure (code, if code == -1 then "timed out after " <> T.pack (show secs) <> " s\n" <> txt else txt)

-- | The search's probe of @text@ in place of @rel@ with @names@: its
-- errors, @sorry@s, axioms beyond the standard three (for each name) and a
-- failing exit, with the output appended to the log under @prefix@. Empty
-- when it passed.
probeProblems :: FilePath -> Int -> FilePath -> Text -> FilePath -> FilePath -> Text -> [Text] -> IO [Text]
probeProblems lake secs logPath prefix root rel text names = do
  let run :: LeanRun
      run = lakeLeanRunWith lake secs
  p <- probe run root rel text names
  appendRun logPath prefix ("probe of " <> T.pack rel) (prExit p) (prOutput p)
  let axiomProblems =
        concat
          [ case Map.lookup n (prAxioms p) of
              Nothing -> ["no axioms reported for " <> n]
              Just as -> ["`" <> n <> "` depends on " <> a | a <- as, not (Set.member a standardAxioms)]
          | n <- names ]
  pure $
    prErrors p
      <> ["`sorry` at line " <> T.pack (show l) | l <- prSorryLines p]
      <> axiomProblems
      <> ["lean exited with " <> T.pack (show (prExit p)) | prExit p /= 0, null (prErrors p)]

-- | Append one run's output to a log: @== PREFIX: LABEL@, the output, then
-- @== PREFIX: exit CODE@.
appendRun :: FilePath -> Text -> Text -> Int -> Text -> IO ()
appendRun logPath prefix label code out =
  BS.appendFile logPath (TE.encodeUtf8 ("== " <> prefix <> ": " <> label <> "\n" <> out <> "== " <> prefix <> ": exit " <> T.pack (show code) <> "\n"))

-- | The error lines of lake's output.
lakeErrorLines :: Text -> [Text]
lakeErrorLines out = [l | l <- T.lines out, ": error" `T.isInfixOf` l || "error:" `T.isPrefixOf` l]

-- | Ask on a handle; yes only for @y@ or @yes@. A closed or unreadable
-- input is no; an interrupt is rethrown.
askOn :: Handle -> Text -> IO Bool
askOn h question = do
  TIO.putStr question
  hFlush stdout
  -- only I/O errors read as no; an interrupt (Ctrl-C) goes through
  r <- try (hIsEOF h >>= \eof -> if eof then pure "" else hGetLine h) :: IO (Either IOException String)
  pure $ case r of
    Right a -> map toLower (trim a) `elem` ["y", "yes"]
    Left _ -> False
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Remove the probe files (@_fisher_search_*.lean@) a check that was
-- killed outright (SIGKILL) left next to the sample.
removeStaleProbes :: FilePath -> IO ()
removeStaleProbes dir = do
  names <- either (\(_ :: SomeException) -> []) id <$> try (listDirectory dir)
  forM_ [n | n <- names, "_fisher_search_" `isPrefixOf` n, ".lean" `isSuffixOf` n] $ \n ->
    try (removeFile (dir </> n)) :: IO (Either SomeException ())

-- | Run the action with SIGINT and SIGTERM turned into 'UserInterrupt' in
-- this thread (the shell's line editor, or GHC's own handler, would send
-- them elsewhere), so a running step's group is killed; the old handlers
-- come back after.
interruptible :: IO a -> IO a
interruptible act = newIORef False >>= \p -> interruptibleWith p act

-- | 'interruptible', setting the flag as a signal comes in (before the
-- exception is thrown), so the action can tell a failure that the signal
-- caused from one of its own.
interruptibleWith :: IORef Bool -> IO a -> IO a
interruptibleWith pending act = do
  tid <- myThreadId
  let h = Catch (atomicWriteIORef pending True >> throwTo tid UserInterrupt)
  bracket
    ((,) <$> installHandler sigINT h Nothing <*> installHandler sigTERM h Nothing)
    (\(i, t) -> installHandler sigINT i Nothing >> installHandler sigTERM t Nothing)
    (const act)
