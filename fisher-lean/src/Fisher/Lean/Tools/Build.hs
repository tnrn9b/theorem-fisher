-- | @lean_build@ (lean-lsp-mcp @tools/build.py@, @build_utils.py@): stop
-- the project's language servers, run @lake clean@ / @lake exe cache get@
-- / @lake build@, then start a fresh server. Builds run as called (the
-- default @LEAN_BUILD_CONCURRENCY=allow@); progress notifications are not
-- sent.
module Fisher.Lean.Tools.Build
  ( buildTool
  , buildLog
  ) where

import Control.Exception (Exception, IOException, SomeException, displayException, finally, fromException, throwIO, try)
import Data.IORef
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Fisher.Lean.Client
import Fisher.Lean.Error (LeanError, errorText)
import Fisher.Lean.Process (ioErrorText)
import Fisher.Lean.Session
import Fisher.Lean.Tool
import Fisher.Py.Value

-- | @LakeBuildRunner._handle_line@ over a command's output: the kept log
-- lines and those that mention an error.
buildLog :: BS.ByteString -> ([Text], [Text])
buildLog out =
  let parts = T.splitOn "\n" (TE.decodeUtf8With TEE.lenientDecode out)
      ls = if not (null parts) && T.null (last parts) then init parts else parts
      kept = [l' | l <- ls, let l' = T.stripEnd l, not ("trace:" `T.isPrefixOf` l'), not ("LEAN_PATH=" `T.isInfixOf` l')]
   in (kept, [l | l <- kept, "error" `T.isInfixOf` T.toLower l])

buildTool :: Tool
buildTool =
  Tool "lean_build" "lsp_buildArguments"
    [nullable "lean_project_path" TStr PNull, opt "clean" TBool (PBool False), opt "fetch_cache" TBool (PBool False), opt "output_lines" TInt (PInt 20)]
    run
  where
    run s a = do
      root <- case arg a "lean_project_path" of
        PStr p | not (T.null p) -> bindProject s (T.unpack p)
        _ -> boundProject s >>= maybe (toolError "Lean project path not known yet. Provide `lean_project_path` explicitly or call another tool first.") pure
      Model <$> runBuild s root (argBool a "clean") (argBool a "fetch_cache") (argInt a "output_lines")

runBuild :: Session -> FilePath -> Bool -> Bool -> Int -> IO PyVal
runBuild s root clean fetch n = do
  logRef <- newIORef ([], [])
  let env = sessionEnv s
      lake args = do
        r <- try (leRunProcess env root ("lake" : args) Nothing)
        case r of
          Left (e :: IOException) -> ioFail (ioErrorText "lake" e)
          Right Nothing -> pure 1
          Right (Just (code, out)) -> let (k, e) = buildLog out in modifyIORef' logRef (\(k0, e0) -> (k0 <> k, e0 <> e)) >> pure code
      result ok errs = do
        (ls, _) <- readIORef logRef
        let shown
              | n == 0 = ""
              | n > 0 = T.intercalate "\n" (drop (length ls - n) ls)
              | otherwise = T.intercalate "\n" (drop (negate n) ls)
        pure (PDict [("success", PBool ok), ("output", PStr shown), ("errors", PList (map PStr errs))])
  withBuildFlag s root True
  r <- try @SomeException
    ( do
        stopProjectClients s root
        if clean then () <$ lake ["clean"] else pure ()
        if fetch then () <$ lake ["exe", "cache", "get"] else pure ()
        code <- lake ["build"]
        if code /= 0
          then do
            (_, errs) <- readIORef logRef
            result False (if null errs then ["Build failed with return code " <> T.pack (show code)] else errs)
          else do
            nmax <- maxOpenFiles s
            c <- startClient (leClientEnv env) root nmax (leRequestTimeout env)
            attachClient s root c
            result True [] )
    `finally` withBuildFlag s root False
  case r of
    Right v -> pure v
    Left e -> result False [exceptionStr e]
  where
    exceptionStr e
      | Just (BuildFailure t) <- fromException e = t
      | Just (lerr :: LeanError) <- fromException e = errorText lerr
      | otherwise = T.pack (displayException e)

newtype BuildFailure = BuildFailure Text
  deriving (Show)

instance Exception BuildFailure

ioFail :: Text -> IO a
ioFail = throwIO . BuildFailure
