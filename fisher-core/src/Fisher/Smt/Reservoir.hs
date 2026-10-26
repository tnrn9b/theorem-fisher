-- | The lean-smt revision to pin in a project, from Reservoir's record of
-- the builds it ran: the Reservoir index keeps, for each package, a
-- @builds.json@ of @{ "data": [ { "built", "toolchain", "revision",
-- "runAt", ... } ] }@. A revision Reservoir built on the project's own
-- toolchain is the one to pin.
module Fisher.Smt.Reservoir
  ( buildsUrl
  , parseBuilds
  , validRevision
  , pickRevision
  , fetchBuilds
  , fetchBuildsWith
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, evaluate, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), withFile)
import System.Process (CreateProcess (..), StdStream (..), proc, terminateProcess, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

-- | lean-smt's builds in the Reservoir index.
buildsUrl :: String
buildsUrl = "https://raw.githubusercontent.com/leanprover/reservoir-index/master/ufmg-smite/smt/builds.json"

-- | A revision Fisher will write into a lakefile: @main@, @no_mathlib@, or a
-- commit of 7 to 40 lowercase hex digits. Anything else (quotes, spaces, a
-- tag) is refused, since the text goes into the lakefile as it is.
validRevision :: Text -> Bool
validRevision r =
  r `elem` ["main", "no_mathlib"]
    || (T.length r >= 7 && T.length r <= 40 && T.all (`elem` ("0123456789abcdef" :: String)) r)

-- | The successful builds as @(revision, toolchain)@, newest run first
-- (each pair once). A build whose revision fails 'validRevision' is left out.
parseBuilds :: BS.ByteString -> Either Text [(Text, Text)]
parseBuilds bs = case A.decodeStrict bs of
  Just (A.Object o) | Just (A.Array ds) <- KM.lookup (K.fromText "data") o ->
    Right (nub (map snd (sortOn (Down . fst) [x | Just x <- map entry (V.toList ds)])))
  _ -> Left "not a Reservoir builds file"
  where
    entry = \case
      A.Object e
        | Just (A.Bool True) <- KM.lookup (K.fromText "built") e
        , Just (A.String rev) <- KM.lookup (K.fromText "revision") e
        , validRevision rev
        , Just (A.String tc) <- KM.lookup (K.fromText "toolchain") e ->
            let runAt = case KM.lookup (K.fromText "runAt") e of
                  Just (A.String t) -> t
                  _ -> ""
             in Just (runAt, (rev, tc))
      _ -> Nothing

-- | The revision to pin, from the project's toolchain, the builds (newest
-- first) and whether the project has Mathlib. A build on the project's
-- toolchain gives its commit; otherwise the branch to fall back to, which
-- the caller warns about: @no_mathlib@ for a project without Mathlib (the
-- builds are of the Mathlib branch), else @main@.
pickRevision :: Maybe Text -> [(Text, Text)] -> Bool -> Either Text Text
pickRevision _ _ False = Left "no_mathlib"
pickRevision Nothing _ True = Left "main"
pickRevision (Just tc) builds True =
  case [rev | (rev, t) <- builds, norm t == norm tc] of
    (rev : _) -> Right rev
    [] -> Left "main"
  where
    norm t = let s = T.strip t in maybe s id (T.stripPrefix "leanprover/lean4:" s)

-- | 'fetchBuildsWith' @curl@ and 'buildsUrl'.
fetchBuilds :: IO (Either Text [(Text, Text)])
fetchBuilds = fetchBuildsWith "curl" buildsUrl

-- | GET the builds file with @curl@ (10 s, stdin from @/dev/null@, at most
-- 1 MB read) and parse it. A missing curl, a non-zero exit (with the first
-- line curl wrote to stderr) or a body that does not parse is a 'Left'.
-- Only I/O errors are caught: an interrupt (Ctrl-C) goes through.
fetchBuildsWith :: FilePath -> String -> IO (Either Text [(Text, Text)])
fetchBuildsWith curl url = do
  r <- try $ withFile "/dev/null" ReadMode $ \devNull ->
    withCreateProcess
      (proc curl ["-fsSL", "--show-error", "--max-time", "10", "--max-filesize", show cap, url])
        {std_in = UseHandle devNull, std_out = CreatePipe, std_err = CreatePipe}
      $ \_ mout merr ph -> case (mout, merr) of
        (Just out, Just err) -> do
          errVar <- newEmptyMVar
          -- a closed handle (after a kill) reads as nothing
          _ <- forkIO (try (BS.hGet err 4096) >>= putMVar errVar . either (\(_ :: IOException) -> BS.empty) id)
          body <- timeout (15 * 1000000) (BS.hGet out (cap + 1) >>= evaluate)
          case body of
            Nothing -> terminateProcess ph >> pure (Left "curl timed out")
            Just b
              | BS.length b > cap -> terminateProcess ph >> pure (Left "the builds file is over 1 MB")
              | otherwise ->
                  waitForProcess ph >>= \case
                    ExitSuccess -> pure (parseBuilds b)
                    ExitFailure n -> do
                      msg <- T.strip . T.takeWhile (/= '\n') . TE.decodeUtf8With TEE.lenientDecode <$> takeMVar errVar
                      pure (Left ("curl exited with " <> T.pack (show n) <> (if T.null msg then "" else ": " <> msg)))
        _ -> pure (Left "no output from curl")
  pure $ case r of
    Left e -> Left (T.pack (show (e :: IOException)))
    Right x -> x
  where
    cap = 1024 * 1024
