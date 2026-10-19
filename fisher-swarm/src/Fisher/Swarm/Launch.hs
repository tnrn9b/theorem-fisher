-- | How a Claude Code child is launched under the swarm: permission prompts
-- off, and the working directory pre-trusted (@_normalize_claude_session_launch@
-- and @_ensure_workspace_trusted@ in @swarm_manager.py@).
module Fisher.Swarm.Launch
  ( normalizeClaudeLaunch
  , ensureWorkspaceTrusted
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Aeson (decodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeFileName, (</>))

import Fisher.Compat (childEnvCompat)
import Fisher.Py (pyTruthy)
import Fisher.Workflow.Types (Env)

-- | Mark @cwd@ trusted in @<home>/.claude.json@ (the running user's home,
-- not the managed one), so Claude Code skips its trust dialog.
ensureWorkspaceTrusted :: FilePath -> FilePath -> IO ()
ensureWorkspaceTrusted home cwd = do
  let path = home </> ".claude.json"
  exists <- doesFileExist path
  raw <- if exists then either (const Nothing) Just <$> (try (BS.readFile path) :: IO (Either IOException BS.ByteString)) else pure Nothing
  let dat = case raw of
        Just bs | Just (Object o) <- decodeStrict bs -> o
        _ -> KM.empty
  resolved <- T.pack <$> canonicalizePath cwd
  let projects = case KM.lookup "projects" dat of
        Just (Object p) -> p
        _ -> KM.empty
      entry = case KM.lookup (K.fromText resolved) projects of
        Just (Object e) -> e
        _ -> KM.empty
      accepted k = maybe False pyTruthy (KM.lookup k entry)
  if accepted "hasTrustDialogAccepted" && accepted "hasTrustDialogHooksAccepted"
    then pure ()
    else do
      let entry' = KM.insert "hasTrustDialogHooksAccepted" (Bool True)
                 . KM.insert "hasTrustDialogAccepted" (Bool True)
                 . (if KM.member "allowedTools" entry then id else KM.insert "allowedTools" (Array V.empty))
                 $ entry
          dat' = KM.insert "projects" (Object (KM.insert (K.fromText resolved) (Object entry') projects)) dat
      _ <- try (BL.writeFile path (P.encodePretty' P.defConfig {P.confIndent = P.Spaces 2} (Object dat'))) :: IO (Either IOException ())
      pure ()

-- | For a @claude@ argv: drop any permission flags, put the bypass back at
-- position 1, and set @FISHER_YOLO_MODE@ and @GAUSS_YOLO_MODE@. Anything
-- else is left alone.
normalizeClaudeLaunch :: Bool -> FilePath -> [Text] -> Env -> Maybe FilePath -> IO ([Text], Env)
normalizeClaudeLaunch isRoot home argv env cwd = case argv of
  [] -> pure (argv, env)
  (exe : rest)
    | takeFileName (T.unpack exe) /= "claude" -> pure (argv, env)
    | otherwise -> do
        mapM_ (ensureWorkspaceTrusted home) (cwd >>= \c -> if null c then Nothing else Just c)
        let strip = \case
              [] -> []
              ("--dangerously-skip-permissions" : xs) -> strip xs
              ("--permission-mode" : xs) -> strip (drop 1 xs)
              (x : xs) | "--permission-mode=" `T.isPrefixOf` x -> strip xs
              (x : xs) -> x : strip xs
            flags = if isRoot then ["--permission-mode", "bypassPermissions"] else ["--dangerously-skip-permissions"]
        pure (exe : flags <> strip rest, foldr (uncurry Map.insert) env (childEnvCompat [("FISHER_YOLO_MODE", "1")]))
