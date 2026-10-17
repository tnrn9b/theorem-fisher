-- | @fisher config path|env-path|check|migrate@, from @config_command@ in
-- @gauss_cli/config.py@.
module Fisher.Cli.ConfigCmd
  ( configCheck
  , configMigrate
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Config (loadConfigAt)
import Fisher.Config.Migrate
import Fisher.EnvVars
import Fisher.Term

present :: FilePath -> EnvVar -> IO Bool
present home v = maybe False (not . T.null) <$> getEnvValueAt home (evName v)

toolsSuffix :: Text -> EnvVar -> Text
toolsSuffix sep v = if null (evTools v) then "" else sep <> T.intercalate ", " (take 2 (evTools v))

-- | @gauss config check@.
configCheck :: Style -> FilePath -> IO [Text]
configCheck st home = do
  (current, latest) <- checkConfigVersionAt home
  required <- forM requiredEnvVars $ \v -> do
    p <- present home v
    pure (if p then "    ✓ " <> evName v else color st [red] ("    ✗ " <> evName v <> " (missing)"))
  optional <- forM optionalEnvVars $ \v -> do
    p <- present home v
    pure (if p then "    ✓ " <> evName v else color st [dim] ("    ○ " <> evName v <> toolsSuffix " → " v))
  (loaded, _) <- loadConfigAt home
  let missing = missingConfigFields loaded
      versionLine
        | current >= latest = "  Config version: " <> T.pack (show current) <> " ✓"
        | otherwise = color st [yellow] ("  Config version: " <> T.pack (show current) <> " → " <> T.pack (show latest) <> " (update available)")
  pure $
    [ "", color st [cyan, bold] "📋 Configuration Status", "", versionLine, ""
    , color st [bold] "  Required:" ] <> required
    <> ["", color st [bold] "  Optional:"] <> optional
    <> (if null missing then []
        else [ "", color st [yellow] ("  " <> T.pack (show (length missing)) <> " new config option(s) available")
             , "    Run 'fisher config migrate' to add them" ])
    <> [""]

-- | @gauss config migrate@: report, then migrate interactively.
configMigrate :: Style -> FilePath -> Prompter -> (Text -> IO ()) -> IO ()
configMigrate st home prompter say = do
  say ""
  say (color st [cyan, bold] "🔄 Checking configuration for updates...")
  say ""
  missingEnv <- fmap concat . forM (map (,True) requiredEnvVars <> map (,False) optionalEnvVars) $ \(v, req) -> do
    p <- present home v
    pure [(v, req) | not p]
  (loaded, _) <- loadConfigAt home
  let missingConfig = missingConfigFields loaded
  (current, latest) <- checkConfigVersionAt home
  if null missingEnv && null missingConfig && current >= latest
    then say (color st [green] "✓ Configuration is up to date!") >> say ""
    else do
      when (current < latest) $ say ("  Config version: " <> T.pack (show current) <> " → " <> T.pack (show latest))
      unless (null missingConfig) $
        say ("\n  " <> T.pack (show (length missingConfig)) <> " new config option(s) will be added with defaults")
      let req = [v | (v, True) <- missingEnv]
          opt = [v | (v, False) <- missingEnv, not (evAdvanced v)]
      unless (null req) $ do
        say ("\n  ⚠️  " <> T.pack (show (length req)) <> " required API key(s) missing:")
        forM_ req $ \v -> say ("     • " <> evName v)
      unless (null opt) $ do
        say ("\n  ℹ️  " <> T.pack (show (length opt)) <> " optional API key(s) not configured:")
        forM_ opt $ \v -> say ("     • " <> evName v <> (if null (evTools v) then "" else " (enables: " <> T.intercalate ", " (take 2 (evTools v)) <> ")"))
      say ""
      results <- migrateConfigAt home True False prompter say
      say ""
      when (not (null (envAdded results)) || not (null (configAdded results))) $
        say (color st [green] "✓ Configuration updated!")
      unless (null (migrationWarnings results)) $ do
        say ""
        forM_ (migrationWarnings results) $ \w -> say (color st [yellow] ("  ⚠️  " <> w))
      say ""
