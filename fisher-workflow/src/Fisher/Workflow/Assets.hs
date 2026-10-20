-- | The lean4-skills checkout under @<state>/assets/lean4-skills@: making
-- sure it is there and complete, and which revision it is.
module Fisher.Workflow.Assets
  ( lean4SkillsUrl
  , LeanPaths (..)
  , leanPaths
  , checkoutRoot
  , checkoutIsComplete
  , readCheckoutRevision
  , ensureLean4Checkout
  , ensureGitCheckout
  , removeExistingPath
  ) where

import Control.Monad (filterM, when)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory hiding (isSymbolicLink)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (getSymbolicLinkStatus, isDirectory, isSymbolicLink)
import Control.Exception (IOException, try)

import Fisher.Workflow.Run (runStaging)
import Fisher.Workflow.Types

lean4SkillsUrl :: Text
lean4SkillsUrl = "https://github.com/cameronfreer/lean4-skills.git"

data LeanPaths = LeanPaths
  { lpPlugin :: FilePath
  , lpSkill :: FilePath
  , lpScripts :: FilePath
  , lpReferences :: FilePath
  }
  deriving (Eq, Show)

checkoutRoot :: FilePath -> FilePath
checkoutRoot assets = assets </> "lean4-skills"

leanPaths :: FilePath -> LeanPaths
leanPaths co =
  let plugin = co </> "plugins" </> "lean4"
      skill = plugin </> "skills" </> "lean4"
   in LeanPaths plugin skill (plugin </> "lib" </> "scripts") (skill </> "references")

missingPaths :: FilePath -> IO [FilePath]
missingPaths co =
  let LeanPaths a b c d = leanPaths co
   in filterM (fmap not . doesDirectoryExist) [a, b, c, d]

checkoutIsComplete :: FilePath -> IO Bool
checkoutIsComplete co = null <$> missingPaths co

revisionFile :: FilePath -> FilePath
revisionFile co = co </> ".fisher-managed-revision"

readCheckoutRevision :: FilePath -> IO (Maybe Text)
readCheckoutRevision co = do
  let f = revisionFile co
  exists <- doesFileExist f
  if not exists then pure Nothing else do
    r <- try (TIO.readFile f) :: IO (Either IOException Text)
    pure $ case T.strip <$> r of
      Right v | not (T.null v) -> Just v
      _ -> Nothing

writeCheckoutRevision :: FilePath -> Text -> IO ()
writeCheckoutRevision co rev = TIO.writeFile (revisionFile co) (T.strip rev <> "\n")

-- | @_remove_existing_path@: a link or file is unlinked, a directory removed
-- with its contents, and a missing path left alone.
removeExistingPath :: FilePath -> IO ()
removeExistingPath p = do
  st <- tryIO (getSymbolicLinkStatus p)
  case st of
    Left _ -> pure ()
    Right s
      | isSymbolicLink s -> removeFile p
      | isDirectory s -> removeDirectoryRecursive p
      | otherwise -> removeFile p

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | Clone or refresh a checkout at a revision (or the default branch) and
-- return its HEAD. Git runs with this process's environment.
ensureGitCheckout :: Text -> Maybe Text -> FilePath -> FilePath -> IO Text
ensureGitCheckout url revision dest git = do
  isFile <- doesFileExist dest
  when isFile $ throwW (StagingError ("Managed asset path is not a directory: " <> T.pack dest))
  hasGit <- doesPathExist (dest </> ".git")
  let g args prefix = () <$ runStaging (git : args) prefix Nothing Nothing
  if hasGit
    then do
      g ["-C", dest, "fetch", "--depth", "1", "origin", T.unpack (fromMaybe "HEAD" revision)] "Failed to refresh managed asset checkout"
      g ["-C", dest, "checkout", "--force", "FETCH_HEAD"] "Failed to check out the managed asset revision"
    else do
      removeExistingPath dest
      createDirectoryIfMissing True (takeDirectory dest)
      case revision of
        Just rev -> do
          g ["clone", "--filter=blob:none", "--no-checkout", T.unpack url, dest] ("Failed to clone managed asset from " <> url)
          g ["-C", dest, "fetch", "--depth", "1", "origin", T.unpack rev] "Failed to fetch the managed asset revision"
          g ["-C", dest, "checkout", "--force", "FETCH_HEAD"] "Failed to check out the managed asset revision"
        Nothing ->
          g ["clone", "--depth", "1", "--filter=blob:none", T.unpack url, dest] ("Failed to clone managed asset from " <> url)
  T.strip <$> runStaging [git, "-C", dest, "rev-parse", "HEAD"] "Failed to read managed asset revision" Nothing Nothing

-- | @_ensure_lean4_checkout_assets@: refresh when asked, when a revision is
-- requested by @FISHER_AUTOFORMALIZE_LEAN4_SKILLS_REF@, or when the checkout
-- is incomplete; otherwise reuse it and its recorded revision.
ensureLean4Checkout :: FilePath -> Env -> FilePath -> Bool -> IO (FilePath, Text)
ensureLean4Checkout assets env git refresh = do
  let co = checkoutRoot assets
      ref = case T.strip (T.pack (fromMaybe "" (Map.lookup "FISHER_AUTOFORMALIZE_LEAN4_SKILLS_REF" env))) of
        t | T.null t -> Nothing
        t -> Just t
  complete <- checkoutIsComplete co
  rev <-
    if refresh || ref /= Nothing || not complete
      then do
        r <- ensureGitCheckout lean4SkillsUrl ref co git
        writeCheckoutRevision co r
        pure r
      else readCheckoutRevision co >>= \case
        Just r -> pure r
        Nothing -> do
          r <- T.strip <$> runStaging [git, "-C", co, "rev-parse", "HEAD"] "Failed to read managed asset revision" Nothing Nothing
          writeCheckoutRevision co r
          pure r
  missing <- missingPaths co
  case missing of
    [] -> pure (co, rev)
    ms -> throwW (StagingError ("Managed Lean bundle is incomplete after checkout: " <> T.intercalate ", " (map T.pack ms)))

