{-# LANGUAGE TemplateHaskell #-}
-- | Where Fisher keeps its state: @~/.fisher@, moved by @FISHER_HOME@
-- ('getFisherHome'), and the name of a project's own directory
-- ('fisherDirName').
module Fisher.Paths
  ( getFisherHome
  , fisherHomeName
  , fisherDirName
  , fisherHomeWith
  , homeLabel
  , fisherHomeLabel
  , fisherHomeLabelWith
  , configPathIn
  , envPathIn
  , ensureFisherHomeAt
  , ensureFisherHome
  , expandUser
  , defaultSoul
  ) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import System.Directory (createDirectoryIfMissing, doesPathExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))

import Fisher.Atomic (secureDir, secureFile)

-- | The SOUL.md seeded into a new home, generated from upstream by
-- @tools/gen-defaults.py@.
defaultSoul :: ByteString
defaultSoul = $(makeRelativeToProject "data/default-soul.md" >>= embedFile)

-- | @~@ and @~/...@ only, which is all @Path.expanduser@ is asked to do here.
expandUser :: FilePath -> IO FilePath
expandUser "~" = getHomeDirectory
expandUser ('~' : rest@('/' : _)) = (<> rest) <$> getHomeDirectory
expandUser p = pure p

-- | The home directory's name under the user's home.
fisherHomeName :: FilePath
fisherHomeName = ".fisher"

-- | The project directory's name under a project root (@.fisher/project.yaml@).
fisherDirName :: FilePath
fisherDirName = ".fisher"

-- | @$FISHER_HOME@ when set and non-empty, else @~/.fisher@. Nothing is
-- created here.
getFisherHome :: IO FilePath
getFisherHome = fisherHomeWith lookupEnv

-- | 'getFisherHome' over a given environment (@HOME@ read from it too,
-- else the process's home directory).
fisherHomeWith :: (String -> IO (Maybe String)) -> IO FilePath
fisherHomeWith get = get "FISHER_HOME" >>= \case
  Just p | not (null p) -> expandUserIn get p
  _ -> (</> fisherHomeName) <$> userHomeWith get

userHomeWith :: (String -> IO (Maybe String)) -> IO FilePath
userHomeWith get = get "HOME" >>= \case
  Just h | not (null h) -> pure h
  _ -> getHomeDirectory

expandUserIn :: (String -> IO (Maybe String)) -> FilePath -> IO FilePath
expandUserIn get = \case
  "~" -> userHomeWith get
  '~' : rest@('/' : _) -> (<> rest) <$> userHomeWith get
  p -> pure p

-- | How a message names the Fisher home, given the user's home and the
-- Fisher home: @~/.fisher@ when it is that directory under the user's
-- home, else its path.
homeLabel :: FilePath -> FilePath -> FilePath
homeLabel userHome home
  | same (userHome </> fisherHomeName) = "~/" <> fisherHomeName
  | otherwise = home
  where
    norm = dropTrailingPathSeparator . normalise
    same p = norm p == norm home

-- | 'homeLabel' for this process: the user's home and 'getFisherHome'.
fisherHomeLabel :: IO FilePath
fisherHomeLabel = fisherHomeLabelWith lookupEnv

-- | 'homeLabel' over a given environment, as 'fisherHomeWith'.
fisherHomeLabelWith :: (String -> IO (Maybe String)) -> IO FilePath
fisherHomeLabelWith get = homeLabel <$> userHomeWith get <*> fisherHomeWith get

configPathIn, envPathIn :: FilePath -> FilePath
configPathIn home = home </> "config.yaml"
envPathIn home = home </> ".env"

-- | The home and its four subdirectories, each owner-only, plus a SOUL.md
-- when there is none. An existing SOUL.md is never touched.
ensureFisherHomeAt :: FilePath -> IO ()
ensureFisherHomeAt home = do
  createDirectoryIfMissing True home
  secureDir home
  forM_ ["cron", "sessions", "logs", "memories"] $ \sub -> do
    createDirectoryIfMissing True (home </> sub)
    secureDir (home </> sub)
  let soul = home </> "SOUL.md"
  present <- doesPathExist soul
  unless present $ do
    BS.writeFile soul defaultSoul
    secureFile soul

ensureFisherHome :: IO FilePath
ensureFisherHome = do
  home <- getFisherHome
  ensureFisherHomeAt home
  pure home
