{-# LANGUAGE TemplateHaskell #-}
-- | Where Fisher keeps its state. The directory is still @~/.gauss@ and is
-- still moved by @GAUSS_HOME@, so a Python OpenGauss install and Fisher read
-- the same files.
module Fisher.Paths
  ( getGaussHome
  , configPathIn
  , envPathIn
  , ensureGaussHomeAt
  , ensureGaussHome
  , expandUser
  , defaultSoul
  ) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import System.Directory (createDirectoryIfMissing, doesPathExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

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

-- | @$GAUSS_HOME@ when set and non-empty, else @~/.gauss@.
getGaussHome :: IO FilePath
getGaussHome = lookupEnv "GAUSS_HOME" >>= \case
  Just p | not (null p) -> expandUser p
  _ -> (</> ".gauss") <$> getHomeDirectory

configPathIn, envPathIn :: FilePath -> FilePath
configPathIn home = home </> "config.yaml"
envPathIn home = home </> ".env"

-- | The home and its four subdirectories, each owner-only, plus a SOUL.md
-- when there is none. An existing SOUL.md is never touched.
ensureGaussHomeAt :: FilePath -> IO ()
ensureGaussHomeAt home = do
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

ensureGaussHome :: IO FilePath
ensureGaussHome = do
  home <- getGaussHome
  ensureGaussHomeAt home
  pure home
