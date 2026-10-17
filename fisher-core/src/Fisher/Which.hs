-- | @shutil.which(name, path=PATH)@.
module Fisher.Which (whichIn, whichEnv) where

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, doesFileExist, executable, getPermissions)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Search the given PATH value. An empty PATH finds nothing, as upstream.
whichIn :: String -> String -> IO (Maybe FilePath)
whichIn pathValue name
  | null pathValue = pure Nothing
  | otherwise = do
      let dirs = filter (not . null) (splitColons pathValue)
      found <- filterM runnable [d </> name | d <- dirs]
      pure (case found of
        (p : _) -> Just p
        [] -> Nothing)
  where
    runnable p = do
      isFile <- doesFileExist p
      isDir <- doesDirectoryExist p
      if not isFile || isDir then pure False else executable <$> getPermissions p
    splitColons s = case break (== ':') s of
      (a, _ : rest) -> a : splitColons rest
      (a, []) -> [a]

-- | Search an environment's PATH, or this process's when it has none.
whichEnv :: [(String, String)] -> String -> IO (Maybe FilePath)
whichEnv env name = do
  pathValue <- case lookup "PATH" env of
    Just p -> pure p
    Nothing -> maybe "" id <$> lookupEnv "PATH"
  whichIn pathValue name
