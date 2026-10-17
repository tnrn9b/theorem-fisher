-- | @~/.gauss/.env@: reading it, and setting one key in it. Ported from
-- @load_env@ and @save_env_value@ in @gauss_cli/config.py@.
module Fisher.Env
  ( parseEnv
  , updateEnvText
  , validEnvName
  , loadEnvAt
  , saveEnvValueAt
  , saveEnvFileAt
  , EnvError (..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (doesFileExist)
import System.Posix.Env (setEnv)

import Fisher.Atomic (atomicWriteFile, secureFile)
import Fisher.Paths (ensureGaussHomeAt, envPathIn)

newtype EnvError = InvalidEnvName Text
  deriving (Eq, Show)

instance Exception EnvError

-- | Python's text-mode read: @\\r\\n@ and lone @\\r@ become @\\n@.
universalNewlines :: Text -> Text
universalNewlines = T.replace "\r" "\n" . T.replace "\r\n" "\n"

-- | @load_env@: every stripped line that is not blank, not a comment and
-- holds an @=@ gives a key and a value, the value with surrounding quotes of
-- either kind stripped. A later line for the same key wins.
parseEnv :: Text -> Map Text Text
parseEnv = foldl step Map.empty . T.lines . universalNewlines
  where
    step acc raw =
      let line = T.strip raw
       in if T.null line || "#" `T.isPrefixOf` line || not ("=" `T.isInfixOf` line)
            then acc
            else
              let (k, rest) = T.breakOn "=" line
                  v = T.dropAround (`elem` ['"', '\'']) (T.strip (T.drop 1 rest))
               in Map.insert (T.strip k) v acc

-- | @^[A-Za-z_][A-Za-z0-9_]*$@.
validEnvName :: Text -> Bool
validEnvName t = case T.uncons t of
  Just (c, rest) -> (alpha c || c == '_') && T.all (\x -> alpha x || isDigit x || x == '_') rest
  Nothing -> False
  where
    alpha x = isAsciiLower x || isAsciiUpper x

-- | The new file text after setting one key: the first line whose stripped
-- form starts with @KEY=@ is replaced, otherwise the line is appended, after
-- giving the last line its newline if it lacked one.
updateEnvText :: Text -> Text -> Text -> Text
updateEnvText key value old =
  let ls = splitKeepingNewlines (universalNewlines old)
      entry = key <> "=" <> value <> "\n"
      (before, after) = break (\l -> (key <> "=") `T.isPrefixOf` T.strip l) ls
   in case after of
        (_ : rest) -> T.concat (before <> [entry] <> rest)
        [] ->
          let fixed = case reverse ls of
                (lastLine : earlier) | not ("\n" `T.isSuffixOf` lastLine) ->
                  reverse ((lastLine <> "\n") : earlier)
                _ -> ls
           in T.concat (fixed <> [entry])

splitKeepingNewlines :: Text -> [Text]
splitKeepingNewlines t
  | T.null t = []
  | otherwise =
      let (line, rest) = T.breakOn "\n" t
       in if T.null rest then [line] else (line <> "\n") : splitKeepingNewlines (T.drop 1 rest)

readText :: FilePath -> IO Text
readText p = TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile p

loadEnvAt :: FilePath -> IO (Map Text Text)
loadEnvAt home = do
  let path = envPathIn home
  exists <- doesFileExist path
  if exists then parseEnv <$> readText path else pure Map.empty

-- | @save_env_value@: validate the name, drop newlines from the value, write
-- the file atomically with mode 0600, and set the variable in this process.
saveEnvValueAt :: FilePath -> Text -> Text -> IO ()
saveEnvValueAt home key value0 = do
  saveEnvFileAt home key value0
  setEnv (T.unpack key) (T.unpack (T.filter (\c -> c /= '\n' && c /= '\r') value0)) True

-- | 'saveEnvValueAt' without touching this process's environment, for
-- code whose environment is a 'Sys' (which sets it itself).
saveEnvFileAt :: FilePath -> Text -> Text -> IO ()
saveEnvFileAt home key value0 = do
  if validEnvName key then pure () else throwIO (InvalidEnvName key)
  let value = T.filter (\c -> c /= '\n' && c /= '\r') value0
  ensureGaussHomeAt home
  let path = envPathIn home
  exists <- doesFileExist path
  old <- if exists then readText path else pure ""
  atomicWriteFile ".env_.tmp" path (TE.encodeUtf8 (updateEnvText key value old))
  secureFile path
