module Fisher.Tools.TestUtil
  ( loadDoc
  , withIsolatedHome
  , cases
  , txt
  , field
  , strList
  , intOf
  , boolOf
  , listOf
  ) where

import Control.Exception (finally)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import Data.Text (Text)

import Fisher.Py.Value

loadDoc :: FilePath -> IO PyVal
loadDoc p = either fail pure . loads =<< BS.readFile p

cases :: PyVal -> [PyVal]
cases d = listOf (field "cases" d)

field :: Text -> PyVal -> PyVal
field k v = maybe PNull id (get k v)

txt :: Text -> PyVal -> Text
txt k v = case get k v of
  Just (PStr s) -> s
  _ -> ""

strList :: PyVal -> [Text]
strList v = [s | PStr s <- listOf v]

listOf :: PyVal -> [PyVal]
listOf = \case
  PList xs -> xs
  _ -> []

intOf :: PyVal -> Integer
intOf = \case
  PInt n -> n
  _ -> 0

boolOf :: PyVal -> Bool
boolOf = \case
  PBool b -> b
  _ -> False

-- | Run with HOME at a fresh empty directory and FISHER_HOME and GAUSS_HOME
-- unset (all restored afterwards), so a hint that names the Fisher home in
-- use says ~/.fisher whatever the machine has. It mutates the process
-- environment; hspec runs these examples one at a time (no @parallel@).
withIsolatedHome :: IO a -> IO a
withIsolatedHome act = withSystemTempDirectory "fisher-home" $ \tmp -> do
  let names = ["HOME", "FISHER_HOME", "GAUSS_HOME"]
  saved <- mapM (\k -> (,) k <$> lookupEnv k) names
  let restore = mapM_ (\(k, v) -> maybe (unsetEnv k) (setEnv k) v) saved
  (do setEnv "HOME" tmp; unsetEnv "FISHER_HOME"; unsetEnv "GAUSS_HOME"; act) `finally` restore
