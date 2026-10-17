-- | Golden-text helper: compares against test/golden/graph/<name>, writing the file when it is missing.
module Fisher.Search.Golden (golden) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

golden :: FilePath -> Text -> Expectation
golden name actual = do
  let path = "test/golden/graph" </> name
  createDirectoryIfMissing True "test/golden/graph"
  there <- doesFileExist path
  if there
    then do
      expected <- TIO.readFile path
      actual `shouldBe` expected
    else TIO.writeFile path actual
