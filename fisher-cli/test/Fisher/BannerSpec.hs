-- | The welcome banner against upstream's (@banner.json@), at each
-- recorded width, skin and project.
module Fisher.BannerSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Hspec

import Fisher.A3Golden
import Fisher.A3World
import Fisher.Rich.Render (linePlain)
import Fisher.Shell.Banner
import Fisher.Skin
import Fisher.Term (Style (..), renderText)

spec :: Spec
spec = do
  cases <- runIO (a3Cases "banner.json")
  describe "welcome banner" $
    forM_ cases $ \c -> it (T.unpack (str "name" c)) $ withTempDirectory "/tmp" "fa3b-" $ \tmp -> withTempDirectory "/var/tmp" "fa3g-" $ \tmpg -> do
      w <- makeWorld tmp tmpg
      let width = case obj "columns" c of
            Number n -> round n
            _ -> 80
          ascii = str "GAUSS_FORCE_ASCII" (obj "env" c) == "1"
          cfg = obj "config" c
          skinName = case str "skin" (obj "display" cfg) of
            "" -> "default"
            s -> s
          cwd = wRoot w </> T.unpack (str "cwd" c)
      skin <- loadSkin (wFisherHome w) skinName
      project <- projectSummaryAt w cwd
      let ls
            | width < 34 = compactBanner width (not ascii) skin
            | otherwise =
                welcomeBanner
                  (BannerInput width (not ascii) skin (str "model" c) (T.pack cwd) project
                     (Just "20261001_120000_abcdef") Nothing Nothing)
          out = T.concat [renderText (Style False (not ascii)) (T.stripEnd (linePlain l)) <> "\n" | l <- ls]
      normalizeWorld w out `shouldBe` str "output" c
  where
    _ = (Null :: Value)
