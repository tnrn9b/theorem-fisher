-- | Skins against upstream's skin engine (@skins.json@).
module Fisher.SkinSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.A3Golden
import Fisher.Skin

spec :: Spec
spec = do
  doc <- runIO (head <$> a3Cases "skins.json")
  describe "built-in skins" $
    case obj "builtin" doc of
      Object o -> forM_ (KM.toList o) $ \(k, want) ->
        it (K.toString k) $ withSystemTempDirectory "fisher-skin" $ \home ->
          (skinView <$> loadSkin home (K.toText k)) `shouldReturn` want
      _ -> pure ()
  describe "user skins" $ do
    let files = case obj "user_files" doc of
          Object o -> [(K.toString k, s) | (k, String s) <- KM.toList o]
          _ -> []
        withHome act = withSystemTempDirectory "fisher-skin" $ \home -> do
          createDirectoryIfMissing True (home </> "skins")
          forM_ files $ \(f, s) -> TIO.writeFile (home </> "skins" </> f) s
          act home
    case obj "user" doc of
      Object o -> forM_ (KM.toList o) $ \(k, want) ->
        it ("loads " <> K.toString k) $ withHome $ \home -> (skinView <$> loadSkin home (K.toText k)) `shouldReturn` want
      _ -> pure ()
    it "lists built-ins, then user skins that do not shadow one" $ withHome $ \home -> do
      got <- listSkins home
      let want = [(str "name" e, str "description" e, str "source" e == "user") | e <- arr "list" doc]
      [(seName e, seDescription e, seUser e) | e <- got] `shouldBe` want
  where
    _ = T.empty
