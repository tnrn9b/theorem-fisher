-- | Upstream's skins (@gauss_cli/skin_engine.py@): the built-in ones, and
-- YAML files in @$GAUSS_HOME/skins/@, each filled in from the default.
module Fisher.Skin
  ( Skin (..)
  , buildSkin
  , loadSkin
  , listSkins
  , SkinEntry (..)
  , skinColor
  , skinBranding
  , defaultSkin
  , skinView
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Fisher.Shell.Data (builtinSkinValues)

data Skin = Skin
  { skName :: Text
  , skDescription :: Text
  , skColors :: KM.KeyMap Value
  , skSpinner :: KM.KeyMap Value
  , skBranding :: KM.KeyMap Value
  , skToolPrefix :: Value
  , skToolEmojis :: Value
  , skBannerLogo :: Text
  , skBannerLogoCompact :: Text
  , skBannerHero :: Text
  , skBannerHeroCompact :: Text
  }
  deriving (Eq, Show)

obj :: Text -> KM.KeyMap Value -> KM.KeyMap Value
obj k o = case KM.lookup (K.fromText k) o of
  Just (Object m) -> m
  _ -> KM.empty

txt :: Text -> KM.KeyMap Value -> Maybe Text
txt k o = case KM.lookup (K.fromText k) o of
  Just (String s) -> Just s
  _ -> Nothing

defaultData :: KM.KeyMap Value
defaultData = case [o | Object o <- builtinSkinValues, txt "name" o == Just "default"] of
  (o : _) -> o
  [] -> error "no default skin"

-- | @_build_skin_config@: the default's colours, spinner and branding,
-- updated by this skin's.
buildSkin :: KM.KeyMap Value -> Skin
buildSkin d =
  Skin
    { skName = fromMaybe "unknown" (txt "name" d)
    , skDescription = fromMaybe "" (txt "description" d)
    , skColors = obj "colors" d <> obj "colors" defaultData
    , skSpinner = obj "spinner" d <> obj "spinner" defaultData
    , skBranding = obj "branding" d <> obj "branding" defaultData
    , skToolPrefix = fromMaybe (fromMaybe (String "┊") (KM.lookup "tool_prefix" defaultData)) (KM.lookup "tool_prefix" d)
    , skToolEmojis = fromMaybe (Object KM.empty) (KM.lookup "tool_emojis" d)
    , skBannerLogo = fromMaybe "" (txt "banner_logo" d)
    , skBannerLogoCompact = fromMaybe "" (txt "banner_logo_compact" d)
    , skBannerHero = fromMaybe "" (txt "banner_hero" d)
    , skBannerHeroCompact = fromMaybe "" (txt "banner_hero_compact" d)
    }

defaultSkin :: Skin
defaultSkin = buildSkin defaultData

builtins :: [(Text, KM.KeyMap Value)]
builtins = [(n, o) | Object o <- builtinSkinValues, Just n <- [txt "name" o]]

-- | @_load_skin_from_yaml@: a mapping with a name, or nothing.
loadYaml :: FilePath -> IO (Maybe (KM.KeyMap Value))
loadYaml p = do
  r <- try (Y.decodeFileEither p) :: IO (Either SomeException (Either Y.ParseException Value))
  pure $ case r of
    Right (Right (Object o)) | KM.member "name" o -> Just o
    _ -> Nothing

-- | @load_skin@: the user's file by that name, else the built-in, else the
-- default.
loadSkin :: FilePath -> Text -> IO Skin
loadSkin gaussHome name = do
  let f = gaussHome </> "skins" </> T.unpack name <> ".yaml"
  isFile <- doesFileExist f
  user <- if isFile then loadYaml f else pure Nothing
  pure $ case user of
    Just d -> buildSkin d
    Nothing -> maybe defaultSkin buildSkin (lookup name builtins)

data SkinEntry = SkinEntry {seName :: Text, seDescription :: Text, seUser :: Bool}
  deriving (Eq, Show)

-- | @list_skins@: built-ins, then user skins (sorted by file) that do not
-- shadow one already listed.
listSkins :: FilePath -> IO [SkinEntry]
listSkins gaussHome = do
  let dir = gaussHome </> "skins"
      base = [SkinEntry n (fromMaybe "" (txt "description" o)) False | (n, o) <- builtins]
  isDir <- doesDirectoryExist dir
  files <- if isDir then sort . filter ((== ".yaml") . takeExtension) <$> listDirectory dir else pure []
  loaded <- mapM (\f -> fmap (\d -> (f, d)) <$> loadYaml (dir </> f)) files
  let add acc (f, d) =
        let n = fromMaybe (T.pack (take (length f - 5) f)) (txt "name" d)
         in if any ((== n) . seName) acc then acc else acc <> [SkinEntry n (fromMaybe "" (txt "description" d)) True]
  pure (foldl add base (mapMaybe id loaded))

skinColor :: Skin -> Text -> Text -> Text
skinColor s k fallback = case KM.lookup (K.fromText k) (skColors s) of
  Just (String c) -> c
  _ -> fallback

skinBranding :: Skin -> Text -> Text -> Text
skinBranding s k fallback = case KM.lookup (K.fromText k) (skBranding s) of
  Just (String c) -> c
  _ -> fallback

-- | The fields @tools/corpus/a3.py@ records for a skin.
skinView :: Skin -> Value
skinView s =
  object
    [ "name" .= skName s, "description" .= skDescription s, "colors" .= skColors s, "spinner" .= skSpinner s
    , "branding" .= skBranding s, "tool_prefix" .= skToolPrefix s, "tool_emojis" .= skToolEmojis s
    , "banner_logo" .= skBannerLogo s, "banner_hero" .= skBannerHero s
    , "banner_logo_compact" .= skBannerLogoCompact s, "banner_hero_compact" .= skBannerHeroCompact s ]
