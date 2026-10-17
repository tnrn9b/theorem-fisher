{-# LANGUAGE TemplateHaskell #-}
-- | What the shell takes from upstream as data (@tools/gen-defaults.py@):
-- the slash-command table, the banner art and the built-in skins.
module Fisher.Shell.Data
  ( upstreamVersion
  , upstreamReleaseDate
  , bannerArt
  , commandTable
  , agentOnlyCommands
  , fisherCommandTable
  , fisherCommands
  , builtinSkinValues
  ) where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Vector as V

bannerJson, skinsJson :: ByteString
bannerJson = $(makeRelativeToProject "data/banner.json" >>= embedFile)
skinsJson = $(makeRelativeToProject "data/skins.json" >>= embedFile)

banner :: KM.KeyMap Value
banner = case decodeStrict bannerJson of
  Just (Object o) -> o
  _ -> error "data/banner.json is malformed"
{-# NOINLINE banner #-}

str :: Text -> Text
str k = case KM.lookup (K.fromText k) banner of
  Just (String s) -> s
  _ -> error ("data/banner.json: no " <> show k)

upstreamVersion, upstreamReleaseDate :: Text
upstreamVersion = str "version"
upstreamReleaseDate = str "release_date"

-- | Logo, logo_ascii, hero, hero_ascii, compact, compact_ascii.
bannerArt :: Text -> Text
bannerArt = str

-- | @COMMANDS_BY_CATEGORY@, in upstream's order.
commandTable :: [(Text, [(Text, Text)])]
commandTable = case KM.lookup "commands" banner of
  Just (Array cats) -> map cat (V.toList cats)
  _ -> error "data/banner.json: no commands"
  where
    cat (Array v) | [String name, Array entries] <- V.toList v = (name, map entry (V.toList entries))
    cat _ = error "data/banner.json: bad category"
    entry (Array v) | [String c, String d] <- V.toList v = (c, d)
    entry _ = error "data/banner.json: bad command"

-- | Commands that drive upstream's own agent loop, which Fisher has not
-- got until the native agent (B3).
agentOnlyCommands :: [Text]
agentOnlyCommands =
  [ "/new", "/reset", "/history", "/save", "/retry", "/undo", "/title", "/compress", "/rollback"
  , "/stop", "/model", "/provider", "/verbose", "/reasoning", "/usage", "/paste" ]

-- | The table without them: what Fisher's help lists and matches against.
fisherCommandTable :: [(Text, [(Text, Text)])]
fisherCommandTable = [(c, [e | e@(n, _) <- es, n `notElem` agentOnlyCommands]) | (c, es) <- commandTable]

fisherCommands :: [Text]
fisherCommands = concatMap (map fst . snd) fisherCommandTable

-- | @_BUILTIN_SKINS@, in upstream's order.
builtinSkinValues :: [Value]
builtinSkinValues = case decodeStrict skinsJson of
  Just (Array v) -> V.toList v
  _ -> error "data/skins.json is malformed"
{-# NOINLINE builtinSkinValues #-}
