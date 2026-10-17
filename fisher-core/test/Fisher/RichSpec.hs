-- | Fisher.Rich against Rich 15.0.0 itself: random markup, tables and
-- panels recorded by @tools/corpus/a3.py@ (@rich.json@), each rebuilt
-- here from its description and printed at the same width.
module Fisher.RichSpec (spec, specFromCases, buildSpec, rendered', getV) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

import Fisher.Rich.Render
import Fisher.Rich.Text

get :: FromJSON a => Text -> Value -> a
get k (Object o) =
  either (error . ((T.unpack k <> ": ") <>)) id (parseEither parseJSON (fromMaybe Null (KM.lookup (K.fromText k) o)))
get k _ = error ("not an object: " <> T.unpack k)

pad4 :: [Int] -> Pad
pad4 = \case
  [a, b] -> (a, b, a, b)
  [a, b, c, d] -> (a, b, c, d)
  [a] -> (a, a, a, a)
  _ -> (0, 0, 0, 0)

-- | The renderable a case describes.
buildSpec :: Value -> Renderable
buildSpec v = case get "kind" v :: Text of
  "text" ->
    let t = fromMarkup (get "markup" v)
     in RTxt t { tJustify = get "justify" v >>= parseJustify
               , tOverflow = get "overflow" v >>= parseOverflow
               , tNoWrap = if get "no_wrap" v then Just True else Nothing }
  "table" ->
    let cols =
          [ (column (get "header" c))
              { cNoWrap = get "no_wrap" c
              , cMaxWidth = get "max_width" c
              , cMinWidth = get "min_width" c
              , cJustify = fromMaybe JLeft (parseJustify (get "justify" c))
              , cWidth = get "width" c
              , cRatio = get "ratio" c
              , cOverflow = get "overflow" c >>= parseOverflow }
          | c <- get "columns" v :: [Value] ]
        cell = \case
          String s -> markup s
          o -> buildSpec o
        rows = [map cell r | r <- get "rows" v :: [[Value]]]
        base = (table cols rows)
          { tbTitle = get "title" v
          , tbBox = (get "box" v :: Maybe Text) >>= boxByName
          , tbShowHeader = get "show_header" v
          , tbPadding = pad4 (get "padding" v)
          , tbPadEdge = get "pad_edge" v
          , tbCollapsePadding = get "collapse_padding" v
          , tbExpand = get "expand" v
          , tbShowEdge = get "show_edge" v
          , tbShowLines = get "show_lines" v }
     in RTable base
  "panel" ->
    RPanel (panel (buildSpec (get "body" v)))
      { pBox = fromMaybe boxRounded (boxByName (get "box" v))
      , pTitle = get "title" v
      , pPadding = pad4 (get "padding" v)
      , pExpand = get "expand" v }
  k -> error ("unknown kind " <> T.unpack k)

rendered :: Value -> Text
rendered c =
  let width = get "width" c
      justify = (get "print" c :: Value) `seq` (case get "print" c of
        Object o | Just (String j) <- KM.lookup "justify" o -> parseJustify j
        _ -> Nothing)
      ls = printLines width justify (buildSpec (get "spec" c))
   in if null ls then "" else T.intercalate "\n" (map (T.stripEnd . linePlain) ls) <> "\n"

specFromCases :: [Value] -> Spec
specFromCases cases =
  forM_ (zip [0 :: Int ..] cases) $ \(i, c) -> do
    let spc = get "spec" c :: Value
    it ("case " <> show i <> " (" <> T.unpack (get "kind" spc) <> ", width " <> show (get "width" c :: Int) <> ")") $ do
      rendered c `shouldBe` get "output" c
      case get "measure" c :: Maybe [Int] of
        Just [mn, mx] -> measure (options (get "width" c)) (buildSpec spc) `shouldBe` (mn, mx)
        _ -> pure ()

spec :: Spec
spec = do
  cases <- runIO $ do
    r <- eitherDecodeFileStrict "test/golden/a3/rich.json"
    case r of
      Right (Object o) | Just (Array cs) <- KM.lookup "cases" o -> pure (V.toList cs)
      Right _ -> fail "rich.json: no cases"
      Left e -> fail e
  describe "Rich, against Rich 15.0.0" (specFromCases cases)

rendered' :: Value -> Text
rendered' = rendered

getV :: Text -> Value -> Text
getV = get
