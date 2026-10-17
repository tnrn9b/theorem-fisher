-- | The small pieces of Python's standard library the tool layer's
-- behaviour depends on: @int()@ and @float()@ on command output and env
-- vars, @os.path@'s splitext, dirname, join, expanduser and realpath, and
-- the type names Python puts in error messages.
module Fisher.Tools.Py
  ( pyInt
  , pyFloat
  , pySplitExt
  , pyDirname
  , pyBasename
  , pyJoin
  , pyRealpath
  , pyTypeName
  , pyIsDigitStr
  , pyStrip
  , pySplitWs
  , pyBool
  , argText
  , argInt
  ) where

import Control.Exception (IOException, try)
import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, getSymbolicLinkTarget, pathIsSymbolicLink)
import Text.Read (readMaybe)

import Fisher.Py.Value (PyVal (..))

-- | @str.strip()@ with no argument: Unicode whitespace plus the ASCII
-- separators Python counts.
pyStrip :: Text -> Text
pyStrip = T.dropAround pyWs

pyWs :: Char -> Bool
pyWs c = isSpace c || c `elem` ("\x1c\x1d\x1e\x1f" :: String)

-- | @str.split()@ with no argument.
pySplitWs :: Text -> [Text]
pySplitWs = filter (not . T.null) . T.split pyWs

-- | @int(s)@ for a str: surrounding whitespace, an optional sign, ASCII
-- digits with single underscores between them.
pyInt :: Text -> Maybe Integer
pyInt raw =
  let s = pyStrip raw
      (sign, body) = case T.uncons s of
        Just ('-', r) -> (negate, r)
        Just ('+', r) -> (id, r)
        _ -> (id, s)
      groups = T.splitOn "_" body
   in if not (T.null body) && all (\g -> not (T.null g) && T.all isDigit g) groups
        then sign <$> readMaybe (T.unpack (T.concat groups))
        else Nothing

-- | @float(s)@ for a str: decimal and exponent forms, @inf@, @infinity@ and
-- @nan@ in any case, with whitespace and underscores as @int()@ allows.
pyFloat :: Text -> Maybe Double
pyFloat raw =
  let s = T.toLower (pyStrip raw)
      (sign, body) = case T.uncons s of
        Just ('-', r) -> (negate, r)
        Just ('+', r) -> (id, r)
        _ -> (id, s)
   in case body of
        "inf" -> Just (sign (1 / 0))
        "infinity" -> Just (sign (1 / 0))
        "nan" -> Just (0 / 0)
        _ -> sign <$> decimal (T.unpack body)
  where
    decimal str0 =
      let str = filter (/= '_') str0
          (ip, r1) = span isDigit str
          (fp, r2) = case r1 of
            ('.' : r) -> let (f, r') = span isDigit r in (f, r')
            _ -> ("", r1)
          hasDot = take 1 r1 == "."
          (ex, r3) = case r2 of
            (e : r) | e `elem` ("eE" :: String) ->
              let (sg, r') = case r of
                    ('-' : x) -> ("-", x)
                    ('+' : x) -> ("", x)
                    x -> ("", x)
                  (ds, r'') = span isDigit r'
               in if null ds then ("bad", r'') else (sg <> ds, r'')
            _ -> ("", r2)
       in if (null ip && null fp) || not (null r3) || ex == "bad" || '_' `elem` take 1 str0
            then Nothing
            else
              readMaybe
                ( (if null ip then "0" else ip)
                    <> (if hasDot || not (null fp) then "." <> (if null fp then "0" else fp) else "")
                    <> (if null ex then "" else "e" <> ex)
                )

-- | @os.path.splitext(p)[1]@: the last dot in the final component, unless
-- every dot before it is leading.
pySplitExt :: Text -> Text
pySplitExt p =
  let base = snd (T.breakOnEnd "/" p)
   in case T.breakOnEnd "." base of
        ("", _) -> ""
        (pre, ext) ->
          -- pre ends with the dot; a non-dot before it makes this an extension
          if T.any (/= '.') (T.dropEnd 1 pre) then "." <> ext else ""

-- | @os.path.dirname@ (posixpath).
pyDirname :: Text -> Text
pyDirname p =
  let (headPart, _) = T.breakOnEnd "/" p
   in if not (T.null headPart) && T.any (/= '/') headPart then T.dropWhileEnd (== '/') headPart else headPart

-- | @os.path.basename@ (posixpath).
pyBasename :: Text -> Text
pyBasename = snd . T.breakOnEnd "/"

-- | @os.path.join(a, b)@ (posixpath).
pyJoin :: Text -> Text -> Text
pyJoin a b
  | "/" `T.isPrefixOf` b = b
  | T.null a || "/" `T.isSuffixOf` a = a <> b
  | otherwise = a <> "/" <> b

-- | @os.path.realpath(p)@ (non-strict): absolute against the process cwd,
-- @.@ and @..@ folded, each existing symlink followed; components that do
-- not exist are kept as they are.
pyRealpath :: FilePath -> IO FilePath
pyRealpath p0 = do
  cwd <- getCurrentDirectory
  let abs0 = if take 1 p0 == "/" then p0 else cwd <> "/" <> p0
  go (40 :: Int) "/" (comps abs0)
  where
    comps = filter (\c -> not (null c) && c /= ".") . splitOnSlash
    splitOnSlash s = case break (== '/') s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOnSlash rest
    parent "/" = "/"
    parent d = case reverse (dropWhile (/= '/') (reverse d)) of
      "/" -> "/"
      x -> init x
    joinP "/" c = '/' : c
    joinP d c = d <> "/" <> c
    go _ acc [] = pure acc
    go n acc (c : rest)
      | c == ".." = go n (parent acc) rest
      | otherwise = do
          let candidate = joinP acc c
          isLink <- either (const False) id <$> (try (pathIsSymbolicLink candidate) :: IO (Either IOException Bool))
          if isLink && n > 0
            then do
              target <- getSymbolicLinkTarget candidate
              let base = if take 1 target == "/" then "/" else acc
              go (n - 1) base (comps target <> rest)
            else go n candidate rest

-- | @type(v).__name__@.
pyTypeName :: PyVal -> Text
pyTypeName = \case
  PNull -> "NoneType"
  PBool _ -> "bool"
  PInt _ -> "int"
  PFloat _ -> "float"
  PStr _ -> "str"
  PList _ -> "list"
  PDict _ -> "dict"

-- | @str.isdigit()@ over ASCII digits (all upstream sees in timestamps).
pyIsDigitStr :: Text -> Bool
pyIsDigitStr t = not (T.null t) && T.all isDigit t

-- | The lowercase-and-membership test upstream uses for boolean env vars:
-- @value.lower() in ("true", "1", "yes")@.
pyBool :: Text -> Bool
pyBool v = T.toLower v `elem` ["true", "1", "yes"]

-- | A string argument as the tool handler sees it.
argText :: PyVal -> Maybe Text
argText = \case
  PStr s -> Just s
  _ -> Nothing

-- | An integer argument (Python bools are ints).
argInt :: PyVal -> Maybe Integer
argInt = \case
  PInt n -> Just n
  PBool b -> Just (if b then 1 else 0)
  _ -> Nothing
