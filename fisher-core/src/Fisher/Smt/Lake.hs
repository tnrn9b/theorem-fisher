-- | Pure-ish helpers for a Lean project that may use lean-smt: reading
-- @lake-manifest.json@ and @lean-toolchain@, writing the @require@ line into
-- the lakefile, and managing the @import Smt@ marker line in a Lean file.
-- Nothing here runs a solver or lake.
module Fisher.Smt.Lake
  ( hasSmt
  , hasMathlib
  , leanToolchain
  , LakeFile (..)
  , findLakeFile
  , requireSmtText
  , hasSmtRequire
  , addSmtRequire
  , withSmtRequire
  , HeaderKind (..)
  , headerKind
  , importSmtLine
  , insertImportSmt
  , stripImportSmt
  ) where

import Control.Exception (SomeException, finally, onException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)

-- | True when the manifest lists a top-level package with this name.
-- A missing, unreadable or differently shaped manifest (including the old
-- version 6 shape, where the name sits under a @git@ key) gives False.
hasPackage :: Text -> FilePath -> IO Bool
hasPackage name root = do
  r <- try (BS.readFile (root </> "lake-manifest.json")) :: IO (Either SomeException BS.ByteString)
  pure $ case r of
    Left _ -> False
    Right bs -> case A.decodeStrict bs of
      Just (A.Object o) | Just (A.Array ps) <- KM.lookup (K.fromText "packages") o -> any named (V.toList ps)
      _ -> False
  where
    named (A.Object p) = KM.lookup (K.fromText "name") p == Just (A.String name)
    named _ = False

hasSmt :: FilePath -> IO Bool
hasSmt = hasPackage "smt"

hasMathlib :: FilePath -> IO Bool
hasMathlib = hasPackage "mathlib"

-- | The trimmed contents of @lean-toolchain@.
leanToolchain :: FilePath -> IO (Maybe Text)
leanToolchain root = do
  r <- try (BS.readFile (root </> "lean-toolchain")) :: IO (Either SomeException BS.ByteString)
  pure $ case r of
    Left _ -> Nothing
    Right bs ->
      let t = T.strip (TE.decodeUtf8With TEE.lenientDecode bs)
       in if T.null t then Nothing else Just t

data LakeFile = LakeLean FilePath | LakeToml FilePath
  deriving (Eq, Show)

-- | @lakefile.lean@ wins over @lakefile.toml@ when both exist (lake's own order).
findLakeFile :: FilePath -> IO (Maybe LakeFile)
findLakeFile root = do
  let l = root </> "lakefile.lean"
      t = root </> "lakefile.toml"
  hasL <- doesFileExist l
  if hasL
    then pure (Just (LakeLean l))
    else do
      hasT <- doesFileExist t
      pure (if hasT then Just (LakeToml t) else Nothing)

-- | The require declaration for lean-smt at a revision.
requireSmtText :: LakeFile -> Text -> Text
requireSmtText (LakeLean _) rev =
  "require smt from git \"https://github.com/ufmg-smite/lean-smt.git\" @ \"" <> rev <> "\""
requireSmtText (LakeToml _) rev =
  T.intercalate "\n"
    [ "[[require]]"
    , "name = \"smt\""
    , "scope = \"ufmg-smite\""
    , "rev = \"" <> rev <> "\""
    ]

-- | Does the lakefile text already require lean-smt?
hasSmtRequire :: Text -> Bool
hasSmtRequire t =
  "lean-smt" `T.isInfixOf` t
    || any isNameSmt (T.lines t)
  where
    isNameSmt l = T.filter (/= ' ') (T.strip l) == "name=\"smt\""

-- | The lakefile text with the require declaration added: before Mathlib's
-- require when there is one (so Mathlib's versions of the packages both
-- depend on take precedence, as lake advises), with the comment lines just
-- above it kept with it, else appended after one blank line. Mathlib's is
-- the @[[require]]@ block holding @name = "mathlib"@ in a toml lakefile and
-- the @require@ line naming mathlib in a lean one.
withSmtRequire :: LakeFile -> Text -> Text -> Text
withSmtRequire lf rev old = case mathlibAt of
  Nothing -> T.stripEnd old <> "\n\n" <> block <> "\n"
  Just i ->
    let at = backOverComments i
     in T.intercalate "\n" (take at ls <> T.lines block <> [""] <> drop at ls)
  where
    block = requireSmtText lf rev
    ls = T.splitOn "\n" old
    indexed = zip [0 :: Int ..] ls
    mathlibAt = case lf of
      LakeToml _ ->
        case [i | (i, l) <- indexed, T.strip l == "[[require]]", any isNameMathlib (tableBody i)] of
          (i : _) -> Just i
          [] -> Nothing
      LakeLean _ -> case [i | (i, l) <- indexed, requiresMathlibLine l] of
        (i : _) -> Just i
        [] -> Nothing
    -- the lines of the table starting at i, up to the next table header
    tableBody i = takeWhile (not . ("[" `T.isPrefixOf`) . T.strip) (drop (i + 1) ls)
    isNameMathlib l = T.filter (/= ' ') (T.strip l) == "name=\"mathlib\""
    requiresMathlibLine l =
      let toks = map (T.dropAround (== '"')) (T.words l)
       in take 1 toks == ["require"] && "mathlib" `elem` takeWhile (`notElem` ["from", "@"]) (drop 1 toks)
    comment = case lf of
      LakeToml _ -> "#"
      LakeLean _ -> "--"
    backOverComments i
      | i > 0, (comment `T.isPrefixOf` T.strip (ls !! (i - 1))) = backOverComments (i - 1)
      | otherwise = i

-- | Add the require declaration ('withSmtRequire'). The new text goes to a
-- temp file beside the lakefile (with its mode), which is then renamed
-- over it. Left when already present, when the file is not valid UTF-8, or
-- when it cannot be read or written.
addSmtRequire :: LakeFile -> Text -> IO (Either Text ())
addSmtRequire lf rev = do
  let path = case lf of LakeLean p -> p; LakeToml p -> p
  r <- try $ do
    bs <- BS.readFile path
    case TE.decodeUtf8' bs of
      Left _ -> pure (Left "lakefile is not valid UTF-8")
      Right old
        | hasSmtRequire old -> pure (Left "lakefile already requires smt")
        | otherwise -> do
            let new = TE.encodeUtf8 (withSmtRequire lf rev old)
            st <- getFileStatus path
            (tmp, h) <- openBinaryTempFile (takeDirectory path) (takeFileName path <> ".tmp")
            (BS.hPut h new `finally` hClose h) `onException` removeFile tmp
            -- the temp file is created 0600; keep the lakefile's own mode
            setFileMode tmp (fileMode st) `onException` removeFile tmp
            renameFile tmp path `onException` removeFile tmp
            pure (Right ())
  pure $ case r of
    Left e -> Left (T.pack (show (e :: SomeException)))
    Right x -> x

importSmtLine :: Text
importSmtLine = "import Smt -- fisher smt"

-- | What a Lean file's header starts with: a plain file, a @prelude@ file,
-- or a @module@ file (the module system, where Fisher adds no marker).
data HeaderKind = Plain | Prelude | Module
  deriving (Eq, Show)

-- | The kind of a Lean file's header, from its first header line.
headerKind :: Text -> HeaderKind
headerKind t = let (k, _, _) = scanHeader (T.splitOn "\n" t) in k

-- | Is this an import line (@import@, @import all@, @public import@,
-- @meta import@, @public meta import@)? Gives the words after @import@.
importWords :: Text -> Maybe [Text]
importWords l = case T.words l of
  ("import" : r) -> Just r
  ("public" : "import" : r) -> Just r
  ("meta" : "import" : r) -> Just r
  ("public" : "meta" : "import" : r) -> Just r
  _ -> Nothing

-- | Scan the header of a Lean file: blank lines, @--@ comments and
-- @/- ... -/@ blocks (including @/-! ... -/@) are skipped, a leading
-- @module@ or @prelude@ line is noted, import lines are collected, and the
-- first other line ends the header. Gives the header kind, the index of the
-- line the marker goes after (the last import, else the @prelude@ line) and
-- whether @import Smt@ is among the imports.
scanHeader :: [Text] -> (HeaderKind, Maybe Int, Bool)
scanHeader = go (0 :: Int) (0 :: Int) Nothing Nothing False
  where
    -- line index, comment depth, kind so far, marker anchor, has Smt
    go _ _ k lastI hasS [] = (fromMaybe Plain k, lastI, hasS)
    go i d k lastI hasS (l : rest)
      | d > 0 = go (i + 1) (blockDepth d l) k lastI hasS rest
      | T.null s || "--" `T.isPrefixOf` s = go (i + 1) d k lastI hasS rest
      | "/-" `T.isPrefixOf` s = go (i + 1) (blockDepth d s) k lastI hasS rest
      | Nothing <- k, firstWord == Just "module" = (Module, Nothing, hasS)
      | Nothing <- k, firstWord == Just "prelude" =
          go (i + 1) d (Just Prelude) (Just i) hasS rest
      | Just ws <- importWords l, T.stripStart l == l =
          go (i + 1) d (Just (fromMaybe Plain k)) (Just i) (hasS || take 1 (dropWhile (== "all") ws) == ["Smt"]) rest
      | otherwise = (fromMaybe Plain k, lastI, hasS)
      where
        s = T.strip l
        firstWord = case T.words s of (w : _) -> Just w; [] -> Nothing

-- | The comment nesting depth after a line, starting from depth @d0@.
blockDepth :: Int -> Text -> Int
blockDepth d0 = walk d0 . T.unpack
  where
    walk d ('/' : '-' : r) = walk (d + 1) r
    walk d ('-' : '/' : r) = walk (max 0 (d - 1)) r
    walk d (_ : r) = walk d r
    walk d [] = d

-- | Add the marker line after the last header import line, else after a
-- @prelude@ line, else at the top. A file that already imports Smt in its
-- header, or whose header starts with @module@, is returned unchanged.
insertImportSmt :: Text -> Text
insertImportSmt t
  | hasS || kind == Module = t
  | otherwise = case lastI of
      Nothing -> importSmtLine <> "\n" <> t
      Just i -> T.intercalate "\n" (take (i + 1) ls ++ [importSmtLine] ++ drop (i + 1) ls)
  where
    ls = T.splitOn "\n" t
    (kind, lastI, hasS) = scanHeader ls

-- | Remove the marker line only; a user's own @import Smt@ stays.
stripImportSmt :: Text -> Text
stripImportSmt = T.intercalate "\n" . filter ((/= importSmtLine) . T.stripEnd) . T.splitOn "\n"
