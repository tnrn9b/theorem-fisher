-- | V4A multi-file patches (@tools.patch_parser@): the forgiving parser and
-- the applier, which edits through 'FileOps' exactly as upstream does,
-- including reading the file back through @read_file@ (line numbers
-- stripped, so lines over 2,000 characters arrive truncated) and silently
-- skipping hunks that only add lines.
module Fisher.Tools.V4a
  ( OpType (..)
  , HunkLine (..)
  , Hunk (..)
  , PatchOp (..)
  , parseV4a
  , applyV4a
  ) where

import Control.Exception (SomeException, displayException, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Re
import Fisher.Py.Value (PyVal (..))
import Fisher.Tools.FileOps
import Fisher.Tools.Fuzzy (fuzzyFindAndReplace)
import Fisher.Tools.Sandbox (ExecResult (..))

data OpType = OpAdd | OpUpdate | OpDelete | OpMove
  deriving (Eq, Show)

data HunkLine = HunkLine {hlPrefix :: Char, hlContent :: Text}
  deriving (Eq, Show)

data Hunk = Hunk {hHint :: Maybe Text, hLines :: [HunkLine]}
  deriving (Eq, Show)

data PatchOp = PatchOp
  { poType :: OpType
  , poPath :: Text
  , poNewPath :: Maybe Text
  , poHunks :: [Hunk]
  }
  deriving (Eq, Show)

updateRe, addRe, deleteRe, moveRe, hintRe, linenoRe :: Regex
updateRe = compileOrDie [] "\\*\\*\\*\\s*Update\\s+File:\\s*(.+)"
addRe = compileOrDie [] "\\*\\*\\*\\s*Add\\s+File:\\s*(.+)"
deleteRe = compileOrDie [] "\\*\\*\\*\\s*Delete\\s+File:\\s*(.+)"
moveRe = compileOrDie [] "\\*\\*\\*\\s*Move\\s+File:\\s*(.+?)\\s*->\\s*(.+)"
hintRe = compileOrDie [] "@@\\s*(.+?)\\s*@@"
linenoRe = compileOrDie [] "^\\s*\\d+\\|"

-- | @parse_v4a_patch@ (it never reports an error).
parseV4a :: Text -> [PatchOp]
parseV4a patch =
  let ls = T.splitOn "\n" patch
      indexed = zip [0 :: Int ..] ls
      isBegin l = "*** Begin Patch" `T.isInfixOf` l || "***Begin Patch" `T.isInfixOf` l
      startIdx = case [i | (i, l) <- indexed, isBegin l, i < endIdx] of
        [] -> -1
        xs -> last xs
      endIdx = case [i | (i, l) <- indexed, not (isBegin l), "*** End Patch" `T.isInfixOf` l || "***End Patch" `T.isInfixOf` l] of
        (i : _) -> i
        [] -> length ls
      body = take (endIdx - startIdx - 1) (drop (startIdx + 1) ls)
   in go body Nothing Nothing []
  where
    strip = T.strip
    -- (current op, current hunk), ops so far (reversed)
    flush op hunk acc = case op of
      Nothing -> acc
      Just o -> o {poHunks = poHunks o <> [h | Just h <- [hunk], not (null (hLines h))]} : acc
    go [] op hunk acc = reverse (flush op hunk acc)
    go (line : rest) op hunk acc
      | Just m <- matchStart updateRe line =
          go rest (Just (PatchOp OpUpdate (strip (groupOr "" 1 m)) Nothing [])) Nothing (flush op hunk acc)
      | Just m <- matchStart addRe line =
          go rest (Just (PatchOp OpAdd (strip (groupOr "" 1 m)) Nothing [])) (Just (Hunk Nothing [])) (flush op hunk acc)
      | Just m <- matchStart deleteRe line =
          go rest Nothing Nothing (PatchOp OpDelete (strip (groupOr "" 1 m)) Nothing [] : flush op hunk acc)
      | Just m <- matchStart moveRe line =
          go rest Nothing Nothing (PatchOp OpMove (strip (groupOr "" 1 m)) (Just (strip (groupOr "" 2 m))) [] : flush op hunk acc)
      | "@@" `T.isPrefixOf` line = case op of
          Just o ->
            let o' = o {poHunks = poHunks o <> [h | Just h <- [hunk], not (null (hLines h))]}
                hint = group 1 =<< matchStart hintRe line
             in go rest (Just o') (Just (Hunk hint [])) acc
          Nothing -> go rest op hunk acc
      | Just _ <- op, not (T.null line) =
          let h = fromMaybe (Hunk Nothing []) hunk
              add c t = Just h {hLines = hLines h <> [HunkLine c t]}
           in case T.head line of
                '+' -> go rest op (add '+' (T.tail line)) acc
                '-' -> go rest op (add '-' (T.tail line)) acc
                ' ' -> go rest op (add ' ' (T.tail line)) acc
                '\\' -> go rest op (Just h) acc
                _ -> go rest op (add ' ' line) acc
      | otherwise = go rest op hunk acc

-- | @apply_v4a_operations(operations, file_ops)@.
applyV4a :: FileOps -> [PatchOp] -> IO PatchResult
applyV4a fo ops = do
  results <- mapM one ops
  let modified = [p | (Right (Mod p), _) <- results]
      created = [p | (Right (Cre p), _) <- results]
      deleted = [p | (Right (Del p), _) <- results]
      diffs = [d | (Right _, d) <- results]
      errors = [e | (Left e, _) <- results]
  lints <- mapM (\f -> (,) f . lintResultDict <$> checkLint fo f) (modified <> created)
  let lint = if null lints then Nothing else Just (pyDict lints)
  pure
    emptyPatch
      { prSuccess = null errors
      , prDiff = T.intercalate "\n" diffs
      , prModified = modified
      , prCreated = created
      , prDeleted = deleted
      , prLint = lint
      , prError = if null errors then Nothing else Just (T.intercalate "; " errors)
      }
  where
    pyDict = PDict . foldl upsert []
    upsert acc (k, v)
      | any ((== k) . fst) acc = [(k', if k' == k then v else v') | (k', v') <- acc]
      | otherwise = acc <> [(k, v)]
    one op = do
      r <- try (apply op) :: IO (Either SomeException (Either Text (Done, Text)))
      pure $ case r of
        Left e -> (Left ("Error processing " <> poPath op <> ": " <> T.pack (displayException e)), "")
        Right (Left e) -> (Left (failMsg op <> e), "")
        Right (Right (d, diff)) -> (Right d, diff)
    failMsg op = case poType op of
      OpAdd -> "Failed to add " <> poPath op <> ": "
      OpDelete -> "Failed to delete " <> poPath op <> ": "
      OpMove -> "Failed to move " <> poPath op <> ": "
      OpUpdate -> "Failed to update " <> poPath op <> ": "
    apply op = case poType op of
      OpAdd -> do
        let contentLines = [hlContent l | h <- poHunks op, l <- hLines h, hlPrefix l == '+']
        w <- writeFileOp fo (poPath op) (T.intercalate "\n" contentLines)
        pure $ case wrError w of
          Just e -> Left e
          Nothing -> Right (Cre (poPath op), "--- /dev/null\n+++ b/" <> poPath op <> "\n" <> T.intercalate "\n" (map ("+" <>) contentLines))
      OpDelete -> do
        r <- readFileOp fo (poPath op) 1 500
        case rrError r of
          Just e | "not found" `T.isInfixOf` T.toLower e -> pure (Right (Del (poPath op), "# " <> poPath op <> " already deleted or doesn't exist"))
          _ -> do
            rm <- foExec fo ("rm -f " <> escapeShellArg (poPath op)) Nothing Nothing
            pure $
              if exCode rm /= 0
                then Left (exOutput rm)
                else Right (Del (poPath op), "--- a/" <> poPath op <> "\n+++ /dev/null\n# File deleted")
      OpMove -> do
        let newPath = fromMaybe "" (poNewPath op)
        mv <- foExec fo ("mv " <> escapeShellArg (poPath op) <> " " <> escapeShellArg newPath) Nothing Nothing
        pure $
          if exCode mv /= 0
            then Left (exOutput mv)
            else Right (Mod (poPath op <> " -> " <> newPath), "# Moved: " <> poPath op <> " -> " <> newPath)
      OpUpdate -> do
        r <- readFileOp fo (poPath op) 1 10000
        case rrError r of
          Just e -> pure (Left ("Cannot read file: " <> e))
          Nothing -> do
            let current = T.intercalate "\n" (map unnumber (T.splitOn "\n" (rrContent r)))
            case foldHunks current (poHunks op) of
              Left e -> pure (Left e)
              Right newContent -> do
                w <- writeFileOp fo (poPath op) newContent
                pure $ case wrError w of
                  Just e -> Left e
                  Nothing -> Right (Mod (poPath op), unifiedDiffText current newContent (poPath op))
    unnumber line = case matchStart linenoRe line of
      Just _ -> case T.breakOn "|" line of
        (_, rest) | not (T.null rest) -> T.drop 1 rest
        _ -> line
      Nothing -> line
    foldHunks content [] = Right content
    foldHunks content (h : hs) =
      let searchLs = [hlContent l | l <- hLines h, hlPrefix l /= '+']
          repl = [hlContent l | l <- hLines h, hlPrefix l /= '-']
          pat = T.intercalate "\n" searchLs
          rep = T.intercalate "\n" repl
       in if null searchLs
            then foldHunks content hs
            else case fuzzyFindAndReplace content pat rep False of
              (c', _, Nothing) -> foldHunks c' hs
              (_, _, Just err) -> case hHint h of
                Just hint
                  | (before, after) <- T.breakOn hint content
                  , not (T.null after) ->
                      let hintPos = T.length before
                          ws = max 0 (hintPos - 500)
                          we = min (T.length content) (hintPos + 2000)
                          window = T.take (we - ws) (T.drop ws content)
                       in case fuzzyFindAndReplace window pat rep False of
                            (wnew, n, _) | n > 0 -> foldHunks (T.take ws content <> wnew <> T.drop we content) hs
                            (_, _, err2) -> Left ("Could not apply hunk: " <> fromMaybe err err2)
                _ -> Left ("Could not apply hunk: " <> err)

data Done = Mod Text | Cre Text | Del Text
