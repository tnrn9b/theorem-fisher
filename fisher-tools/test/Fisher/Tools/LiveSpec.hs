-- | The file tools on a real local sandbox (bash on this machine) against
-- what upstream returned for the same calls on the same fixture tree
-- (live.json, paths shown as <ROOT> and <HOME>).
--
-- Where the answer depends on the machine rather than on Fisher, the
-- comparison is looser, and each loosening is named here:
--   * lint results are dropped (they depend on which of python, node, go
--     and rustfmt are installed);
--   * search hits and file lists are compared as sets (ripgrep's order
--     across files is not fixed, and BSD find has no -printf, so upstream
--     falls back to an unsorted listing there too);
--   * mv's own error text (GNU and BSD word it differently) is masked;
--   * "read tilde" is skipped (its suggestions list your home directory);
--   * "search files alias" (sorted pagination) is skipped where find has
--     no -printf.
module Fisher.Tools.LiveSpec (spec) where

import Control.Monad (forM_)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.List (sort, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word8)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Fisher.Py.Value
import Fisher.Tools.File
import Fisher.Tools.FileOps
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox.Local
import Fisher.Tools.TestUtil

b64 :: T.Text -> BS.ByteString
b64 t = BS.pack (go (map val (filter (/= '=') (T.unpack t))))
  where
    val c
      | c >= 'A' && c <= 'Z' = ord c - 65
      | c >= 'a' && c <= 'z' = ord c - 71
      | c >= '0' && c <= '9' = ord c + 4
      | c == '+' = 62
      | otherwise = 63
    go (a : b : c : d : rest) = let n = (a `shiftL` 18) .|. (b `shiftL` 12) .|. (c `shiftL` 6) .|. d in map w8 [n `shiftR` 16, n `shiftR` 8, n] <> go rest
    go [a, b, c] = let n = (a `shiftL` 18) .|. (b `shiftL` 12) .|. (c `shiftL` 6) in map w8 [n `shiftR` 16, n `shiftR` 8]
    go [a, b] = let n = (a `shiftL` 18) .|. (b `shiftL` 12) in [w8 (n `shiftR` 16)]
    go _ = []
    w8 :: Int -> Word8
    w8 x = fromIntegral (x .&. 255)

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b2/live.json")
  gnuFind <- runIO $ do
    (code, _, _) <- readProcessWithExitCode "find" ["/", "-maxdepth", "0", "-printf", ""] ""
    pure (code == ExitSuccess)
  home <- runIO getHomeDirectory
  describe "file tools on a local sandbox, against upstream" $
    forM_ (cases doc) $ \c -> do
      let name = txt "name" c
          skip = name == "read tilde" || (name == "search files alias" && not gnuFind)
      it (T.unpack name) $
        if skip
          then pendingWith "depends on this machine (see the module header)"
          else withSystemTempDirectory "fisher-live" $ \tmp -> do
            let root = tmp </> "root"
            forM_ (case field "fixture" doc of PDict kvs -> kvs; _ -> []) $ \(rel, f) -> do
              let p = root </> T.unpack rel
              createDirectoryIfMissing True (takeDirectory p)
              BS.writeFile p (b64 (txt "b64" f))
              setModificationTime p (posixSecondsToUTCTime (fromIntegral (intOf (field "mtime" f))))
            se <- defaultSandboxEnv
            sb <- newLocalSandbox se (T.pack root) 60 False
            scope <- homeWriteScope (T.pack home)
            ops <- newFileOps scope sb
            ft <- newFileTools (const (pure (Right ops))) (pure Nothing) (pure True)
            reg <- newRegistry
            registerFileTools reg ft
            let ctx = defaultCtx {tcTaskId = txt "task" c}
                unplace = T.replace (T.pack home) "<HOME>" . T.replace (T.pack root) "<ROOT>"
            forM_ (zip [1 :: Int ..] (listOf (field "steps" c))) $ \(i, s) -> do
              let args = replaceIn "<ROOT>" (T.pack root) (field "args" s)
              out <- handleFunctionCall reg ctx (txt "tool" s) args
              (i, normalize (maskMv (unplace out))) `shouldBe` (i, normalize (maskMv (txt "result" s)))
            tree <- listTree root
            let want = [(k, v) | (k, PStr v) <- case field "files_after" c of PDict kvs -> kvs; _ -> [], not ("__pycache__" `T.isInfixOf` k)]
            [(k, v) | (k, v) <- tree, not ("__pycache__" `T.isInfixOf` k)] `shouldBe` want
  where
    maskMv t = case T.breakOn ": mv: " t of
      (pre, rest) | not (T.null rest) -> pre <> ": mv: <message>" <> snd (T.breakOn "\\n\"" rest)
      _ -> t
    replaceIn k v = \case
      PStr s -> PStr (T.replace k v s)
      PList xs -> PList (map (replaceIn k v) xs)
      PDict xs -> PDict [(a, replaceIn k v b) | (a, b) <- xs]
      x -> x
    -- a result is JSON, sometimes followed by a hint
    normalize t =
      let (j, hint) = T.breakOn "\n\n[Hint" t
       in case loads (TE.encodeUtf8 j) of
            Right v -> (loosen v, hint)
            Left _ -> (PStr t, "")
    loosen = \case
      PDict kvs -> PDict [(k, if k `elem` ["matches", "files"] then sortList v else v) | (k, v) <- kvs, k /= "lint"]
      v -> v
    sortList = \case
      PList xs -> PList (map snd (sortOn fst [(dumpsUnicode x, x) | x <- xs]))
      v -> v

listTree :: FilePath -> IO [(T.Text, T.Text)]
listTree root = go ""
  where
    go rel = do
      names <- sort <$> listDirectory (root </> rel)
      concat
        <$> mapM
          ( \n -> do
              let r = if null rel then n else rel </> n
              isDir <- doesDirectoryExist (root </> r)
              if isDir
                then go r
                else do
                  b <- BS.readFile (root </> r)
                  pure [(T.pack r, decodeReplace b)]
          )
          names
    decodeReplace = TE.decodeUtf8With (\_ _ -> Just '\xfffd')
