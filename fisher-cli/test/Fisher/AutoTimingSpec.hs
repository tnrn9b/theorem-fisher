-- | A measurement, not a check: Auto's time per snippet call on a long
-- file, through the real language server ('openLsp'), with the whole file
-- and with the file cut after the goal (as the search sends it now). It
-- runs only when FISHER_AUTO_TIMING is set to ROOT|REL|GOAL (a Lake
-- project, its file, the goal's full name), and writes its lines to
-- FISHER_AUTO_TIMING_OUT (default /dev/stdout). FISHER_AUTO_TIMING_CAP is
-- the cap per call in seconds (default 300). No model is called.
module Fisher.AutoTimingSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Cli.SearchLsp (openLsp)
import Fisher.Search.Auto
import Fisher.Search.Lean (Decl (..), declarations, findDecl)

spec :: Spec
spec = do
  setting <- runIO (lookupEnv "FISHER_AUTO_TIMING")
  forM_ setting $ \v -> describe "Auto's time per snippet on a long file" $
    it "with the whole file, then cut after the goal" $ do
      (root, rel, goal) <- case T.splitOn "|" (T.pack v) of
        [a, b, c] -> pure (T.unpack a, T.unpack b, c)
        _ -> fail "FISHER_AUTO_TIMING is ROOT|REL|GOAL"
      cap <- maybe 300 read <$> lookupEnv "FISHER_AUTO_TIMING_CAP" :: IO Double
      out <- maybe "/dev/stdout" id <$> lookupEnv "FISHER_AUTO_TIMING_OUT"
      let say t = TIO.appendFile out (t <> "\n") >> TIO.putStrLn ("      " <> t)
          secs x = T.pack (showFFloat (Just 1) x "")
          timed act = do
            t0 <- getMonotonicTime
            r <- timeout (ceiling (cap * 1000000)) act
            t1 <- getMonotonicTime
            pure (t1 - t0, r)
      src <- TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile (root </> rel)
      d <- maybe (fail ("no goal " <> T.unpack goal)) pure (findDecl goal (declarations src))
      h <- case holesIn src d of
        (x : _) -> pure x
        [] -> fail "the goal has no hole"
      let cut = cutAtHole src h
      say ("file " <> T.pack rel <> ": " <> T.pack (show (length (T.lines src))) <> " lines; goal " <> goal
           <> " at line " <> T.pack (show (dStart d + 1)) <> "; cut text " <> T.pack (show (length (T.lines cut))) <> " lines; cap " <> secs cap <> " s per call")
      (tw, lsp0) <- timed (openLsp 3600 root rel)
      lsp <- case lsp0 of
        Just (Right l) -> pure l
        Just (Left e) -> fail ("the server did not start: " <> T.unpack e)
        Nothing -> fail "the server did not start within the cap"
      say ("warm-up (whole file): " <> secs tw <> " s")
      forM_ [("before (whole file)", src), ("after (cut after the goal)", cut)] $ \(mode, text) -> do
        (tb, rb) <- timed (lspDiagnostics lsp text)
        say (mode <> ": the text as it is: " <> secs tb <> " s" <> maybe " (cut off)" (either (" failed: " <>) (const "")) rb)
        forM_ defaultTactics $ \t -> do
          (ts, r) <- timed (lspAttempts lsp text h [wrapSnippet 200000 t])
          let what = case r of
                Nothing -> "cut off at the cap"
                Just (Left e) -> "failed: " <> e
                Just (Right trs) -> maybe "no fill" (const "closes") (winner trs)
          say (mode <> ": " <> t <> ": " <> secs ts <> " s (" <> what <> ")")
      lspClose lsp
