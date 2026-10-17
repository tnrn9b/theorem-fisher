-- | @gauss setup@ against upstream's recorded pty runs
-- (@tools/corpus/b1_auth.py@, @setup.json@): each case is replayed through
-- a scripted 'Sys' (typed input, HTTP answers, subprocess results, clock)
-- and compared on the transcript, exit code, HTTP requests, subprocesses,
-- browser, sleeps and every file left behind.
module Fisher.SetupSpec (spec, goldenWith) where

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Cli.Setup (SetupInfo (..), setupMainWith)
import Fisher.LLM.Replay
import Fisher.LLM.Sys
import Fisher.Py.Value

-- | The recording masks upstream's interpreter and checkout.
info :: SetupInfo
info = SetupInfo "<VENV>/bin/python" "<SRC>"

dispatch :: PyVal -> Sys -> IO Int
dispatch c sys = case maybe [] (\case PList xs -> [pyStrOf x | x <- xs]; _ -> []) (get "argv" c) of
  (_ : "setup" : rest) -> setupMainWith info sys rest
  argv -> fail ("no command for " <> show argv)

httpView :: PyVal -> PyVal
httpView e =
  obj
    [ (k, norm k v)
    | k <- ["via", "method", "url", "headers", "body", "timeout", "status", "unmatched", "raised"]
    , Just v <- [get k e] ]
  where
    norm "timeout" (PInt n) = PFloat (fromInteger n)
    norm _ v = v

listOf :: T.Text -> PyVal -> [PyVal]
listOf k c = case get k c of
  Just (PList xs) -> xs
  _ -> []

-- | Every comparison for one recording file, with the command to run.
goldenWith :: FilePath -> (PyVal -> Sys -> IO Int) -> Spec
goldenWith file run = do
  (doc, cases) <- runIO (loadCases ("test/golden/b1/" <> file))
  let soul = maybe "" pyStrOf (get "soul_md" doc)
  describe file $
    forM_ cases $ \(Case c) -> it (T.unpack (maybe "?" pyStrOf (get "name" c))) $ do
      (w, o) <- replay doc Nothing c (run c)
      files <- snapshotFiles w soul
      oTranscript o `shouldBe` maybe "" pyStrOf (get "transcript" c)
      oExit o `shouldBe` maybe 0 (\case PInt n -> fromInteger n; _ -> -1) (get "exit" c)
      map httpView (oHttp o) `shouldBe` map httpView (httpLog c)
      oSubprocess o `shouldBe` [[pyStrOf a | a <- listOf "argv" s] | s <- listOf "subprocess" c]
      oBrowser o `shouldBe` map pyStrOf (listOf "browser" c)
      let sleeps = [s | Just s <- map (get "seconds") (listOf "sleeps" c)]
      unless (null sleeps) $
        map PFloat (oSleeps o) `shouldBe` map (\case PInt n -> PFloat (fromInteger n); v -> v) sleeps
      files `shouldBe` expectedFiles c

spec :: Spec
spec = goldenWith "setup.json" dispatch
