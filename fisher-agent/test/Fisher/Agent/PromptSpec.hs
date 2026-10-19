-- | 'Fisher.Agent.Prompt' against @agent/prompt_builder.py@
-- (@test/golden/b3/prompt.json@): the context-files section for a range
-- of project layouts and SOUL.md contents (and what building it creates
-- in the Fisher home), the injection scan, truncation, and the constants.
module Fisher.Agent.PromptSpec (spec) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Prompt
import Fisher.Agent.TestUtil
import Fisher.Py.Value

listTree :: FilePath -> IO [FilePath]
listTree root = do
  ex <- doesDirectoryExist root
  if not ex then pure [] else go ""
  where
    go rel = do
      names <- listDirectory (root </> rel)
      fmap concat . forM names $ \n -> do
        let r = if null rel then n else rel </> n
        d <- doesDirectoryExist (root </> r)
        if d then (r :) <$> go r else pure [r]

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b3/prompt.json")
  describe "agent/prompt_builder.py" $ do
    it "has upstream's constants" $ do
      PStr defaultAgentIdentity `shouldBe` field "identity" doc
      PStr memoryGuidance `shouldBe` field "memory_guidance" doc
      PStr sessionSearchGuidance `shouldBe` field "session_search_guidance" doc
      PStr skillsGuidance `shouldBe` field "skills_guidance" doc
      PStr defaultSoulMd `shouldBe` field "default_soul" doc
      PDict [(k, PStr v) | (k, v) <- platformHints] `shouldBe` field "platform_hints" doc
    forM_ (zip [1 :: Int ..] (list "cases" doc)) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "kind" c)) $
      case txt "kind" c of
        "context_files" -> withSystemTempDirectory "fisher-prompt" $ \tmp -> do
          let work = tmp </> "work"
              home = tmp </> "home"
          createDirectory work
          case field "layout" c of
            PDict kvs -> forM_ kvs $ \(rel, v) -> do
              let p = work </> T.unpack rel
              createDirectoryIfMissing True (takeDirectory p)
              BS.writeFile p (TE.encodeUtf8 (pyStrOf v))
            _ -> pure ()
          case field "soul" c of
            PStr soul -> createDirectory home >> BS.writeFile (home </> "SOUL.md") (TE.encodeUtf8 soul)
            _ -> pure ()
          out <- buildContextFilesPrompt (const (pure ())) home work
          out `shouldBe` txt "prompt" c
          created <- sort <$> listTree home
          map T.pack created `shouldBe` [s | PStr s <- list "created" c]
          case field "soul_after" c of
            PStr s -> do
              b <- BS.readFile (home </> "SOUL.md")
              TE.decodeUtf8 b `shouldBe` s
            _ -> pure ()
        "scan" -> fst (scanContextContent (txt "content" c) "AGENTS.md") `shouldBe` txt "out" c
        "truncate" -> do
          let n = fromIntegral (int "length" c)
              content = T.replicate (n `div` 2) "ab" <> T.replicate (n `mod` 2) "c"
              out = truncateContent content "X.md"
          fromIntegral (T.length out) `shouldBe` int "out_len" c
          T.take 130 (T.drop 13990 out) `shouldBe` txt "out_head" c
        k -> expectationFailure ("unknown kind " <> T.unpack k)
