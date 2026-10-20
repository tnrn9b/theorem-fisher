-- | Redaction, the MCP error sanitizer, fuzzy matching and the V4A parser
-- against upstream (redact.json, sanitize.json, fuzzy.json, v4a.json).
module Fisher.Tools.PureSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Py.Value
import Fisher.Tools.Fuzzy
import Fisher.Tools.Redact
import Fisher.Tools.TestUtil
import Fisher.Tools.V4a

spec :: Spec
spec = do
  red <- runIO (cases <$> loadDoc "test/golden/b2/redact.json")
  describe "redact_sensitive_text" $
    forM_ (zip [1 :: Int ..] red) $ \(i, c) -> it (show i <> " " <> show (T.take 40 (txt "text" c))) $ do
      let enabled = redactEnabled (case field "env" c of PNull -> Nothing; e -> Just (txt "FISHER_REDACT_SECRETS" e))
      (if enabled then redactSensitiveText (txt "text" c) else txt "text" c) `shouldBe` txt "redacted" c
  san <- runIO (cases <$> loadDoc "test/golden/b2/sanitize.json")
  describe "_sanitize_error" $
    forM_ (zip [1 :: Int ..] san) $ \(i, c) -> it (show i) $ sanitizeError (txt "text" c) `shouldBe` txt "sanitized" c
  fz <- runIO (cases <$> loadDoc "test/golden/b2/fuzzy.json")
  describe "fuzzy_find_and_replace" $
    forM_ (zip [1 :: Int ..] fz) $ \(i, c) -> it (show i <> " " <> show (T.take 30 (txt "old" c))) $ do
      let (newContent, n, err) = fuzzyFindAndReplace (txt "content" c) (txt "old" c) (txt "new" c) (boolOf (field "replace_all" c))
      (newContent, toInteger n, err) `shouldBe` (txt "result" c, intOf (field "count" c), case field "error" c of PStr e -> Just e; _ -> Nothing)
  vs <- runIO (cases <$> loadDoc "test/golden/b2/v4a.json")
  describe "parse_v4a_patch" $
    forM_ (zip [1 :: Int ..] vs) $ \(i, c) -> it (show i) $
      PList (map dumpOp (parseV4a (txt "patch" c))) `shouldBe` field "ops" c
  where
    dumpOp o =
      PDict
        [ ("op", PStr (case poType o of OpAdd -> "add"; OpUpdate -> "update"; OpDelete -> "delete"; OpMove -> "move"))
        , ("path", PStr (poPath o))
        , ("new_path", maybe PNull PStr (poNewPath o))
        , ("content", PNull)
        , ("hunks", PList [PDict [("hint", maybe PNull PStr (hHint h)), ("lines", PList [PList [PStr (T.singleton (hlPrefix l)), PStr (hlContent l)] | l <- hLines h])] | h <- poHunks o])
        ]
