-- | The Lake project, library lemma and retrieval settings the retrieval
-- specs share.
module Fisher.Search.RetrievalFixture (project, bumpLemma, retrievalIn) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Fisher.Search.Facts (defaultFacts)
import Fisher.Search.Index
import Fisher.Search.Library
import Fisher.Search.Retrieval

-- | A Lake project: P/Defs.lean with @P.bump@ and a theorem about it, and
-- the target file F.lean importing it.
project :: FilePath -> Text -> IO ()
project root target = do
  createDirectoryIfMissing True (root </> "P")
  BS.writeFile (root </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage p\nlean_lib P\n"
  BS.writeFile (root </> "P" </> "Defs.lean") $ TE.encodeUtf8 $ T.unlines
    [ "namespace P", ""
    , "def bump (n : Nat) : Nat := n + 1", ""
    , "theorem bump_pos (n : Nat) : 0 < bump n := Nat.succ_pos n", ""
    , "end P" ]
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 target)

-- | A library lemma about @P.bump@ from an earlier run.
bumpLemma :: Lemma
bumpLemma = Lemma "P.bump_eq" "theorem P.bump_eq (n : Nat) : P.bump n = n + 1 :=" " by rfl" "Other.lean" "Other" "run0" "2026-10-01T00:00:00Z" ["P.bump"] []

-- | Retrieval over the project at the root: the index built there, the
-- library given, a library directory under the root.
retrievalIn :: Maybe Library -> FilePath -> IO RetrievalEnv
retrievalIn lib root = do
  (ix, _) <- buildIndex root (root </> "index.json")
  pure (RetrievalEnv ix (Just defaultFacts) lib True (root </> "lib") root (root </> "run") retrievalCutoff mempty)

