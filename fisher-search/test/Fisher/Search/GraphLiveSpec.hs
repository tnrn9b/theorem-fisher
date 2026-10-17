-- | The lemma graph against the real kernel: a Lean core project (no
-- Mathlib) with one open theorem, split by a scripted planner into two
-- lemmas (declared with @theorem@: core Lean has no @lemma@) that scripted
-- workers prove. Pending unless @lake@ is on PATH
-- (build the toolchain first: tools/lean-fixtures.sh installs the same one).
module Fisher.Search.GraphLiveSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (copyFile, createDirectoryIfMissing, findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Search.Check (AxiomPolicy (..), lakeLeanRun, standardAxioms)
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.GraphRun
import Fisher.Search.Scheduler
import Fisher.Search.Scripted (accept, planWith, withProof)

fixture :: FilePath
fixture = "test/fixtures/graph-proj"

-- | What each goal's attempt writes: the root's unchanged, the lemmas' proofs.
proofOf :: Text -> Maybe Text
proofOf = \case
  "l1" -> Just "intro a b; simp"
  "l2" -> Just "intro a b; omega"
  _ -> Nothing

spec :: Spec
spec = describe "runGraph on a Lean project" $ do
  lake <- runIO (findExecutable "lake")
  case lake of
    Nothing -> it "closes t through two lemmas by the real kernel" (pendingWith "lake is not on PATH")
    Just _ -> it "closes t through two lemmas by the real kernel" $ withSystemTempDirectory "graph-live" $ \root -> do
      createDirectoryIfMissing True root
      forM_ ["lakefile.toml", "lean-toolchain", "GraphProj.lean"] $ \f -> copyFile (fixture </> f) (root </> f)
      file <- TE.decodeUtf8 <$> BS.readFile (root </> "GraphProj.lean")
      let run sp = pure $ plainResult
            (Just (maybe (asStart sp) (\p -> withProof (asGoal sp) p (asStart sp)) (proofOf (asGoal sp))))
            (Just (ChildStats (Just 0) 0 Nothing Nothing Nothing (Just 0) Nothing Map.empty))
            ""
          ask role _ = pure $ case role of
            Planner -> Right
              ( planWith "theorem" [("l1", "∀ a b : Nat, a + b + 0 = a + b"), ("l2", "∀ a b : Nat, a + b = b + a")]
                  "theorem t (a b : Nat) : a + b + 0 = b + a" "rw [l1, l2]"
              , 0 )
            Verifier -> Right (accept, 0)
            Worker -> Left (AskFailed "no model")
          senv = SearchEnv run (lakeLeanRun 600) (AxiomPolicy standardAxioms False) root "GraphProj.lean"
                   (pure 0) (\_ -> pure ()) (\_ -> pure ()) Nothing Nothing (\_ -> pure Nothing)
          genv = GraphEnv senv ask (\_ -> pure ()) (\_ _ -> pure ())
      r <- timeout 900000000 (runGraph defaultGraphConfig genv file)
      case r of
        Nothing -> expectationFailure "the run did not end"
        Just (Left e) -> expectationFailure (T.unpack e)
        Just (Right rep) -> do
          grClosedRoots rep `shouldBe` 1
          grBase rep `shouldSatisfy` (not . T.isInfixOf "sorry")
