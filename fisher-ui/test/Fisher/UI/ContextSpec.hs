module Fisher.UI.ContextSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.SQLite.Simple (close, execute, open)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.UI.Fixture

key :: T.Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

spec :: Spec
spec = describe "GET /api/context/:id" $ do
  it "a CLM session gives now, prev, state and its clm_* events" $ withFixture $ \fx -> do
    (code, v) <- getJson (fxEnv fx) ("/api/context/" <> TE.encodeUtf8 s1)
    code `shouldBe` 200
    key "strategy" v `shouldBe` A.String "clm"
    key "now" v `shouldBe` A.String "goal: t\nproved"
    key "prev" v `shouldBe` A.String "goal: t"
    key "counter" (key "state" v) `shouldBe` A.Number 3
    let evs = arr (key "events" v)
    map (key "kind") evs `shouldBe` map A.String ["clm_write", "clm_spill", "clm_read"]
    -- a turn of its own (spill), else the model calls so far
    map (key "turn") evs `shouldBe` map A.Number [1, 2, 3]
    key "text" (key "detail" (head evs)) `shouldBe` A.String "goal: t"
    key "kind" (key "detail" (head evs)) `shouldBe` A.Null
    key "summaries" v `shouldBe` A.Array mempty
  it "a compressor child gives its [CONTEXT COMPACTION] summary" $ withFixture $ \fx -> do
    (code, v) <- getJson (fxEnv fx) ("/api/context/" <> TE.encodeUtf8 s3)
    code `shouldBe` 200
    key "strategy" v `shouldBe` A.String "compress"
    key "now" v `shouldBe` A.Null
    key "state" v `shouldBe` A.Null
    key "events" v `shouldBe` A.Array mempty
    case arr (key "summaries" v) of
      [x] -> do
        key "session" x `shouldBe` A.String s3
        key "text" x `shouldSatisfy` \case
          A.String t -> "[CONTEXT COMPACTION]" `T.isPrefixOf` t && "Proved two lemmas." `T.isInfixOf` t
          _ -> False
      xs -> expectationFailure ("one summary, got " <> show (length xs))
  it "the parent of a compression sees its child's summary" $ withFixture $ \fx -> do
    (_, v) <- getJson (fxEnv fx) ("/api/context/" <> TE.encodeUtf8 s2)
    map (key "session") (arr (key "summaries" v)) `shouldBe` [A.String s3]
  it "an unknown session is 404" $ withFixture $ \fx ->
    fst <$> getJson (fxEnv fx) "/api/context/nope" `shouldReturn` 404
  it "reads no ctx dir named by an unsafe id from a database row" $ withFixture $ \fx -> do
    -- a chain root whose id climbs out of fisher/ctx: its ctx dir would be fisher/
    B.writeFile (fxHome fx </> "fisher" </> "context.txt") "outside"
    w <- open (fxHome fx </> "state.db")
    execute w "INSERT INTO sessions (id, source, started_at, parent_session_id) VALUES (?, 'cli', 1790002001, ?)" ("20261001_120000_dddddd" :: T.Text, ".." :: T.Text)
    close w
    (code, v) <- getJson (fxEnv fx) "/api/context/20261001_120000_dddddd"
    code `shouldBe` 200
    key "now" v `shouldBe` A.Null
