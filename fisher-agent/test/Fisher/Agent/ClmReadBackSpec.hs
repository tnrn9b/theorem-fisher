-- | The mirror's read-back and the edit gate against the hand-written
-- CLM1 cases (test/golden/clm/readback.json and gate.json, from
-- tools/clm/cases.py; docs/clm.md sections 4, 5 and 7).
module Fisher.Agent.ClmReadBackSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (readHex)
import Test.Hspec

import Fisher.Agent.Context.EditGate (GateMode (..), accept)
import Fisher.Agent.Context.Mirror (Edit (..), ReadBack (..), Turn (..), readBack, render)
import Fisher.Agent.TestUtil
import Fisher.Py.Value

golden :: FilePath
golden = "test/golden/clm/"

turns :: PyVal -> [Turn]
turns v = [Turn (fromIntegral (int "id" t)) (field "message" t) | t <- plist v]
  where
    plist (PList xs) = xs
    plist _ = []

hexBytes :: PyVal -> BS.ByteString
hexBytes (PStr t) = BS.pack (go (T.unpack t))
  where
    go (a : b : rest) = fromIntegral (fst (head (readHex [a, b])) :: Int) : go rest
    go _ = []
hexBytes _ = BS.empty

spec :: Spec
spec = do
  doc <- runIO (loadDoc (golden <> "readback.json"))
  let sid = txt "session_id" doc
      fixture name = field name (field "fixtures" doc)
  describe "Fisher.Agent.Context.Mirror.readBack (readback.json)" $
    forM_ (list "cases" doc) $ \c -> it (show (txt "name" c) <> ": " <> show (txt "why" c)) $ do
      let fx = fixture (txt "fixture" c)
          editable = turns (field "editable" fx)
          counter0 = fromIntegral (int "counter" fx)
          expect = field "expect" c
          input = case (field "text" c, field "bytes_hex" c) of
            (PStr t, _) -> Just (TE.encodeUtf8 t)
            (_, h@(PStr _)) -> Just (hexBytes h)
            _ -> Nothing
          seen = case field "seen" c of
            PList xs -> [fromIntegral n | PInt n <- xs]
            _ -> map turnId editable
          want
            | field "rejected" expect == PBool True = NotUtf8
            | field "missing" expect == PBool True = Missing
            | field "edited" expect == PBool False = NoEdit (fromIntegral (int "kept" expect))
            | otherwise =
                Edited
                  Edit
                    { editTurns = turns (field "messages" expect), editNext = fromIntegral (int "counter" expect)
                    , editKept = fromIntegral (int "kept" expect), editMergeIntoLive = field "merge_into_live" expect == PBool True
                    , editWritten = [] }
          -- which ids count as written is checked in the scenarios (write-twice)
          unwritten = \case
            Edited e -> Edited e {editWritten = []}
            r -> r
      unwritten (readBack sid editable counter0 seen input) `shouldBe` want
  describe "the mirror of every expected result reads back unchanged (P2 on the cases)" $
    forM_ (list "cases" doc) $ \c -> it (show (txt "name" c)) $ do
      let expect = field "expect" c
          ts = turns (field "messages" expect)
          n = fromIntegral (int "counter" expect)
      readBack sid ts n (map turnId ts) (Just (TE.encodeUtf8 (render sid ts))) `shouldBe` NoEdit 0
  gdoc <- runIO (loadDoc (golden <> "gate.json"))
  describe "Fisher.Agent.Context.EditGate.accept (gate.json)" $ do
    let fxA = turns (field "editable" (fixture "A"))
        old = map turnMsg fxA
        live = case field "live" gdoc of
          PList xs -> xs
          _ -> []
        resultOf name = head [turns (field "messages" (field "expect" c)) | c <- list "cases" doc, txt "name" c == name]
    forM_ (list "cases" gdoc) $ \g -> it (show (txt "name" g)) $ do
      let mode = if txt "mode" g == "shrink" then Shrink else Fit
          new = map turnMsg (resultOf (txt "after" g))
      accept mode (int "limit" g) old new live `shouldBe` (field "accept" g == PBool True)
