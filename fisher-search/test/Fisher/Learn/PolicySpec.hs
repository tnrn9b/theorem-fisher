module Fisher.Learn.PolicySpec (spec) where

import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Exception (IOException, try)
import Data.Aeson (object)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Learn.Policy
import Fisher.Learn.Stats

-- | Stats of one project with each (knob, arm, cell) at every level, for
-- shape S, and the given close times for S.
uniform :: [(Knob, Text, Cell)] -> [Double] -> Stats
uniform cells closes =
  Stats
    { stCells =
        Map.fromList
          [ ((lvl, sh, k, arm), c)
          | (k, arm, c) <- cells
          , (lvl, sh) <- [(LShape, "S"), (LProject, ""), (LGlobalShape, "S"), (LGlobal, "")] ]
    , stCloseSeconds = Map.fromList [("S", map plain closes), ("", map plain closes)] }
  where
    plain x = (x, Nothing)

-- | n attempts, c closes, one dollar and 100 seconds each.
cell :: Int -> Int -> Cell
cell n c = Cell n c (fromIntegral n) (100 * fromIntegral n)

defaults :: Defaults
defaults = Defaults ["claude-code", "fisher"] ["m1", "m2"] ["rfl", "omega"] 1200

backendsFor :: Stats -> [Text]
backendsFor st = gpBackends (policyFor (computePolicy 0 "k" defaults st ["S"]) "S")

secsFor :: [Double] -> Maybe Int
secsFor closes = gpAttemptSecs (policyFor (computePolicy 0 "k" defaults (uniform [] closes) ["S"]) "S")

evidence :: Stats
evidence = uniform [(KBackend, "fisher", cell 10 8), (KBackend, "claude-code", cell 10 2)] []

spec :: Spec
spec = do
  describe "computePolicy" $ do
    it "puts the cheaper, better backend first" $
      backendsFor evidence `shouldBe` ["fisher", "claude-code"]

    it "does not reorder on 4 attempts" $
      backendsFor (uniform [(KBackend, "fisher", cell 4 4), (KBackend, "claude-code", cell 10 2)] [])
        `shouldBe` ["claude-code", "fisher"]

    it "does not reorder on a gap under 0.1" $ do
      let st = uniform [(KBackend, "fisher", cell 20 11), (KBackend, "claude-code", cell 20 10)] []
          gap = smoothedRate st "S" KBackend "fisher" - smoothedRate st "S" KBackend "claude-code"
      gap `shouldSatisfy` (\g -> g > 0.04 && g < 0.1)
      backendsFor st `shouldBe` ["claude-code", "fisher"]

    it "never brings in an arm that is not configured" $ do
      let st = uniform [(KBackend, "codex", Cell 10 10 0.1 100), (KBackend, "fisher", cell 10 8), (KBackend, "claude-code", cell 10 2)] []
      backendsFor st `shouldBe` ["fisher", "claude-code"]

    it "never brings in a tier or tactic that is not configured" $ do
      let st = uniform [(KTier, "m3", cell 10 10), (KTier, "m2", cell 10 9), (KTier, "m1", cell 10 1),
                        (KTactic, "decide", Cell 10 10 0 1), (KTactic, "omega", Cell 10 9 0 10), (KTactic, "rfl", Cell 10 1 0 10)] []
          gp = policyFor (computePolicy 0 "k" defaults st ["S"]) "S"
      (gpTiers gp, gpTactics gp) `shouldBe` (["m2", "m1"], ["omega", "rfl"])

    it "does not rank first an arm with no recorded cost, nor one never tried" $ do
      let d3 = defaults {dBackends = ["claude-code", "fisher", "codex", "other"]}
          st = uniform [(KBackend, "codex", Cell 10 0 0 1000), (KBackend, "fisher", Cell 10 8 1 1000), (KBackend, "claude-code", Cell 10 2 10 1000)] []
          bs = gpBackends (policyFor (computePolicy 0 "k" d3 st ["S"]) "S")
      take 1 bs `shouldBe` ["fisher"]
      last bs `shouldBe` "other"

    it "falls back to the global level when it is the only one with data" $ do
      let st = Stats (Map.fromList [((LGlobal, "", KBackend, "fisher"), cell 10 8), ((LGlobal, "", KBackend, "claude-code"), cell 10 2)]) Map.empty
      smoothedRate st "S" KBackend "fisher" `shouldBe` smoothedRateAt st LGlobal "" KBackend "fisher"
      dollarsPerAttempt st "S" KBackend "fisher" `shouldBe` Just 1
      closesPerHour st "S" KBackend "fisher" `shouldBe` 3600 * 8 / 1000
      backendsFor st `shouldBe` ["fisher", "claude-code"]

    it "orders tactics, which cost nothing, by closes per hour" $ do
      let st = uniform [(KTactic, "omega", Cell 10 9 0 10), (KTactic, "rfl", Cell 10 1 0 10)] []
      gpTactics (policyFor (computePolicy 0 "k" defaults st ["S"]) "S") `shouldBe` ["omega", "rfl"]

    it "orders tiers by model" $ do
      let st = uniform [(KTier, "m2", cell 10 9), (KTier, "m1", cell 10 1)] []
      gpTiers (policyFor (computePolicy 0 "k" defaults st ["S"]) "S") `shouldBe` ["m2", "m1"]

    it "sets the attempt budget to the 80th percentile of the seconds to close" $
      secsFor [100, 200 .. 1000] `shouldBe` Just 800

    it "keeps the budget at or above half the configured one" $
      secsFor [50, 60 .. 140] `shouldBe` Just 600

    it "rounds the floor up for an odd budget" $
      gpAttemptSecs (policyFor (computePolicy 0 "k" defaults {dAttemptSecs = 1201} (uniform [] [1, 2, 3, 4, 5]) ["S"]) "S")
        `shouldBe` Just 601

    it "never raises the budget" $
      secsFor [2000, 2100 .. 2900] `shouldBe` Just 1200

    it "leaves the budget alone with fewer than 5 closes" $
      secsFor [100, 200, 300, 400] `shouldBe` Nothing
    it "takes the percentile only from closes at the configured budget (a close with none is at it)" $ do
      let at b xs = [(x, b) | x <- xs]
          secsOf cs = gpAttemptSecs (policyFor (computePolicy 0 "k" defaults ((uniform [] []) {stCloseSeconds = Map.fromList [("S", cs)]}) ["S"]) "S")
      -- closes under a lowered budget of 600 s say nothing of the 1200 s one
      secsOf (at (Just 600) [550, 560 .. 590] <> at (Just 1200) [1100, 1150]) `shouldBe` Nothing
      secsOf (at (Just 600) [550, 560 .. 590] <> at (Just 1200) [1000, 1050 .. 1150] <> at Nothing [1180]) `shouldBe` Just 1150
      secsOf (at Nothing [100, 200 .. 1000]) `shouldBe` Just 800

    it "equals the defaults with no history, and with one attempt on a shape" $ do
      let one = uniform [(KBackend, "fisher", cell 1 1)] [60]
      mapM_
        (\st -> do
           let p = computePolicy 0 "k" defaults st ["S"]
           mapM_
             (\sh -> do
                let gp = policyFor p sh
                (gpBackends gp, gpTiers gp, gpTactics gp, gpAttemptSecs gp, gpExplore gp)
                  `shouldBe` (dBackends defaults, dTiers defaults, dTactics defaults, Nothing, False))
             ["S", "*"])
        [uniform [] [], one]

    it "falls back to the default entry for an unseen shape" $ do
      let p = computePolicy 0 "k" defaults evidence ["S"]
      Map.keys (pShapes p) `shouldBe` ["*", "S"]
      gpBackends (policyFor p "unseen") `shouldBe` gpBackends (policyFor p "*")

  describe "exploreNow" $
    it "explores every eighth attempt" $ do
      exploreNow 7 `shouldBe` True
      exploreNow 8 `shouldBe` False
      exploreNow 15 `shouldBe` True

  describe "the file" $ do
    it "lives under the Fisher home by project key" $
      policyPath "/h" "abc" `shouldBe` "/h/fisher/policy/abc.json"

    it "reads absent as Nothing and round trips" $ withSystemTempDirectory "policy" $ \tmp -> do
      let path = policyPath tmp "k"
          p = computePolicy 12.5 "k" defaults (evidence {stCloseSeconds = Map.fromList [("S", [(x, Nothing) | x <- [100, 200 .. 1000]])]}) ["S"]
      readPolicy path >>= (`shouldBe` Right Nothing)
      writePolicy path p
      readPolicy path >>= (`shouldBe` Right (Just p))

    it "is Left on garbage, an unknown version or an unknown knob" $ withSystemTempDirectory "policy" $ \tmp -> do
      let path = tmp </> "fisher" </> "policy" </> "k.json"
          entry = "{\"backends\":[],\"tiers\":[],\"tactics\":[],\"attempt_secs\":null,\"evidence\":{}"
      createDirectoryIfMissing True (takeDirectory path)
      mapM_
        (\body -> do
           BS.writeFile path body
           r <- readPolicy path
           r `shouldSatisfy` isLeft)
        [ "{\"garbage\": 1}"
        , "not json"
        , "{\"version\":99,\"computed_at\":0,\"project_key\":\"k\",\"shapes\":{\"*\":" <> entry <> "}}}"
        , "{\"computed_at\":0,\"project_key\":\"k\",\"shapes\":{\"*\":" <> entry <> "}}}"
        , "{\"version\":1,\"computed_at\":0,\"project_key\":\"k\",\"shapes\":{\"*\":" <> entry <> ",\"budget_usd\":3}}}" ]

    it "reads a shape entry without attempt_secs or evidence" $ withSystemTempDirectory "policy" $ \tmp -> do
      let path = tmp </> "k.json"
      BS.writeFile path "{\"version\":1,\"computed_at\":0,\"project_key\":\"k\",\"shapes\":{\"*\":{\"backends\":[\"fisher\"],\"tiers\":[],\"tactics\":[]}}}"
      r <- readPolicy path
      fmap (fmap (\p -> let g = policyFor p "*" in (gpBackends g, gpAttemptSecs g, gpEvidence g))) r
        `shouldBe` Right (Just (["fisher"], Nothing, object []))

    it "leaves no temporary file behind" $ withSystemTempDirectory "policy" $ \tmp -> do
      let path = policyPath tmp "k"
      writePolicy path (computePolicy 0 "k" defaults evidence ["S"])
      writePolicy path (computePolicy 1 "k" defaults evidence ["S"])
      listDirectory (takeDirectory path) `shouldReturn` ["k.json"]

    it "removes its temporary file when the rename fails" $ withSystemTempDirectory "policy" $ \tmp -> do
      let path = policyPath tmp "k"
      createDirectoryIfMissing True (path </> "in-the-way")
      r <- try @IOException (writePolicy path (computePolicy 0 "k" defaults evidence ["S"]))
      r `shouldSatisfy` isLeft
      listDirectory (takeDirectory path) `shouldReturn` ["k.json"]

  describe "policyDiff" $ do
    it "lists every knob of every shape against no policy" $ do
      let p = computePolicy 0 "k" defaults evidence ["S"]
          d = policyDiff Nothing p
      [(sh, k) | (sh, k, _, _) <- d]
        `shouldMatchList` [(sh, k) | sh <- ["*", "S"], k <- ["backends", "tiers", "tactics", "attempt_secs"]]

    it "lists nothing against itself and the changed knob otherwise" $ do
      let p = computePolicy 0 "k" defaults evidence ["S"]
          q = computePolicy 0 "k" defaults (uniform [] []) ["S"]
      policyDiff (Just p) p `shouldBe` []
      [(sh, k) | (sh, k, _, _) <- policyDiff (Just q) p] `shouldBe` [("*", "backends"), ("S", "backends")]
