-- | Flat search with retrieval and the library: the facts and library
-- sections in each attempt's brief, the cut-off, the harvest at the end,
-- the report's fields, and today's search when both are off.
module Fisher.Search.SchedulerRetrievalSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, asyncThreadId, waitCatch)
import Control.Exception (AsyncException (UserInterrupt), SomeException, fromException, throwTo, toException)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Role (partText, static)
import Fisher.Search.Auto
import Fisher.Search.Check (AxiomPolicy (..), LeanRun, standardAxioms)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.FakeLsp
import Fisher.Search.Index
import Fisher.Search.Lean
import Fisher.Search.Library
import Fisher.Search.Report
import Fisher.Search.RetrievalFixture
import Fisher.Search.Retrieval
import Fisher.Search.Orchestrate (realOrchestrator)
import Fisher.Search.Scheduler

goals :: Text
goals = T.unlines
  [ "import Mathlib", "import P.Defs", ""
  , "theorem a : P.bump 0 = 1 := by", "  sorry", ""
  , "theorem b : P.bump 1 = 2 := by", "  sorry" ]

one :: Text
one = T.unlines ["import Mathlib", "import P.Defs", "", "theorem t : P.bump 2 = 3 := by", "  sorry"]

type Script = Map.Map (Text, Text) [(Text, Int)]

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  , rRoot :: FilePath
  }

-- | A search in a fresh project; @check@ runs on the result inside the
-- project's directory. A proof starting with @HELPER@ also adds a helper
-- lemma above the goal.
search :: SearchConfig -> Maybe AutoConfig -> Fake -> LeanRun -> Text -> Script -> (FilePath -> IO (Maybe RetrievalEnv)) -> (Run -> IO a) -> IO a
search cfg ac f lean file script0 mkRet check = withSystemTempDirectory "search" $ \root -> do
  project root file
  ret <- mkRet root
  events <- newIORef []
  specs <- newIORef []
  calls <- newIORef []
  script <- newIORef script0
  let pop g b = atomicModifyIORef' script $ \m ->
        case [(k, x, xs) | k <- [(g, b), (g, "*")], x : xs <- [Map.findWithDefault [] k m]] of
          ((k, x, xs) : _) -> (Map.insert k xs m, x)
          [] -> (m, ("sorry", 0))
      run sp = do
        atomicModifyIORef' specs (\ss -> (ss <> [sp], ()))
        (proof0, delay) <- pop (asGoal sp) (asBackend sp)
        threadDelay (delay * 1000)
        let ls = T.splitOn "\n" (asStart sp)
            d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
            (helper, proof) = case T.stripPrefix "HELPER " proof0 of
              Just p -> (["theorem P.bump_succ (n : Nat) : P.bump (n + 1) = n + 2 := by", "  rfl", ""], p)
              Nothing -> ([], proof0)
            out = T.intercalate "\n" (take (dStart d) ls <> helper <> take (dStmtEnd d + 1 - dStart d) (drop (dStart d) ls) <> ["  " <> proof] <> drop (dEnd d) ls)
        pure (plainResult (Just out) Nothing "")
  orch <- realOrchestrator run (realToFrac <$> getPOSIXTime) (\e -> modifyIORef events (e :)) (\_ -> pure Nothing)
  let env = SearchEnv orch lean (AxiomPolicy standardAxioms False) root "F.lean"
              (\_ -> pure ())
              (fmap (\c -> AutoEnv c (fakeLsp calls f) Nothing) ac) ret
  r <- runSearch cfg env file
  es <- reverse <$> readIORef events
  ss <- readIORef specs
  rep <- either (fail . T.unpack) pure r
  check (Run rep es ss root)

-- | A search interrupted (Ctrl-C's exception thrown to its thread) once an
-- event satisfies the predicate: what it ended with, its events, the root.
interrupted :: (Event -> Bool) -> Text -> Script -> (FilePath -> IO (Maybe RetrievalEnv)) -> ((Either SomeException (Either Text SearchReport), [Event], FilePath) -> IO a) -> IO a
interrupted stopAt = interruptedBy (toException UserInterrupt) Nothing (Right stopAt)

-- | 'interrupted' with the exception, the automation, and when: after so
-- many ms, or once an event satisfies the predicate.
interruptedBy :: SomeException -> Maybe AutoEnv -> Either Int (Event -> Bool) -> Text -> Script -> (FilePath -> IO (Maybe RetrievalEnv)) -> ((Either SomeException (Either Text SearchReport), [Event], FilePath) -> IO a) -> IO a
interruptedBy ex ae when_ file script0 mkRet check = withSystemTempDirectory "search" $ \root -> do
  project root file
  ret <- mkRet root
  events <- newIORef []
  script <- newIORef script0
  let pop g = atomicModifyIORef' script $ \m -> case Map.findWithDefault [] (g, "*") m of
        x : xs -> (Map.insert (g, "*") xs m, x)
        [] -> (m, ("sorry", 0))
      run sp = do
        (proof0, delay) <- pop (asGoal sp)
        threadDelay (delay * 1000)
        let ls = T.splitOn "\n" (asStart sp)
            d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
            (helper, proof) = case T.stripPrefix "HELPER " proof0 of
              Just p -> (["theorem P.bump_succ (n : Nat) : P.bump (n + 1) = n + 2 := by", "  rfl", ""], p)
              Nothing -> ([], proof0)
            out = T.intercalate "\n" (take (dStart d) ls <> helper <> take (dStmtEnd d + 1 - dStart d) (drop (dStart d) ls) <> ["  " <> proof] <> drop (dEnd d) ls)
        pure (plainResult (Just out) Nothing "")
  orch <- realOrchestrator run (realToFrac <$> getPOSIXTime) (\e -> atomicModifyIORef' events (\es -> (e : es, ()))) (\_ -> pure Nothing)
  let env = SearchEnv orch fakeLean (AxiomPolicy standardAxioms False) root "F.lean"
              (\_ -> pure ()) ae ret
  a <- async (runSearch cfg0 env file)
  let waitFor n = do
        es <- readIORef events
        if any stopAt es || n > (2000 :: Int) then pure () else threadDelay 10000 >> waitFor (n + 1)
      stopAt = either (const (const False)) id when_
  either (\ms -> threadDelay (ms * 1000)) (const (waitFor 0)) when_
  throwTo (asyncThreadId a) ex
  r <- waitCatch a
  es <- reverse <$> readIORef events
  check (r, es, root)

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = False}

auto :: AutoConfig
auto = defaultAutoConfig {acTactics = ["rfl", "simp_all"]}

specsFor :: Text -> Run -> [AttemptSpec]
specsFor g r = [s | s <- rSpecs r, asGoal s == g]

field :: Text -> Value -> Value
field k = \case
  Object o -> fromMaybe Null (KM.lookup (K.fromText k) o)
  _ -> Null

keysOf :: Value -> [Text]
keysOf = \case
  Object o -> sort (map K.toText (KM.keys o))
  _ -> []

goalsJson :: Value -> [Value]
goalsJson v = case field "goals" v of
  Array xs -> foldr (:) [] xs
  _ -> []

isRetrievalEvent :: Event -> Bool
isRetrievalEvent = \case
  IndexBuilt {} -> True
  IndexSkipped {} -> True
  Facts {} -> True
  LibraryOffered {} -> True
  LibrarySaved {} -> True
  LibraryProblem {} -> True
  _ -> False

-- | The position of a text in another (its length when absent).
at :: Text -> Text -> Int
at needle hay = T.length (fst (T.breakOn needle hay))

spec :: Spec
spec = do
  describe "retrieval in flat search" $ do
    it "puts the facts, then the library, in an attempt's brief" $
      search cfg0 Nothing fake0 fakeLean goals (Map.fromList [(("a", "*"), [("ok", 0)]), (("b", "*"), [("ok", 0)])])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \r -> do
          case specsFor "a" r of
            [sp] -> do
              let br = briefText sp
              br `shouldSatisfy` T.isPrefixOf "Relevant project facts:\n- P.bump : def bump (n : Nat) : Nat (P/Defs.lean:3)\n"
              br `shouldSatisfy` T.isInfixOf "Lemmas proved in earlier runs"
              br `shouldSatisfy` T.isInfixOf "-- P.bump_eq (Other.lean)\ntheorem P.bump_eq (n : Nat) : P.bump n = n + 1 :=\n by rfl\n"
              at "Relevant project facts:" br `shouldSatisfy` (< at "Lemmas proved in earlier runs" br)
            ss -> expectationFailure ("attempts on a: " <> show (length ss))
          -- b also sees a, above it in the same file
          [(g, n, c) | Facts _ g n _ c <- rEvents r] `shouldBe` [("a", 2, False), ("b", 3, False)]
          [(g, ls, n, c) | LibraryOffered _ g ls n c <- rEvents r] `shouldBe` [("a", ["P.bump_eq"], 0, False), ("b", ["P.bump_eq"], 0, False)]
          -- one retrieval per goal: a retry reuses it
          length [() | Facts {} <- rEvents r] `shouldBe` 2

    it "puts the facts and the library before the suspect text" $
      search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 4, scBudget = 30, scParallelPerGoal = False}) (Just auto) fake0 {fSuspects = ["t"]} noCertify one
        (Map.fromList [(("t", "*"), [("BAD", 5), ("BAD", 5), ("ok", 5)])])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \r ->
          case specsFor "t" r of
            [_, _, third] -> do
              let br = briefText third
              br `shouldSatisfy` T.isPrefixOf "Relevant project facts:"
              br `shouldSatisfy` T.isInfixOf "x := 0"
              at "Lemmas proved in earlier runs" br `shouldSatisfy` (< at "A counterexample search" br)
            ss -> expectationFailure ("attempts on t: " <> show (length ss))

    it "puts the facts and the library before the repaired proof's holes" $
      search cfg0 {scMaxAttempts = 2} (Just auto) fake0 fakeLean one
        (Map.fromList [(("t", "*"), [("intro x\n  exact BAD", 10), ("ok", 5)])])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \r ->
          case specsFor "t" r of
            [_, second] -> do
              let br = briefText second
              br `shouldSatisfy` T.isPrefixOf "Relevant project facts:"
              at "Lemmas proved in earlier runs" br `shouldSatisfy` (< at "Start from this partial proof" br)
              br `shouldSatisfy` T.isInfixOf "Start from this partial proof"
            ss -> expectationFailure ("attempts on t: " <> show (length ss))

    it "cuts a goal's retrieval off and still launches the attempt" $ do
      retrievalCutoff `shouldBe` 10
      let slow root = do
            env <- retrievalIn (Just ([bumpLemma], [])) root
            -- an index that never finishes building its tables
            let e = head (ixEntries (reIndex env))
            pure (Just env {reIndex = (reIndex env) {ixEntries = cycle [e]}, reCutoff = 0.3})
      t0 <- getPOSIXTime
      search cfg0 Nothing fake0 fakeLean one (Map.fromList [(("t", "*"), [("ok", 0)])]) slow $ \r -> do
        map briefText (specsFor "t" r) `shouldBe` [""]
        gsStatus (fromMaybe (error "t") (lookup "t" (srGoals (rReport r)))) `shouldBe` Closed
        [(g, n, c) | Facts _ g n _ c <- rEvents r] `shouldBe` [("t", 0, True)]
        [c | LibraryOffered _ _ _ _ c <- rEvents r] `shouldBe` [True]
        [field "cut" (eventJson e) | e@Facts {} <- rEvents r] `shouldBe` [Bool True]
        [T.isSuffixOf "(cut off)" (eventLine e) | e@LibraryOffered {} <- rEvents r] `shouldBe` [True]
      t1 <- getPOSIXTime
      realToFrac (t1 - t0) `shouldSatisfy` (< (5 :: Double))

    it "retries a cut-off retrieval once, at the goal's next launch" $ do
      let slow root = do
            env <- retrievalIn (Just ([bumpLemma], [])) root
            let e = head (ixEntries (reIndex env))
            pure (Just env {reIndex = (reIndex env) {ixEntries = cycle [e]}, reCutoff = 0.2})
      search cfg0 {scMaxAttempts = 3} Nothing fake0 fakeLean one (Map.fromList [(("t", "*"), [("BAD", 0), ("BAD", 0), ("ok", 0)])]) slow $ \r -> do
        length (specsFor "t" r) `shouldBe` 3
        [(g, c) | Facts _ g _ _ c <- rEvents r] `shouldBe` [("t", True), ("t", True)]
        length [() | LibraryOffered {} <- rEvents r] `shouldBe` 2

    it "saves the merged helpers to the library at the end, logged" $
      search cfg0 Nothing fake0 fakeLean one (Map.fromList [(("t", "*"), [("HELPER exact P.bump_eq 2", 0)])])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \r -> do
          [(l, n) | LibrarySaved _ l n <- rEvents r] `shouldBe` [(1, 0)]
          (ls, _) <- loadLibrary (rRoot r </> "lib")
          map lmName ls `shouldBe` ["P.bump_succ"]
          map lmUses ls `shouldBe` [["P.bump"]]
          map lmRun ls `shouldBe` [rRoot r </> "run"]
          doesFileExist (rRoot r </> "lib" </> "root.txt") `shouldReturn` True

    it "saves nothing with the library off" $
      search cfg0 Nothing fake0 fakeLean one (Map.fromList [(("t", "*"), [("HELPER exact P.bump_eq 2", 0)])])
        (\root -> Just . (\e -> e {reLibrary = Nothing}) <$> retrievalIn Nothing root) $ \r -> do
          [() | LibrarySaved {} <- rEvents r] `shouldBe` []
          [() | LibraryOffered {} <- rEvents r] `shouldBe` []
          doesFileExist (rRoot r </> "lib" </> "lemmas.jsonl") `shouldReturn` False
          [g | Facts _ g _ _ _ <- rEvents r] `shouldBe` ["t"]

    it "gives facts, offered and used per goal in report.json, and the totals" $
      search cfg0 Nothing fake0 fakeLean goals
        (Map.fromList [(("a", "*"), [("exact P.bump_eq 0", 0)]), (("b", "*"), [("HELPER rfl", 0)])])
        (fmap Just . retrievalIn (Just ([bumpLemma, bumpLemma {lmName = "P.bump_succ", lmStatement = "theorem P.bump_succ (n : Nat) : P.bump (n + 1) = n + 2 :="}], []))) $ \r -> do
          let j = reportJson [] (rReport r)
              gs = goalsJson j
          map (field "facts") gs `shouldBe` [toJSON (2 :: Int), toJSON (3 :: Int)]
          map (field "offered") gs `shouldBe` replicate 2 (toJSON (["P.bump_eq", "P.bump_succ"] :: [Text]))
          -- a: named in the proof; b: pasted as the helper
          map (field "used") gs `shouldBe` [toJSON (["P.bump_eq"] :: [Text]), toJSON (["P.bump_succ"] :: [Text])]
          field "facts_mean" j `shouldBe` toJSON (2.5 :: Double)
          field "offered_total" j `shouldBe` toJSON (4 :: Int)
          field "used_total" j `shouldBe` toJSON (2 :: Int)
          -- the library directory starts empty: the helper is saved
          field "saved_lemmas" j `shouldBe` toJSON (1 :: Int)
          field "saved_notes" j `shouldBe` toJSON (0 :: Int)

  describe "an interrupted run" $
    it "saves what was merged before Ctrl-C to the library, logged, then ends with the interrupt" $
      interrupted (\case GoalClosed _ "a" _ -> True; _ -> False) goals
        (Map.fromList [(("a", "*"), [("HELPER exact P.bump_eq 0", 0)]), (("b", "*"), [("ok", 60000)])])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \(r, es, root) -> do
          either (\e -> fromException e == Just UserInterrupt) (const False) r `shouldBe` True
          [(l, n) | LibrarySaved _ l n <- es] `shouldBe` [(1, 0)]
          (ls, _) <- loadLibrary (root </> "lib")
          map lmName ls `shouldBe` ["P.bump_succ"]

  describe "SIGTERM's exception" $ do
    let terminated r = either (\e -> fromException e == Just Terminated) (const False) r
    it "is asynchronous: a retrieval under way rethrows it and the harvest runs" $ do
      t0 <- getPOSIXTime
      -- a library whose corpus never finishes: the retrieval runs until its cut-off
      let slowLib root = Just <$> retrievalIn (Just (cycle [bumpLemma], [])) root
      interruptedBy (toException Terminated) Nothing (Left 300) one (Map.fromList [(("t", "*"), [("ok", 0)])]) slowLib $ \(r, es, _) -> do
        terminated r `shouldBe` True
        [() | Launched {} <- es] `shouldBe` []
        [(l, n) | LibrarySaved _ l n <- es] `shouldBe` [(0, 0)]
      t1 <- getPOSIXTime
      realToFrac (t1 - t0) `shouldSatisfy` (< (5 :: Double))
    it "is asynchronous: the Auto pre-pass under way rethrows it and the harvest runs" $ do
      calls <- newIORef []
      let ae = AutoEnv auto {acSeconds = 30} (fakeLsp calls fake0 {fDelay = \m -> if m == "attempts" then 5000 else 0}) Nothing
      interruptedBy (toException Terminated) (Just ae) (Left 300) one (Map.fromList [(("t", "*"), [("ok", 0)])])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \(r, es, _) -> do
          terminated r `shouldBe` True
          [() | Launched {} <- es] `shouldBe` []
          [(l, n) | LibrarySaved _ l n <- es] `shouldBe` [(0, 0)]

  describe "helperStatements, ambiguous" $
    it "treats more than one suffix match as none" $ do
      let txt = T.unlines ["namespace A", "theorem aux : 1 = 1 := rfl", "end A", "namespace B", "theorem aux : 2 = 2 := rfl", "end B"]
      helperStatements (declarations txt) ["aux"] `shouldBe` [("aux", "")]

  describe "a read-only library" $
    it "offers from the library and saves nothing to it" $
      search cfg0 Nothing fake0 fakeLean one (Map.fromList [(("t", "*"), [("HELPER exact P.bump_eq 2", 0)])])
        (\root -> Just . (\e -> e {reHarvest = False}) <$> retrievalIn (Just ([bumpLemma], [])) root) $ \r -> do
          [ls | LibraryOffered _ _ ls _ _ <- rEvents r] `shouldBe` [["P.bump_eq"]]
          [() | LibrarySaved {} <- rEvents r] `shouldBe` []
          doesFileExist (rRoot r </> "lib" </> "lemmas.jsonl") `shouldReturn` False
          field "saved_lemmas" (reportJson [] (rReport r)) `shouldBe` toJSON (0 :: Int)

  describe "usedOf" $ do
    let eqS = "theorem P.bump_eq (n : Nat) : P.bump n = n + 1 :="
        succS = "theorem P.bump_succ (n : Nat) : P.bump (n + 1) = n + 2 :="
    it "needs the full name in the proof" $ do
      usedOf [("P.bump_eq", eqS)] "exact P.bump_eq 0" [] `shouldBe` ["P.bump_eq"]
      usedOf [("P.bump_eq", eqS)] "exact bump_eq 0" [] `shouldBe` []
      usedOf [("P.bump_eq", eqS)] "exact P.bump_eq'" [] `shouldBe` []
    it "takes a merged helper by its full name, or by its last component only with the lemma's statement" $ do
      usedOf [("P.bump_succ", succS)] "rfl" [("P.bump_succ", "theorem P.bump_succ : True := by")] `shouldBe` ["P.bump_succ"]
      usedOf [("P.bump_succ", succS)] "rfl" [("Q.bump_succ", "theorem Q.bump_succ (n : Nat) :  P.bump (n + 1) = n + 2 := by")] `shouldBe` ["P.bump_succ"]
      usedOf [("P.bump_succ", succS)] "rfl" [("Q.bump_succ", "theorem Q.bump_succ : 1 = 1 := by")] `shouldBe` []
      usedOf [("P.bump_succ", succS)] "rfl" [("bump_succ2", "theorem bump_succ2 (n : Nat) : P.bump (n + 2) = n + 3 := by")] `shouldBe` []
    it "takes a merged helper under another name with the lemma's exact statement (as the live check's model pasted it)" $ do
      usedOf [("P.bump_succ", succS)] "rfl" [("bump_succ_aux", "theorem bump_succ_aux (n : Nat) :\n    P.bump (n + 1) = n + 2 := by")] `shouldBe` ["P.bump_succ"]
      usedOf [("P.bump_succ", succS)] "rfl" [("bump_succ_aux", "theorem bump_succ_aux (n : Nat) : P.bump (n + 1) = n + 3 := by")] `shouldBe` []

  describe "helperStatements" $
    it "finds a helper the check names without its namespace" $ do
      let txt = T.unlines ["namespace Fix", "", "theorem aux' (n : Nat) : n = n := by", "  rfl", "", "theorem g : True := by", "  trivial", "", "end Fix"]
      helperStatements (declarations txt) ["aux'", "Fix.g", "missing"]
        `shouldBe` [("aux'", "theorem aux' (n : Nat) : n = n := by"), ("Fix.g", "theorem g : True := by"), ("missing", "")]

  describe "attemptBrief" $
    it "with no retrieval, today's brief byte for byte" $ do
      partText (attemptBrief mempty (static "Start from this partial proof; fill these holes:\n- line 4:\n    goal  \n") [static "A counterexample\nx := 0\n"])
        `shouldBe` "Start from this partial proof; fill these holes:\n- line 4:\n    goal  \n\n\nA counterexample\nx := 0\n"
      partText (attemptBrief mempty mempty []) `shouldBe` ""
      partText (attemptBrief mempty mempty [static "hint "]) `shouldBe` "hint "
      partText (attemptBrief (static "Relevant project facts:\n- a") (static "holes") []) `shouldBe` "Relevant project facts:\n- a\n\nholes"

  describe "with retrieval and the library off" $
    it "the events and report.json are today's" $
      search cfg0 Nothing fake0 fakeLean goals (Map.fromList [(("a", "*"), [("ok", 0)]), (("b", "*"), [("BAD", 0), ("ok", 0)])])
        (const (pure Nothing)) $ \r -> do
          filter isRetrievalEvent (rEvents r) `shouldBe` []
          map briefText (rSpecs r) `shouldBe` ["", "", ""]
          let j = reportJson [] (rReport r)
          keysOf j `shouldBe` sort ["elapsed_s", "goals_total", "goals_closed", "closed_per_hour", "budget_spent", "goals", "backends", "attempts"]
          map keysOf (goalsJson j) `shouldBe` replicate 2 (sort ["goal", "status", "attempts", "by_attempt", "by_backend", "closed_at_s"])

-- | An attempt's brief as the text the backend is handed.
briefText :: AttemptSpec -> Text
briefText = partText . asBrief
