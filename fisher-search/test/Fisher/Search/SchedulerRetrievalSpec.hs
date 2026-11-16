-- | Flat search with retrieval and the library: the facts and library
-- sections in each attempt's brief, the cut-off, the harvest at the end,
-- the report's fields, and today's search when both are off.
module Fisher.Search.SchedulerRetrievalSpec (spec) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (async, asyncThreadId, waitCatch)
import Control.Exception (AsyncException (UserInterrupt), SomeException, fromException, throwTo, toException)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Role (partText, static)
import Fisher.Search.Auto
import Fisher.Search.Check (LeanRun)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.FakeLsp
import Fisher.Search.Index
import Fisher.Search.Lean
import Fisher.Search.Library
import Fisher.Search.Orchestrate (SimTrace (..))
import Fisher.Search.Report
import Fisher.Search.RetrievalFixture
import Fisher.Search.Retrieval
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

goals :: Text
goals = T.unlines
  [ "import Mathlib", "import P.Defs", ""
  , "theorem a : P.bump 0 = 1 := by", "  sorry", ""
  , "theorem b : P.bump 1 = 2 := by", "  sorry" ]

one :: Text
one = T.unlines ["import Mathlib", "import P.Defs", "", "theorem t : P.bump 2 = 3 := by", "  sorry"]

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  , rRoot :: FilePath
  }

-- | An attempt that writes this proof; one starting with @HELPER@ also
-- adds a helper lemma above the goal.
helperAttempt :: Text -> AttemptSpec -> Either Text AttemptResult
helperAttempt proof0 sp =
  let ls = T.splitOn "\n" (asStart sp)
      d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
      (helper, proof) = case T.stripPrefix "HELPER " proof0 of
        Just p -> (["theorem P.bump_succ (n : Nat) : P.bump (n + 1) = n + 2 := by", "  rfl", ""], p)
        Nothing -> ([], proof0)
      out = T.intercalate "\n" (take (dStart d) ls <> helper <> take (dStmtEnd d + 1 - dStart d) (drop (dStart d) ls) <> ["  " <> proof] <> drop (dEnd d) ls)
   in Right (plainResult (Just out) Nothing "")

-- | A goal's attempts in launch order on the one backend @x@: the proof
-- and the virtual seconds.
proofs :: Text -> [(Text, Double)] -> [ScriptEntry]
proofs g xs = inOrder g [("x", secs, helperAttempt p) | (p, secs) <- xs]

-- | A search in a fresh project; @check@ runs on the result inside the
-- project's directory. Every attempt launched must be named ('allNamed').
search :: SearchConfig -> Maybe AutoConfig -> Fake -> LeanRun -> Text -> [ScriptEntry] -> (FilePath -> IO (Maybe RetrievalEnv)) -> (Run -> IO a) -> IO a
search cfg ac f lean file es mkRet check = do
  calls <- newIORef []
  let se = (simEnv file) {simSetup = project, simLean = lean, simAuto = fmap (\c -> AutoEnv c (fakeLsp calls f) Nothing) ac, simRetrieval = mkRet}
  simSearchIn se cfg (script es) $ \root (rep, evs, tr) -> allNamed tr >> check (Run rep evs (stSpawned tr) root)

-- | A search interrupted (Ctrl-C's exception thrown to its thread) once an
-- event is logged that satisfies the predicate: what it ended with, its
-- events, the root.
interrupted :: (Event -> Bool) -> Text -> [ScriptEntry] -> (FilePath -> IO (Maybe RetrievalEnv)) -> ((Either SomeException (Either Text SearchReport), [Event], FilePath) -> IO a) -> IO a
interrupted stopAt file es mkRet check = do
  ready <- newEmptyMVar
  interruptedBy (toException UserInterrupt) Nothing ready stopAt file es (const mkRet) check

-- | 'interrupted' with the exception and the automation: it is thrown
-- once @ready@ is filled, by the search itself when it logs an event that
-- satisfies the predicate (the search then waits there for it), or by
-- whatever else the caller hands @ready@ to (the retrieval env is built
-- with it).
interruptedBy
  :: SomeException -> Maybe AutoEnv -> MVar () -> (Event -> Bool) -> Text -> [ScriptEntry]
  -> (MVar () -> FilePath -> IO (Maybe RetrievalEnv))
  -> ((Either SomeException (Either Text SearchReport), [Event], FilePath) -> IO a) -> IO a
interruptedBy ex ae ready stopAt file es mkRet check = do
  hold <- newEmptyMVar
  let hook e = if stopAt e then putMVar ready () >> takeMVar hold else pure ()
      se = (simEnv file) {simSetup = project, simAuto = ae, simRetrieval = mkRet ready, simLogHook = hook}
  withSimEnv se (script es) $ \root env trace -> do
    a <- async (runSearch cfg0 env file)
    takeMVar ready
    throwTo (asyncThreadId a) ex
    r <- waitCatch a
    -- the search may still hold @hold@; it is released only now
    _ <- tryPutMVar hold ()
    evs <- stEvents <$> trace
    check (r, evs, root)

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 6000, scParallelPerGoal = False}

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
      search cfg0 Nothing fake0 fakeLean goals (proofs "a" [("ok", 0)] <> proofs "b" [("ok", 0)])
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
      search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 4, scBudget = 6000, scParallelPerGoal = False}) (Just auto) fake0 {fSuspects = ["t"]} noCertify one
        (proofs "t" [("BAD", 1), ("BAD", 1), ("ok", 1)])
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
        (proofs "t" [("intro x\n  exact BAD", 2), ("ok", 1)])
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
      -- the cut-off is the retrieval's own real one: well within 5 s
      done <- timeout 5000000 $ search cfg0 Nothing fake0 fakeLean one (proofs "t" [("ok", 0)]) slow $ \r -> do
        map briefText (specsFor "t" r) `shouldBe` [""]
        gsStatus (fromMaybe (error "t") (lookup "t" (srGoals (rReport r)))) `shouldBe` Closed
        [(g, n, c) | Facts _ g n _ c <- rEvents r] `shouldBe` [("t", 0, True)]
        [c | LibraryOffered _ _ _ _ c <- rEvents r] `shouldBe` [True]
        [field "cut" (eventJson e) | e@Facts {} <- rEvents r] `shouldBe` [Bool True]
        [T.isSuffixOf "(cut off)" (eventLine e) | e@LibraryOffered {} <- rEvents r] `shouldBe` [True]
      done `shouldBe` Just ()

    it "retries a cut-off retrieval once, at the goal's next launch" $ do
      let slow root = do
            env <- retrievalIn (Just ([bumpLemma], [])) root
            let e = head (ixEntries (reIndex env))
            pure (Just env {reIndex = (reIndex env) {ixEntries = cycle [e]}, reCutoff = 0.2})
      search cfg0 {scMaxAttempts = 3} Nothing fake0 fakeLean one (proofs "t" [("BAD", 0), ("BAD", 0), ("ok", 0)]) slow $ \r -> do
        length (specsFor "t" r) `shouldBe` 3
        [(g, c) | Facts _ g _ _ c <- rEvents r] `shouldBe` [("t", True), ("t", True)]
        length [() | LibraryOffered {} <- rEvents r] `shouldBe` 2

    it "saves the merged helpers to the library at the end, logged" $
      search cfg0 Nothing fake0 fakeLean one (proofs "t" [("HELPER exact P.bump_eq 2", 0)])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \r -> do
          [(l, n) | LibrarySaved _ l n <- rEvents r] `shouldBe` [(1, 0)]
          (ls, _) <- loadLibrary (rRoot r </> "lib")
          map lmName ls `shouldBe` ["P.bump_succ"]
          map lmUses ls `shouldBe` [["P.bump"]]
          map lmRun ls `shouldBe` [rRoot r </> "run"]
          doesFileExist (rRoot r </> "lib" </> "root.txt") `shouldReturn` True

    it "saves nothing with the library off" $
      search cfg0 Nothing fake0 fakeLean one (proofs "t" [("HELPER exact P.bump_eq 2", 0)])
        (\root -> Just . (\e -> e {reLibrary = Nothing}) <$> retrievalIn Nothing root) $ \r -> do
          [() | LibrarySaved {} <- rEvents r] `shouldBe` []
          [() | LibraryOffered {} <- rEvents r] `shouldBe` []
          doesFileExist (rRoot r </> "lib" </> "lemmas.jsonl") `shouldReturn` False
          [g | Facts _ g _ _ _ <- rEvents r] `shouldBe` ["t"]

    it "gives facts, offered and used per goal in report.json, and the totals" $
      search cfg0 Nothing fake0 fakeLean goals
        (proofs "a" [("exact P.bump_eq 0", 0)] <> proofs "b" [("HELPER rfl", 0)])
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
        (proofs "a" [("HELPER exact P.bump_eq 0", 0)] <> proofs "b" [("ok", 12000)])
        (fmap Just . retrievalIn (Just ([bumpLemma], []))) $ \(r, es, root) -> do
          either (\e -> fromException e == Just UserInterrupt) (const False) r `shouldBe` True
          [(l, n) | LibrarySaved _ l n <- es] `shouldBe` [(1, 0)]
          (ls, _) <- loadLibrary (root </> "lib")
          map lmName ls `shouldBe` ["P.bump_succ"]

  describe "SIGTERM's exception" $ do
    let terminated r = either (\e -> fromException e == Just Terminated) (const False) r
    it "is asynchronous: a retrieval under way rethrows it and the harvest runs" $ do
      -- a library whose corpus never finishes: the retrieval runs until its
      -- cut-off; reading past the corpus's first lemma fills @ready@, so the
      -- exception is thrown while the retrieval is under way
      let slowLib ready root = do
            rest <- unsafeInterleaveIO (tryPutMVar ready () >> pure (cycle [bumpLemma]))
            Just <$> retrievalIn (Just (bumpLemma : rest, [])) root
      ready <- newEmptyMVar
      done <- timeout 5000000 $ interruptedBy (toException Terminated) Nothing ready (const False) one (proofs "t" [("ok", 0)]) slowLib $ \(r, es, _) -> do
        terminated r `shouldBe` True
        [() | Launched {} <- es] `shouldBe` []
        [(l, n) | LibrarySaved _ l n <- es] `shouldBe` [(0, 0)]
      done `shouldBe` Just ()
    it "is asynchronous: the Auto pre-pass under way rethrows it and the harvest runs" $ do
      calls <- newIORef []
      ready <- newEmptyMVar
      stuck <- newEmptyMVar
      -- the pre-pass's first call fills @ready@ and waits there
      let lsp = fakeLsp calls fake0
          ae = AutoEnv auto {acSeconds = 30} lsp {lspAttempts = \_ _ _ -> putMVar ready () >> takeMVar stuck >> pure (Right [])} Nothing
      interruptedBy (toException Terminated) (Just ae) ready (const False) one (proofs "t" [("ok", 0)])
        (const (fmap Just . retrievalIn (Just ([bumpLemma], [])))) $ \(r, es, _) -> do
          terminated r `shouldBe` True
          [() | Launched {} <- es] `shouldBe` []
          [(l, n) | LibrarySaved _ l n <- es] `shouldBe` [(0, 0)]
      -- @stuck@ is held until here, so the waiting call is never taken for
      -- a deadlock before the exception reaches it
      _ <- tryPutMVar stuck ()
      pure ()

  describe "helperStatements, ambiguous" $
    it "treats more than one suffix match as none" $ do
      let txt = T.unlines ["namespace A", "theorem aux : 1 = 1 := rfl", "end A", "namespace B", "theorem aux : 2 = 2 := rfl", "end B"]
      helperStatements (declarations txt) ["aux"] `shouldBe` [("aux", "")]

  describe "a read-only library" $
    it "offers from the library and saves nothing to it" $
      search cfg0 Nothing fake0 fakeLean one (proofs "t" [("HELPER exact P.bump_eq 2", 0)])
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
      search cfg0 Nothing fake0 fakeLean goals (proofs "a" [("ok", 0)] <> proofs "b" [("BAD", 0), ("ok", 0)])
        (const (pure Nothing)) $ \r -> do
          filter isRetrievalEvent (rEvents r) `shouldBe` []
          map briefText (rSpecs r) `shouldBe` ["", "", ""]
          let j = reportJson [] (rReport r)
          keysOf j `shouldBe` sort ["elapsed_s", "goals_total", "goals_closed", "closed_per_hour", "budget_spent", "goals", "backends", "attempts"]
          map keysOf (goalsJson j) `shouldBe` replicate 2 (sort ["goal", "status", "attempts", "by_attempt", "by_backend", "closed_at_s"])

-- | An attempt's brief as the text the backend is handed.
briefText :: AttemptSpec -> Text
briefText = partText . asBrief
