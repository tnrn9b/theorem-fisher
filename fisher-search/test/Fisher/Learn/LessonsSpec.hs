module Fisher.Learn.LessonsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Learn.Lessons
import Fisher.Lessons.File (Template (..), globalLessonsPath, loadTemplates, projectLessonsPath, renderTemplate)
import Fisher.Role (Candidate, template)
import Fisher.Runlog

projA, projB, projC :: FilePath
projA = "/work/a"
projB = "/work/b"
projC = "/work/c"

keyA, keyB, keyC :: Text
keyA = projectKeyOf projA
keyB = projectKeyOf projB
keyC = projectKeyOf projC

withLog :: (FilePath -> Runlog -> IO a) -> IO a
withLog k = withSystemTempDirectory "lessons" $ \tmp -> withRunlog (tmp </> "runs.db") (k tmp)

-- | A search run of a project started at a time.
run :: Runlog -> FilePath -> Text -> Double -> IO ()
run rl proj rid t =
  startRun rl NewRun
    { nrId = rid, nrKind = SearchRun, nrParent = Nothing, nrProject = Just proj, nrProver = Just "lean"
    , nrBackend = Nothing, nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = t
    , nrRawDir = Nothing, nrCapture = Full }

rule :: Text -> Text -> Text -> Text -> Evidence
rule rid pk kind name =
  Evidence { evRun = rid, evAttempt = rid <> "-1", evProjectKey = pk, evKind = kind, evKey = kind <> ":" <> name, evText = cand ("lesson " <> name), evJudged = False }

-- | A text the gate lets through, as a candidate ('template'); every text
-- here passes it unchanged, and a refused one fails the test.
cand :: Text -> Candidate
cand t = maybe (error ("refused by the gate: " <> T.unpack t)) id (template t)

-- | A judged sighting from attempt 1 of a run. Its text is a candidate as
-- 'Fisher.Role.candidate' would let it through; the gate itself is tested
-- in RoleSpec and MineSpec.
judged :: Text -> Bool -> Text -> Evidence
judged rid j txt = Evidence { evRun = rid, evAttempt = rid <> "-1", evProjectKey = keyA, evKind = "judged", evKey = "", evText = cand txt, evJudged = j }

-- | 'recordEvidence'.
record :: Runlog -> Double -> Evidence -> IO LessonRow
record = recordEvidence

templates :: IO [Template]
templates = withSystemTempDirectory "home" $ \h -> either (fail . T.unpack) pure =<< loadTemplates h

promoteA :: Runlog -> IO [LessonRow]
promoteA rl = templates >>= \ts -> promote rl 50 defaultLimits ts keyA

statusOf :: Runlog -> Text -> Text -> IO (Maybe LessonStatus)
statusOf rl pk k = fmap lrStatus <$> findLesson rl pk k

-- | A lesson of project A, active, after n before-runs of (hits, attempts):
-- three evidence runs, then the before runs noted, then promoted.
activeAfter :: Runlog -> Text -> Int -> (Int, Int) -> IO LessonRow
activeAfter rl name n (h, a) = do
  forM_ [1 .. 3 :: Int] $ \i -> do
    let rid = name <> "-ev" <> tshow i
    run rl projA rid (fromIntegral i)
    _ <- record rl 1 (rule rid keyA "unknown_identifier" name)
    pure ()
  forM_ [1 .. n] $ \i -> do
    let rid = name <> "-b" <> tshow i
    run rl projA rid (10 + fromIntegral i)
    noteInForce rl defaultLimits rid [] a (Map.fromList [("unknown_identifier:" <> name, h)])
  _ <- promoteA rl
  Just r <- findLesson rl keyA ("unknown_identifier:" <> name)
  lrStatus r `shouldBe` Active
  pure r

-- | n runs of project A in force for a lesson with (hits, attempts) each.
inForce :: Runlog -> LessonRow -> Int -> (Int, Int) -> IO ()
inForce rl l = inForceFrom rl l 1

-- | The same, numbering the runs from a first.
inForceFrom :: Runlog -> LessonRow -> Int -> Int -> (Int, Int) -> IO ()
inForceFrom rl l first n (h, a) =
  forM_ [first .. first + n - 1] $ \i -> do
    let rid = lrKey l <> "-f" <> tshow i
    run rl projA rid (100 + fromIntegral i)
    noteInForce rl defaultLimits rid [lrId l] a (Map.fromList [(lrKey l, h)])

-- | A row with nothing counted yet.
blankRow :: Text -> Text -> Text -> Text -> LessonRow
blankRow pk kind key txt =
  LessonRow
    { lrId = 0, lrProjectKey = pk, lrKind = kind, lrKey = key, lrText = txt, lrStatus = Candidate
    , lrEvidenceRuns = ["e1"], lrAgreements = 0, lrFirstSeen = 1, lrActivatedAt = Nothing, lrRetiredAt = Nothing
    , lrRunsActive = [], lrHitsWhileActive = 0, lrAttemptsWhileActive = 0, lrHitsBefore = 0
    , lrAttemptsBefore = 0, lrRunsBefore = 0, lrBeforeWindow = [], lrAgreedAttempts = [] }

tshow :: Int -> Text
tshow = T.pack . show

spec :: Spec
spec = do
  it "keeps the numbers in the limits" $
    defaultLimits `shouldBe` Limits 3 2 10 0.8 5 12 3 False

  it "keeps evidence from two runs a candidate and promotes on the third" $ withLog $ \_ rl -> do
    forM_ ["r1", "r2"] $ \r -> record rl 1 (rule r keyA "unknown_identifier" "foo")
    -- the same run again counts once
    r2 <- record rl 2 (rule "r2" keyA "unknown_identifier" "foo")
    lrEvidenceRuns r2 `shouldBe` ["r1", "r2"]
    lrStatus r2 `shouldBe` Candidate
    _ <- promoteA rl
    statusOf rl keyA "unknown_identifier:foo" `shouldReturn` Just Candidate
    _ <- record rl 3 (rule "r3" keyA "unknown_identifier" "foo")
    changed <- promoteA rl
    map lrKey changed `shouldBe` ["unknown_identifier:foo"]
    map lrStatus changed `shouldBe` [Active]
    statusOf rl keyA "unknown_identifier:foo" `shouldReturn` Just Active

  it "needs two judge agreements for a judged candidate, merging near texts" $ withLog $ \_ rl -> do
    _ <- record rl 1 (judged "r1" True "Use `Nat.succ_le_of_lt` to turn a strict bound into a successor bound.")
    _ <- record rl 1 (judged "r2" False "Use `Nat.succ_le_of_lt` for strict bounds.")
    r <- record rl 1 (judged "r3" False "use Nat.succ_le_of_lt, it turns < into succ <=")
    lrEvidenceRuns r `shouldBe` ["r1", "r2", "r3"]
    lrAgreements r `shouldBe` 1
    -- a judged key has its own namespace, never a rule's
    lrKey r `shouldBe` "judged:nat.succ_le_of_lt"
    _ <- promoteA rl
    statusOf rl keyA "judged:nat.succ_le_of_lt" `shouldReturn` Just Candidate
    r' <- record rl 1 (judged "r3" True "Use Nat.succ_le_of_lt.")
    lrAgreements r' `shouldBe` 2
    -- the counts are met, but a judged lesson waits for a human
    _ <- promoteA rl
    statusOf rl keyA "judged:nat.succ_le_of_lt" `shouldReturn` Just Candidate
    awaitingApproval defaultLimits r' `shouldBe` True
    -- fisher.lessons.auto_judged: true promotes it on the counts
    ts <- templates
    _ <- promote rl 50 defaultLimits {lAutoJudged = True} ts keyA
    statusOf rl keyA "judged:nat.succ_le_of_lt" `shouldReturn` Just Active
    length <$> listLessons rl (Just keyA) [] `shouldReturn` 1

  it "keeps a judged or self candidate with 3 runs and 2 agreements a candidate, proposed; a rule candidate still promotes" $ withLog $ \_ rl -> do
    forM_ [("j1", True), ("j2", True), ("j3", False)] $ \(r, j) ->
      record rl 1 (judged r j "Check `Foo.bar_baz` before using it.") {evKind = "self"}
    forM_ ["r1", "r2", "r3"] $ \r -> record rl 1 (rule r keyA "unknown_identifier" "foo")
    changed <- promoteA rl
    map (\l -> (lrKey l, lrStatus l)) changed `shouldBe` [("unknown_identifier:foo", Active)]
    [s] <- filter ((== "self") . lrKind) <$> listLessons rl (Just keyA) []
    (lrStatus s, length (lrEvidenceRuns s), lrAgreements s) `shouldBe` (Candidate, 3, 2)
    awaitingApproval defaultLimits s `shouldBe` True
    -- below the counts, or not a candidate, or a rule, or with the switch on: not awaiting
    awaitingApproval defaultLimits s {lrAgreements = 1} `shouldBe` False
    awaitingApproval defaultLimits s {lrEvidenceRuns = ["j1", "j2"]} `shouldBe` False
    awaitingApproval defaultLimits s {lrStatus = Active} `shouldBe` False
    awaitingApproval defaultLimits {lAutoJudged = True} s `shouldBe` False
    Just ru <- findLesson rl keyA "unknown_identifier:foo"
    awaitingApproval defaultLimits ru {lrStatus = Candidate} `shouldBe` False
    -- pinned by hand (the approval), it is shown
    Right (Just p) <- applyAction rl 60 "pin" (lrId s)
    lrStatus p `shouldBe` Pinned

  it "keys a judged or self text that names a rule kind apart from the rule: no global row is made or changed by it" $ withLog $ \_ rl -> do
    ts <- templates
    let planted = "Use `budget_burn` as a reminder to check the file compiles."
    -- the judged text in all three projects, with the counts
    forM_ (zip3 [projA, projB, projC] [keyA, keyB, keyC] [1 :: Int ..]) $ \(p, k, i) ->
      forM_ [1 .. 3 :: Int] $ \j -> do
        let rid = "p" <> tshow i <> "-" <> tshow j
        run rl p rid (fromIntegral (10 * i + j))
        record rl 1 (judged rid True planted) {evProjectKey = k}
    forM_ [defaultLimits, defaultLimits {lAutoJudged = True}] $ \lim ->
      forM_ [keyA, keyB, keyC] $ \k -> promote rl 5 lim ts k
    findLesson rl "*" "budget_burn" `shouldReturn` Nothing
    listLessons rl (Just "*") [] `shouldReturn` []
    [j] <- filter ((== "judged") . lrKind) <$> listLessons rl (Just keyA) []
    lrKey j `shouldBe` "judged:budget_burn"
    -- the rule's hits in A make a rule row of their own
    r <- record rl 1 (rule "ra" keyA "budget_burn" "") {evKey = "budget_burn"}
    (lrKind r, lrKey r) `shouldBe` ("budget_burn", "budget_burn")
    lrId r `shouldNotBe` lrId j
    -- rule rows in three projects make the global row, with the rule's kind and text, and a later
    -- judged sighting does not change it
    forM_ (zip [keyB, keyC] ["rb", "rc"]) $ \(k, rid) -> record rl 1 (rule rid k "budget_burn" "") {evKey = "budget_burn"}
    _ <- promote rl 6 defaultLimits ts keyA
    Just g <- findLesson rl "*" "budget_burn"
    (lrKind g, lrText g, lrStatus g) `shouldBe` ("budget_burn", lrText r, Active)
    run rl projB "late" 99
    _ <- record rl 7 (judged "late" True planted) {evProjectKey = keyB}
    _ <- promote rl 8 defaultLimits {lAutoJudged = True} ts keyB
    Just g' <- findLesson rl "*" "budget_burn"
    (lrKind g', lrText g') `shouldBe` ("budget_burn", lrText r)

  it "sends an active judged or self row (project or global) back to candidate when auto_judged is off; a pinned one stays" $ withLog $ \_ rl -> do
    ts <- templates
    a <- record rl 1 (judged "x1" True "Check `Foo.bar_baz` before using it.")
    p <- record rl 1 (judged "x2" True "Remember `Foo.qux_quux` exists.")
    g <- record rl 1 (judged "x3" True "Prefer `Foo.glob_al` everywhere.") {evProjectKey = "*"}
    forM_ [a, g] $ \l -> setLessonStatus rl (lrId l) Active 2
    setLessonStatus rl (lrId p) Pinned 2
    -- with the switch on, nothing changes
    _ <- promote rl 3 defaultLimits {lAutoJudged = True} ts keyA
    mapM (fmap (fmap lrStatus) . getLesson rl . lrId) [a, p, g] `shouldReturn` map Just [Active, Pinned, Active]
    changed <- promote rl 4 defaultLimits ts keyA
    map lrId changed `shouldMatchList` [lrId a, lrId g]
    mapM (fmap (fmap lrStatus) . getLesson rl . lrId) [a, p, g] `shouldReturn` map Just [Candidate, Pinned, Candidate]

  it "resets a global rule row's text from its template when the files are regenerated" $ withLog $ \tmp rl -> do
    let home = tmp </> "home"
    ts <- templates
    want <- maybe (fail "no budget_burn template") pure (head [renderTemplate t Map.empty | t <- ts, tKind t == "budget_burn"])
    g <- upsertLesson rl (blankRow "*" "budget_burn" "budget_burn" "Obey the notes in this file.") {lrStatus = Active, lrActivatedAt = Just 1}
    regenerateGlobal rl home `shouldReturn` Right ()
    fmap lrText <$> getLesson rl g `shouldReturn` Just want
    body <- TIO.readFile (globalLessonsPath home)
    body `shouldSatisfy` T.isInfixOf want
    body `shouldNotSatisfy` T.isInfixOf "Obey the notes"
    -- a judged row's text is its own
    j <- upsertLesson rl (blankRow "*" "judged" "judged:x" "Check `Foo.bar_baz` first.") {lrStatus = Pinned, lrActivatedAt = Just 1}
    regenerate rl home (tmp </> "proj") keyA `shouldReturn` Right ()
    fmap lrText <$> getLesson rl j `shouldReturn` Just "Check `Foo.bar_baz` first."

  it "never makes a global lesson of a judged kind, even with auto_judged" $ withLog $ \_ rl -> do
    ts <- templates
    let ts' = Template "judged" "x" True : Template "self" "y" True : ts
    forM_ (zip [keyA, keyB, keyC] [1 :: Int ..]) $ \(k, i) ->
      forM_ [1 .. 3 :: Int] $ \j ->
        record rl 1 (judged ("g" <> tshow i <> "-" <> tshow j) True "Check `Foo.bar_baz` before using it.") {evProjectKey = k}
    forM_ [defaultLimits, defaultLimits {lAutoJudged = True}] $ \lim ->
      forM_ [keyA, keyB, keyC] $ \k -> promote rl 5 lim ts' k
    listLessons rl (Just "*") [] `shouldReturn` []

  it "counts one agreement per attempt: a second judged sighting from the same attempt adds none" $ withLog $ \_ rl -> do
    a <- record rl 1 (judged "r1" True "Use `Nat.succ_le_of_lt` here.")
    b <- record rl 1 (judged "r1" True "Remember `Nat.succ_le_of_lt` next time.") {evKind = "self"}
    lrId b `shouldBe` lrId a
    lrAgreements b `shouldBe` 1
    lrAgreedAttempts b `shouldBe` ["r1-1"]
    c <- record rl 2 (judged "r2" True "Use `Nat.succ_le_of_lt` again.")
    (lrAgreements c, lrAgreedAttempts c) `shouldBe` (2, ["r1-1", "r2-1"])

  it "merges a judged text only into a judged or self row, never into a rule row" $ withLog $ \_ rl -> do
    r <- record rl 1 (rule "r1" keyA "unknown_identifier" "Nat.foo") {evText = cand "In this project `Nat.foo` does not exist."}
    j <- record rl 1 (judged "r2" True "Use `Nat.foo` only after checking its type.")
    lrId j `shouldNotBe` lrId r
    lrKind j `shouldBe` "judged"
    Just r' <- getLesson rl (lrId r)
    lrEvidenceRuns r' `shouldBe` ["r1"]

  it "stores the candidate's text as it is (the gate is at the candidate)" $ withLog $ \_ rl -> do
    k <- record rl 1 (judged "r1" True "Check `Nat.foo_bar` before `lake build`.")
    lrText k `shouldBe` "Check `Nat.foo_bar` before `lake build`."
    length <$> listLessons rl (Just keyA) [] `shouldReturn` 1

  it "caps a file at twelve: a thirteenth promotion sends the oldest latest evidence back" $ withLog $ \_ rl -> do
    -- lesson i has its latest evidence at time 100 - i, so lesson 13 is the oldest
    forM_ [1 .. 13 :: Int] $ \i -> forM_ [1 .. 3 :: Int] $ \j -> do
      let rid = "l" <> tshow i <> "-" <> tshow j
      run rl projA rid (fromIntegral (100 - i - (3 - j)))
      record rl 1 (rule rid keyA "unknown_identifier" ("n" <> tshow i))
    _ <- promoteA rl
    act <- listLessons rl (Just keyA) [Active]
    length act `shouldBe` 12
    statusOf rl keyA "unknown_identifier:n13" `shouldReturn` Just Candidate
    -- a newer piece of evidence for 13 makes 12 the oldest
    run rl projA "late" 1000
    _ <- record rl 1 (rule "late" keyA "unknown_identifier" "n13")
    changed <- promoteA rl
    map (\l -> (lrKey l, lrStatus l)) changed
      `shouldMatchList` [("unknown_identifier:n13", Active), ("unknown_identifier:n12", Candidate)]
    length <$> listLessons rl (Just keyA) [Active] `shouldReturn` 12

  it "counts a pinned line toward the cap and never demotes it" $ withLog $ \_ rl -> do
    forM_ [1 .. 13 :: Int] $ \i -> forM_ [1 .. 3 :: Int] $ \j -> do
      let rid = "p" <> tshow i <> "-" <> tshow j
      run rl projA rid (fromIntegral (100 - i - (3 - j)))
      record rl 1 (rule rid keyA "unknown_identifier" ("n" <> tshow i))
    -- n13 has the oldest evidence and is pinned
    Just p <- findLesson rl keyA "unknown_identifier:n13"
    setLessonStatus rl (lrId p) Pinned 10
    _ <- promoteA rl
    length <$> listLessons rl (Just keyA) [Active] `shouldReturn` 11
    statusOf rl keyA "unknown_identifier:n13" `shouldReturn` Just Pinned
    statusOf rl keyA "unknown_identifier:n12" `shouldReturn` Just Candidate

  it "clears the runs in force of a line the cap demotes" $ withLog $ \_ rl -> do
    l <- activeAfter rl "old" 0 (0, 0)
    inForce rl l 2 (1, 10)
    forM_ [1 .. 12 :: Int] $ \i -> forM_ [1 .. 3 :: Int] $ \j -> do
      let rid = "q" <> tshow i <> "-" <> tshow j
      run rl projA rid (500 + fromIntegral (i * 3 + j))
      record rl 1 (rule rid keyA "unknown_identifier" ("m" <> tshow i))
    _ <- promoteA rl
    Just d <- getLesson rl (lrId l)
    (lrStatus d, lrRunsActive d, lrHitsWhileActive d, lrAttemptsWhileActive d) `shouldBe` (Candidate, [], 0, 0)

  it "keeps the before window at the last ten runs" $ withLog $ \_ rl -> do
    run rl projA "e1" 1
    _ <- record rl 1 (rule "e1" keyA "unknown_identifier" "w")
    forM_ [1 .. 14 :: Int] $ \i -> do
      run rl projA ("w" <> tshow i) (fromIntegral i + 1)
      noteInForce rl defaultLimits ("w" <> tshow i) [] 10 (Map.fromList [("unknown_identifier:w", 5)])
    Just r <- findLesson rl keyA "unknown_identifier:w"
    (lrRunsBefore r, lrHitsBefore r, lrAttemptsBefore r) `shouldBe` (10, 50, 100)
    -- a run of another project does not count
    run rl projB "other" 50
    noteInForce rl defaultLimits "other" [] 10 (Map.fromList [("unknown_identifier:w", 5)])
    Just r' <- findLesson rl keyA "unknown_identifier:w"
    lrRunsBefore r' `shouldBe` 10
    -- noting a run again changes nothing
    noteInForce rl defaultLimits "w14" [] 10 (Map.fromList [("unknown_identifier:w", 5)])
    findLesson rl keyA "unknown_identifier:w" `shouldReturn` Just r'

  it "lets early hits leave the before window: 4 early hits, then 50 clean runs" $ withLog $ \_ rl -> do
    run rl projA "e1" 1
    _ <- record rl 1 (rule "e1" keyA "unknown_identifier" "early")
    forM_ [1 .. 54 :: Int] $ \i -> do
      run rl projA ("x" <> tshow i) (fromIntegral i + 1)
      noteInForce rl defaultLimits ("x" <> tshow i) [] 10 (Map.fromList [("unknown_identifier:early", if i <= 4 then 1 else 0)])
    Just r <- findLesson rl keyA "unknown_identifier:early"
    (lrRunsBefore r, lrHitsBefore r, lrAttemptsBefore r) `shouldBe` (10, 0, 100)
    map (\(x, _, _) -> x) (lrBeforeWindow r) `shouldBe` ["x" <> tshow i | i <- [45 .. 54]]

  it "retires after ten runs in force at the same rate" $ withLog $ \_ rl -> do
    l <- activeAfter rl "same" 10 (5, 10)
    inForce rl l 9 (5, 10)
    retire rl 60 defaultLimits keyA `shouldReturn` []
    inForceFrom rl l 10 1 (5, 10)
    Just l' <- getLesson rl (lrId l)
    length (lrRunsActive l') `shouldBe` 10
    (lrHitsWhileActive l', lrAttemptsWhileActive l') `shouldBe` (50, 100)
    -- a run noted twice counts once
    noteInForce rl defaultLimits (lrKey l <> "-f1") [lrId l] 10 (Map.fromList [(lrKey l, 5)])
    fmap lrHitsWhileActive <$> getLesson rl (lrId l) `shouldReturn` Just 50
    retired <- retire rl 60 defaultLimits keyA
    map lrId retired `shouldBe` [lrId l]
    statusOf rl keyA (lrKey l) `shouldReturn` Just Retired

  it "keeps a lesson whose rate halved" $ withLog $ \_ rl -> do
    l <- activeAfter rl "half" 10 (4, 10)
    inForce rl l 10 (2, 10)
    retire rl 60 defaultLimits keyA `shouldReturn` []
    statusOf rl keyA (lrKey l) `shouldReturn` Just Active

  it "never retires with four before runs" $ withLog $ \_ rl -> do
    l <- activeAfter rl "few" 4 (5, 10)
    inForce rl l 12 (5, 10)
    retire rl 60 defaultLimits keyA `shouldReturn` []
    statusOf rl keyA (lrKey l) `shouldReturn` Just Active

  it "never retires a pinned lesson" $ withLog $ \_ rl -> do
    l <- activeAfter rl "pin" 10 (5, 10)
    setLessonStatus rl (lrId l) Pinned 55
    inForce rl l 10 (5, 10)
    retire rl 60 defaultLimits keyA `shouldReturn` []
    statusOf rl keyA (lrKey l) `shouldReturn` Just Pinned

  it "gives a retired or dropped key no new evidence" $ withLog $ \_ rl -> do
    l <- activeAfter rl "gone" 10 (5, 10)
    inForce rl l 10 (5, 10)
    _ <- retire rl 60 defaultLimits keyA
    Just old <- getLesson rl (lrId l)
    recordEvidence rl 70 (rule "new-run" keyA "unknown_identifier" "gone") `shouldReturn` old
    _ <- promoteA rl
    statusOf rl keyA (lrKey l) `shouldReturn` Just Retired
    -- a dropped judged lesson swallows a near text without changing
    d <- record rl 1 (judged "j1" True "Never call `Foo.bar_baz` on a list.")
    setLessonStatus rl (lrId d) Dropped 2
    Just d0 <- getLesson rl (lrId d)
    recordEvidence rl 3 (judged "j2" True "Do not call `Foo.bar_baz` on lists.") `shouldReturn` d0

  it "makes a global lesson from three projects and hides the project rows" $ withLog $ \_ rl -> do
    ts <- templates
    forM_ (zip3 [projA, projB, projC] [keyA, keyB, keyC] [1 :: Int ..]) $ \(p, k, i) -> do
      let rid = "g" <> tshow i
      run rl p rid (fromIntegral i)
      record rl 1 (rule rid k "sorry_left" "")
    changed <- promote rl 5 defaultLimits ts keyC
    map (\l -> (lrProjectKey l, lrStatus l)) changed `shouldBe` [("*", Active)]
    Just g <- findLesson rl "*" "sorry_left:"
    lrEvidenceRuns g `shouldMatchList` ["g1", "g2", "g3"]
    statusOf rl keyA "sorry_left:" `shouldReturn` Just Candidate
    -- a kind that is not global stays per project
    forM_ (zip3 [projA, projB, projC] [keyA, keyB, keyC] [1 :: Int ..]) $ \(p, k, i) -> do
      let rid = "u" <> tshow i
      run rl p rid (fromIntegral i)
      record rl 1 (rule rid k "unknown_identifier" "x")
    _ <- promote rl 5 defaultLimits ts keyA
    findLesson rl "*" "unknown_identifier:x" `shouldReturn` Nothing
    -- project A's own evidence for sorry_left reaches three runs: still a
    -- candidate, since the global row carries it
    forM_ ["a2", "a3"] $ \rid -> run rl projA rid 9 >> record rl 1 (rule rid keyA "sorry_left" "")
    _ <- promote rl 5 defaultLimits ts keyA
    statusOf rl keyA "sorry_left:" `shouldReturn` Just Candidate

  it "does not propose per project a key whose global row is retired" $ withLog $ \tmp rl -> do
    ts <- templates
    forM_ (zip3 [projA, projB, projC] [keyA, keyB, keyC] [1 :: Int ..]) $ \(p, k, i) -> do
      let rid = "g" <> tshow i
      run rl p rid (fromIntegral i)
      record rl 1 (rule rid k "sorry_left" "")
    _ <- promote rl 5 defaultLimits ts keyA
    Just g <- findLesson rl "*" "sorry_left:"
    setLessonStatus rl (lrId g) Retired 6
    forM_ ["a2", "a3"] $ \rid -> run rl projA rid 9 >> record rl 1 (rule rid keyA "sorry_left" "")
    _ <- promote rl 7 defaultLimits ts keyA
    statusOf rl keyA "sorry_left:" `shouldReturn` Just Candidate
    statusOf rl "*" "sorry_left:" `shouldReturn` Just Retired
    -- an active project row of the key stays out of the project file
    Just pa <- findLesson rl keyA "sorry_left:"
    setLessonStatus rl (lrId pa) Active 8
    let root = tmp </> "proj"
    regenerate rl (tmp </> "home") root keyA `shouldReturn` Right ()
    doesFileExist (projectLessonsPath root) `shouldReturn` False

  it "counts a pinned project row toward the projects of a global lesson" $ withLog $ \_ rl -> do
    ts <- templates
    forM_ (zip3 [projA, projB, projC] [keyA, keyB, keyC] [1 :: Int ..]) $ \(p, k, i) -> do
      let rid = "g" <> tshow i
      run rl p rid (fromIntegral i)
      record rl 1 (rule rid k "repeated_call" "")
    Just pb <- findLesson rl keyB "repeated_call:"
    setLessonStatus rl (lrId pb) Pinned 3
    _ <- promote rl 5 defaultLimits ts keyA
    statusOf rl "*" "repeated_call:" `shouldReturn` Just Active

  it "takes the newest text for a global row and notes any run in a global candidate's window" $ withLog $ \_ rl -> do
    ts <- templates
    forM_ (zip3 [projA, projB, projC] [keyA, keyB, keyC] [1 :: Int ..]) $ \(p, k, i) -> do
      let rid = "g" <> tshow i
      run rl p rid (fromIntegral i)
      _ <- record rl (fromIntegral i) (rule rid k "timeout_loop" "") {evText = cand ("text " <> tshow i)}
      pure ()
    _ <- promote rl 5 defaultLimits ts keyA
    Just g <- findLesson rl "*" "timeout_loop:"
    lrText g `shouldBe` "text 3"
    -- demoted (as the cap would), the global candidate takes runs of any project
    setLessonStatus rl (lrId g) Candidate 6
    run rl projB "b9" 20
    noteInForce rl defaultLimits "b9" [] 4 (Map.fromList [("timeout_loop:", 2)])
    fmap lrBeforeWindow <$> getLesson rl (lrId g) `shouldReturn` Just [("b9", 2, 4)]

  it "regenerates both files, keeping the lines of yours" $ withLog $ \tmp rl -> do
    let home = tmp </> "home"
        root = tmp </> "proj"
    createDirectoryIfMissing True (root </> ".fisher")
    TIO.writeFile (projectLessonsPath root) "<!-- fisher lessons: generated, edit only under \"## Yours\" -->\n\n1. old\n\n## Yours\n\nmy own note\n"
    l <- activeAfter rl "foo" 0 (0, 0)
    -- a candidate and a global lesson
    run rl projA "c1" 1
    _ <- record rl 1 (rule "c1" keyA "unknown_identifier" "cand")
    gid <- upsertLesson rl (LessonRow 0 "*" "sorry_left" "sorry_left:" "Remove every sorry." Candidate [] 0 1 Nothing Nothing [] 0 0 0 0 0 [] [])
    setLessonStatus rl gid Active 2
    regenerate rl home root keyA `shouldReturn` Right ()
    proj <- TIO.readFile (projectLessonsPath root)
    T.lines proj `shouldContain` ["1. " <> lrText l]
    T.lines proj `shouldContain` ["my own note"]
    proj `shouldNotSatisfy` T.isInfixOf "lesson cand"
    proj `shouldNotSatisfy` T.isInfixOf "old"
    glob <- TIO.readFile (globalLessonsPath home)
    -- a global rule row's text is its template's (globalRuleTexts)
    T.lines glob `shouldContain` ["1. Remove every `sorry` and confirm the file has none before you finish."]
    glob `shouldNotSatisfy` T.isInfixOf "lesson foo"

  it "leaves a foreign file alone and says so" $ withLog $ \tmp rl -> do
    let home = tmp </> "home"
        root = tmp </> "proj"
    createDirectoryIfMissing True (root </> ".fisher")
    TIO.writeFile (projectLessonsPath root) "my file\n"
    _ <- activeAfter rl "foo" 0 (0, 0)
    r <- regenerate rl home root keyA
    r `shouldSatisfy` either (T.isInfixOf "foreign") (const False)
    TIO.readFile (projectLessonsPath root) `shouldReturn` "my file\n"

  it "creates no file when there is nothing to write" $ withLog $ \tmp rl -> do
    let home = tmp </> "home"
        root = tmp </> "proj"
    regenerate rl home root keyA `shouldReturn` Right ()
    doesFileExist (projectLessonsPath root) `shouldReturn` False
    doesFileExist (globalLessonsPath home) `shouldReturn` False

  describe "applyTo and applyAction" $ do
    let base st =
          LessonRow
            { lrId = 0, lrProjectKey = keyA, lrKind = "unknown_identifier", lrKey = "k:x", lrText = "t", lrStatus = st
            , lrEvidenceRuns = ["r1"], lrAgreements = 0, lrFirstSeen = 10, lrActivatedAt = Just 20, lrRetiredAt = Nothing
            , lrRunsActive = ["r2"], lrHitsWhileActive = 3, lrAttemptsWhileActive = 4, lrHitsBefore = 0
            , lrAttemptsBefore = 0, lrRunsBefore = 0, lrBeforeWindow = [], lrAgreedAttempts = [] }
    it "follows the rules: pin, unpin, drop, and the refusals" $ do
      fmap lrStatus (applyTo "pin" 99 (base Candidate)) `shouldBe` Right Pinned
      fmap lrActivatedAt (applyTo "pin" 99 (base Candidate) {lrActivatedAt = Nothing}) `shouldBe` Right (Just 99)
      fmap lrActivatedAt (applyTo "pin" 99 (base Active)) `shouldBe` Right (Just 20)
      fmap (\l -> (lrActivatedAt l, lrRetiredAt l, lrRunsActive l, lrHitsWhileActive l, lrAttemptsWhileActive l))
        (applyTo "pin" 99 (base Retired) {lrRetiredAt = Just 50})
        `shouldBe` Right (Just 99, Nothing, [], 0, 0)
      applyTo "pin" 99 (base Pinned) `shouldBe` Left "already pinned"
      applyTo "pin" 99 (base Dropped) `shouldBe` Left "dropped"
      fmap lrStatus (applyTo "unpin" 99 (base Pinned)) `shouldBe` Right Active
      fmap lrActivatedAt (applyTo "unpin" 99 (base Pinned)) `shouldBe` Right (Just 99)
      applyTo "unpin" 99 (base Active) `shouldBe` Left "not pinned"
      -- a judged or self lesson unpinned is a candidate again (proposed), its in-force counters cleared
      forM_ ["judged", "self"] $ \k ->
        fmap (\l -> (lrStatus l, lrActivatedAt l, lrRunsActive l, lrHitsWhileActive l, lrAttemptsWhileActive l))
          (applyTo "unpin" 99 (base Pinned) {lrKind = k})
          `shouldBe` Right (Candidate, Nothing, [], 0, 0)
      fmap (\l -> (lrStatus l, lrRetiredAt l)) (applyTo "drop" 99 (base Active)) `shouldBe` Right (Dropped, Just 99)
      applyTo "drop" 99 (base Dropped) `shouldBe` Left "already dropped"
      applyTo "promote" 99 (base Active) `shouldSatisfy` either (T.isInfixOf "unknown action") (const False)

    it "decides on the row as stored: a row retired meanwhile is pinned afresh" $ withLog $ \_ rl -> do
      i <- upsertLesson rl (base Active)
      -- a mining pass retires it after the caller last looked
      setLessonStatus rl i Retired 60
      Right (Just l) <- applyAction rl 99 "pin" i
      (lrStatus l, lrActivatedAt l, lrRunsActive l, lrHitsWhileActive l) `shouldBe` (Pinned, Just 99, [], 0)
      -- a row dropped meanwhile is refused, nothing written
      setLessonStatus rl i Dropped 70
      applyAction rl 99 "unpin" i `shouldReturn` Left "not pinned"
      (fmap lrStatus <$> getLesson rl i) `shouldReturn` Just Dropped
      applyAction rl 99 "pin" (i + 100) `shouldReturn` Right Nothing
