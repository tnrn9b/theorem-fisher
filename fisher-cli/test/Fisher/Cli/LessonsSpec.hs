-- | @fisher lessons list|show|pin|unpin|drop|mine|reset@ over a temporary
-- run log, Fisher home and project, with a fake judge.
module Fisher.Cli.LessonsSpec (spec) where

import Control.Monad (forM_, void)
import Data.Aeson (Value, object, (.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, helper, info, renderFailure, (<**>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.Lessons
import Fisher.Lessons.File (LessonsFile (..), lessonsHeader, projectLessonsPath, readLessonsFile)
import Fisher.Runlog
import Fisher.Search.GraphJobs (Ask)

-- | The command over the Fisher home and project in a temporary
-- directory, with a judge or none: its output.
lessonsWith :: Maybe Ask -> FilePath -> FilePath -> [String] -> IO Text
lessonsWith = lessonsWithJ (object [])

-- | The same with a configuration and no judge.
lessonsWithCfg :: Value -> FilePath -> FilePath -> [String] -> IO Text
lessonsWithCfg cfg = lessonsWithJ cfg Nothing

lessonsWithJ :: Value -> Maybe Ask -> FilePath -> FilePath -> [String] -> IO Text
lessonsWithJ cfg judge home cwd argv = do
  cmd <- case execParserPure defaultPrefs (info lessonsParser mempty) argv of
    Success c -> pure c
    _ -> fail ("argv did not parse: " <> show argv)
  out <- newIORef ([] :: [Text])
  runLessons
    LessonsEnv
      { lsRunlogPath = runlogPath home, lsFisherHome = home, lsCwd = cwd, lsConfig = cfg, lsNow = pure 1000
      , lsPut = \t -> modifyIORef out (t :), lsJudge = pure judge }
    cmd
  T.unlines . reverse <$> readIORef out

lessonsIn :: FilePath -> FilePath -> [String] -> IO Text
lessonsIn = lessonsWith Nothing

-- | A temporary Fisher home and project root.
withDirs :: (FilePath -> FilePath -> IO a) -> IO a
withDirs k = withSystemTempDirectory "lessons" $ \tmp -> do
  let home = tmp </> "home"
      root = tmp </> "proj"
  createDirectoryIfMissing True home
  createDirectoryIfMissing True root
  k home root

newRun :: Text -> RunKind -> Maybe Text -> FilePath -> Double -> NewRun
newRun rid kind parent root t =
  NewRun
    { nrId = rid, nrKind = kind, nrParent = parent, nrProject = Just root, nrProver = Just "lean"
    , nrBackend = Nothing, nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = t
    , nrRawDir = Nothing, nrCapture = Full }

lesson :: Text -> Text -> Text -> Text -> LessonStatus -> [Text] -> Int -> LessonRow
lesson pk kind key txt st runs agree =
  LessonRow
    { lrId = 0, lrProjectKey = pk, lrKind = kind, lrKey = key, lrText = txt, lrStatus = st
    , lrEvidenceRuns = runs, lrAgreements = agree, lrFirstSeen = 50, lrActivatedAt = Nothing, lrRetiredAt = Nothing
    , lrRunsActive = [], lrHitsWhileActive = 0, lrAttemptsWhileActive = 0, lrHitsBefore = 0
    , lrAttemptsBefore = 0, lrRunsBefore = 0, lrBeforeWindow = [], lrAgreedAttempts = [] }

activeText, pinnedText, candText, droppedText, globalText :: Text
activeText = "In this project `Nat.foo` does not exist. Search before naming a lemma."
pinnedText = "Check the type of the lemma before using it."
candText = "Run `lake build` before finishing."
droppedText = "An old lesson nobody wants."
globalText = "Always check the file compiles before you finish."

-- | One run of the project and five lessons: active (3 runs), pinned (a
-- judged one with 2 agreements), candidate, dropped, and a global active
-- one. Answers their ids in that order.
seeded :: FilePath -> FilePath -> IO [Int]
seeded home root = withRunlog (runlogPath home) $ \rl -> do
  startRun rl (newRun "s1" SearchRun Nothing root 100)
  let pk = projectKeyOf root
  mapM
    (upsertLesson rl)
    [ lesson pk "unknown_identifier" "unknown_identifier:Nat.foo" activeText Active ["s1", "s2", "s3"] 0
    , lesson pk "judged" "check-lemma" pinnedText Pinned ["s1", "s2"] 2
    , lesson pk "no_check_before_finish" "no_check_before_finish" candText Candidate ["s1"] 0
    , lesson pk "unknown_identifier" "unknown_identifier:Nat.bar" droppedText Dropped ["s1"] 0
    , lesson "*" "no_check_before_finish" "no_check_before_finish" globalText Active ["a", "b", "c"] 0 ]

lineWith :: Text -> Text -> Maybe [Text]
lineWith needle out = case filter (T.isInfixOf needle) (T.lines out) of
  l : _ -> Just (T.words l)
  [] -> Nothing

statusOf :: FilePath -> Int -> IO (Maybe LessonStatus)
statusOf home i = withRunlog (runlogPath home) $ \rl -> fmap lrStatus <$> getLesson rl i

-- | A search run of the project with attempts: each its events, its class
-- and its outcome text (a @finished@ event on the run).
searchRun :: Runlog -> FilePath -> Text -> Double -> [([(Double, Text, Value)], Text, Text)] -> IO ()
searchRun rl root rid t attempts = do
  startRun rl (newRun rid SearchRun Nothing root t)
  forM_ (zip [1 :: Int ..] attempts) $ \(n, (es, cls, outcome)) -> do
    let tag = rid <> "-" <> T.pack (show n)
    startRun rl (newRun tag SearchAttempt (Just rid) root (t + fromIntegral n))
    event rl tag 0 "attempt" (object ["goal" .= ("g" :: Text), "shape" .= ("arith" :: Text), "backend" .= ("fisher" :: Text)])
    forM_ es $ \(et, k, d) -> event rl tag et k d
    endRun rl tag (RunEnd (t + 10) Nothing cls Nothing Nothing Nothing Nothing)
    event rl rid 5 "finished" (object ["attempt" .= n, "goal" .= ("g" :: Text), "outcome" .= outcome])

unknownFoo :: ([(Double, Text, Value)], Text, Text)
unknownFoo =
  ( [ (1, "tool_start", object ["name" .= ("lean_diagnostic_messages" :: Text), "args" .= (Nothing :: Maybe Text), "call_id" .= ("c1" :: Text)])
    , (2, "tool_end", object ["name" .= ("lean_diagnostic_messages" :: Text), "call_id" .= ("c1" :: Text), "ok" .= True
                            , "result" .= ("{\"items\":[{\"severity\":\"error\",\"message\":\"unknown identifier 'Nat.foo'\"}]}" :: Text)]) ]
  , "failed", "exit 1" )

residue :: ([(Double, Text, Value)], Text, Text)
residue = ([(1, "message", object ["role" .= ("assistant" :: Text), "text" .= ("trying omega" :: Text)])], "failed", "exit 1")

verdict :: Text
verdict = "{\"mistake\": \"it guessed\", \"lesson\": \"Check the type of `Nat.add_comm` before using it.\", \"confidence\": 0.9, \"tell_yourself\": \"Look up the lemma first. Then use it.\"}"

fakeJudge :: IORef Int -> Ask
fakeJudge calls _ _ = do
  modifyIORef' calls (+ 1)
  pure (Right (verdict, 0.01))

spec :: Spec
spec = do
  it "list shows the active and pinned lines with their evidence counts, then the global ones" $ withDirs $ \home root -> do
    void (seeded home root)
    out <- lessonsIn home root ["list", "--project", root]
    case lineWith "Nat.foo" out of
      Just ws -> do
        ws `shouldSatisfy` elem "active"
        ws `shouldSatisfy` elem "3"
      Nothing -> expectationFailure ("no active line in:\n" <> T.unpack out)
    case lineWith "type of the lemma" out of
      Just ws -> do
        ws `shouldSatisfy` elem "pinned"
        ws `shouldSatisfy` elem "2"
      Nothing -> expectationFailure ("no pinned line in:\n" <> T.unpack out)
    out `shouldNotSatisfy` T.isInfixOf "lake build"
    out `shouldNotSatisfy` T.isInfixOf "nobody wants"
    let ls = T.lines out
        globalAt = length (takeWhile (not . T.isPrefixOf "global") ls)
        lineAt needle = length (takeWhile (not . T.isInfixOf needle) ls)
    globalAt `shouldSatisfy` (< length ls)
    lineAt "compiles" `shouldSatisfy` (> globalAt)
    lineAt "Nat.foo" `shouldSatisfy` (< globalAt)

  it "list --all adds the candidates" $ withDirs $ \home root -> do
    void (seeded home root)
    out <- lessonsIn home root ["list", "--all", "--project", root]
    case lineWith "lake build" out of
      Just ws -> ws `shouldSatisfy` elem "candidate"
      Nothing -> expectationFailure ("no candidate line in:\n" <> T.unpack out)

  it "list cuts a long text to 100 characters" $ withDirs $ \home root -> do
    withRunlog (runlogPath home) $ \rl -> do
      startRun rl (newRun "s1" SearchRun Nothing root 100)
      void (upsertLesson rl (lesson (projectKeyOf root) "judged" "long" (T.replicate 30 "abcdefg ") Active ["s1"] 1))
    out <- lessonsIn home root ["list", "--project", root]
    case filter (T.isInfixOf "abcdefg") (T.lines out) of
      [l] -> T.length (snd (T.breakOn "abcdefg" l)) `shouldBe` 100
      _ -> expectationFailure ("no single long line in:\n" <> T.unpack out)

  it "show ID prints the text, status, evidence runs and the counters" $ withDirs $ \home root -> do
    [a, _, _, _, _] <- seeded home root
    out <- lessonsIn home root ["show", show a]
    out `shouldSatisfy` T.isInfixOf activeText
    out `shouldSatisfy` T.isInfixOf "active"
    out `shouldSatisfy` T.isInfixOf "s1, s2, s3"
    out `shouldSatisfy` T.isInfixOf "in force"
    out `shouldSatisfy` T.isInfixOf "before"
    missing <- lessonsIn home root ["show", "999"]
    T.strip missing `shouldBe` "no lesson 999"

  it "pin, unpin and drop change the status, and list follows" $ withDirs $ \home root -> do
    [a, p, c, _, _] <- seeded home root
    _ <- lessonsIn home root ["pin", show c]
    statusOf home c `shouldReturn` Just Pinned
    -- p is a judged lesson: unpinned, it waits for a pin again
    unpinOut <- lessonsIn home root ["unpin", show p]
    unpinOut `shouldSatisfy` T.isInfixOf ("lesson " <> T.pack (show p) <> ": unpinned; it is proposed again until you pin it")
    statusOf home p `shouldReturn` Just Candidate
    _ <- lessonsIn home root ["drop", show a]
    statusOf home a `shouldReturn` Just Dropped
    out <- lessonsIn home root ["list", "--project", root]
    out `shouldNotSatisfy` T.isInfixOf "Nat.foo"
    (lineWith "lake build" out >>= Just . elem "pinned") `shouldBe` Just True
    out `shouldNotSatisfy` T.isInfixOf "type of the lemma"
    -- the project file follows the change
    f <- readLessonsFile (projectLessonsPath root)
    fmap (map snd . lfGenerated) f `shouldBe` Just [candText]
    -- a rule lesson unpinned is active, with no such line
    ruleOut <- lessonsIn home root ["unpin", show c]
    ruleOut `shouldNotSatisfy` T.isInfixOf "proposed again"
    statusOf home c `shouldReturn` Just Active

  it "list has a proposed section: judged and self candidates with the counts, waiting for a pin" $ withDirs $ \home root -> do
    let pk = projectKeyOf root
        proposedText = "Check `Foo.bar_baz` before using it."
        shortText = "Look up `Foo.qux` first."
    [j, _, r] <- withRunlog (runlogPath home) $ \rl -> do
      startRun rl (newRun "s1" SearchRun Nothing root 100)
      mapM (upsertLesson rl)
        [ lesson pk "judged" "foo.bar_baz" proposedText Candidate ["s1", "s2", "s3"] 2
        , lesson pk "self" "foo.qux" shortText Candidate ["s1", "s2", "s3"] 1
        , lesson pk "no_check_before_finish" "no_check_before_finish" candText Candidate ["s1", "s2", "s3"] 0 ]
    out <- lessonsIn home root ["list", "--project", root]
    let ls = T.lines out
        at needle = length (takeWhile (not . T.isInfixOf needle) ls)
    at "proposed" `shouldSatisfy` (< length ls)
    at "Foo.bar_baz" `shouldSatisfy` (> at "proposed")
    at "Foo.bar_baz" `shouldSatisfy` (< at "global (*)")
    (lineWith "Foo.bar_baz" out >>= Just . elem (T.pack (show j))) `shouldBe` Just True
    -- the hint names no id to paste: read it first
    out `shouldSatisfy` T.isInfixOf "fisher lessons show ID, then approve it with: fisher lessons pin ID"
    -- one agreement short, and a rule candidate: not proposed (and not listed without --all)
    out `shouldNotSatisfy` T.isInfixOf "Foo.qux"
    out `shouldNotSatisfy` T.isInfixOf "lake build"
    -- with --all it is listed once, in the proposed section
    allOut <- lessonsIn home root ["list", "--all", "--project", root]
    length (filter (T.isInfixOf "Foo.bar_baz") (T.lines allOut)) `shouldBe` 1
    allOut `shouldSatisfy` T.isInfixOf "Foo.qux"
    -- pin is the approval: it is shown, said so, and leaves the section
    pinOut <- lessonsIn home root ["pin", show j]
    pinOut `shouldSatisfy` T.isInfixOf "approved"
    statusOf home j `shouldReturn` Just Pinned
    f <- readLessonsFile (projectLessonsPath root)
    fmap (map snd . lfGenerated) f `shouldBe` Just [proposedText]
    later <- lessonsIn home root ["list", "--project", root]
    later `shouldNotSatisfy` T.isInfixOf "proposed"
    (lineWith "Foo.bar_baz" later >>= Just . elem "pinned") `shouldBe` Just True
    -- pinning a rule candidate, or a judged row that was not proposed, says nothing of an approval
    ruleOut <- lessonsIn home root ["pin", show r]
    ruleOut `shouldNotSatisfy` T.isInfixOf "approved"
    a <- withRunlog (runlogPath home) $ \rl ->
      upsertLesson rl (lesson pk "judged" "foo.act" "Use `Foo.act_now` here." Active ["s1", "s2", "s3"] 2)
    actOut <- lessonsIn home root ["pin", show a]
    actOut `shouldNotSatisfy` T.isInfixOf "approved"
    statusOf home a `shouldReturn` Just Pinned

  it "list shows a proposed lesson's whole text, wrapped, not cut to 100 characters" $ withDirs $ \home root -> do
    let long = "Check `Foo.bar_baz` before using it. " <> T.replicate 14 "Then look again. " <> "The very end is here."
    T.length long `shouldSatisfy` (> 150)
    withRunlog (runlogPath home) $ \rl -> do
      startRun rl (newRun "s1" SearchRun Nothing root 100)
      void (upsertLesson rl (lesson (projectKeyOf root) "judged" "foo.bar_baz" long Candidate ["s1", "s2", "s3"] 2))
    out <- lessonsIn home root ["list", "--project", root]
    T.unwords (concatMap T.words (dropWhile (not . T.isInfixOf "Foo.bar_baz") (T.lines out))) `shouldSatisfy` T.isInfixOf long
    all ((<= 140) . T.length) (T.lines out) `shouldBe` True

  it "with fisher.lessons.auto_judged on, list proposes nothing" $ withDirs $ \home root -> do
    withRunlog (runlogPath home) $ \rl -> do
      startRun rl (newRun "s1" SearchRun Nothing root 100)
      void (upsertLesson rl (lesson (projectKeyOf root) "judged" "foo.bar_baz" "Check `Foo.bar_baz` before using it." Candidate ["s1", "s2", "s3"] 2))
    out <- lessonsWithCfg (object ["fisher" .= object ["lessons" .= object ["auto_judged" .= True]]]) home root ["list", "--project", root]
    out `shouldNotSatisfy` T.isInfixOf "proposed"

  describe "pin, by the lesson's status" $ do
    let one home root st = withRunlog (runlogPath home) $ \rl -> do
          startRun rl (newRun "s1" SearchRun Nothing root 100)
          i <- upsertLesson rl (lesson (projectKeyOf root) "unknown_identifier" "unknown_identifier:Nat.foo" activeText Candidate ["s1"] 0)
          case st of
            Candidate -> pure ()
            Retired -> do
              setLessonStatus rl i Active 200
              void (modifyLesson rl i (\l -> l {lrRunsActive = ["r1", "r2"], lrHitsWhileActive = 3, lrAttemptsWhileActive = 9}))
              setLessonStatus rl i Retired 300
            _ -> setLessonStatus rl i st 200
          pure i
        row home i = withRunlog (runlogPath home) (\rl -> getLesson rl i)
    it "a candidate becomes pinned, activated now" $ withDirs $ \home root -> do
      i <- one home root Candidate
      _ <- lessonsIn home root ["pin", show i]
      fmap (\l -> (lrStatus l, lrActivatedAt l)) <$> row home i `shouldReturn` Just (Pinned, Just 1000)
    it "an active one becomes pinned, keeping its activation" $ withDirs $ \home root -> do
      i <- one home root Active
      _ <- lessonsIn home root ["pin", show i]
      fmap (\l -> (lrStatus l, lrActivatedAt l)) <$> row home i `shouldReturn` Just (Pinned, Just 200)
    it "a retired one is pinned afresh: activated now, the in-force counters cleared" $ withDirs $ \home root -> do
      i <- one home root Retired
      _ <- lessonsIn home root ["pin", show i]
      fmap (\l -> (lrStatus l, lrActivatedAt l, lrRetiredAt l)) <$> row home i `shouldReturn` Just (Pinned, Just 1000, Nothing)
      fmap (\l -> (lrRunsActive l, lrHitsWhileActive l, lrAttemptsWhileActive l)) <$> row home i `shouldReturn` Just ([], 0, 0)
    it "a dropped one is refused" $ withDirs $ \home root -> do
      i <- one home root Dropped
      out <- lessonsIn home root ["pin", show i]
      out `shouldSatisfy` T.isInfixOf "dropped, nothing changed"
      fmap lrStatus <$> row home i `shouldReturn` Just Dropped
    it "a pinned one is left as it is" $ withDirs $ \home root -> do
      i <- one home root Pinned
      out <- lessonsIn home root ["pin", show i]
      out `shouldSatisfy` T.isInfixOf "nothing changed"
      fmap (\l -> (lrStatus l, lrActivatedAt l)) <$> row home i `shouldReturn` Just (Pinned, Just 200)

  it "unpin on a lesson that is not pinned changes nothing" $ withDirs $ \home root -> do
    [_, _, c, _, _] <- seeded home root
    out <- lessonsIn home root ["unpin", show c]
    out `shouldSatisfy` T.isInfixOf "not pinned"
    statusOf home c `shouldReturn` Just Candidate

  it "mine --since 7d with a fake judge reports the counts and the day's judge spend" $ withDirs $ \home root -> do
    withRunlog (runlogPath home) $ \rl -> searchRun rl root "s1" 900 [unknownFoo, residue]
    calls <- newIORef (0 :: Int)
    out <- lessonsWith (Just (fakeJudge calls)) home root ["mine", "--since", "7d", "--project", root]
    readIORef calls `shouldReturn` 1
    out `shouldSatisfy` T.isInfixOf "mined 1"
    out `shouldSatisfy` T.isInfixOf "judged 1"
    out `shouldSatisfy` T.isInfixOf "unjudged 0"
    out `shouldSatisfy` T.isInfixOf "$0.01"
    again <- lessonsWith (Just (fakeJudge calls)) home root ["mine", "--project", root]
    again `shouldSatisfy` T.isInfixOf "mined 0"
    readIORef calls `shouldReturn` 1

  it "reset deletes every lesson of the project and regenerates the file with only the ## Yours lines" $ withDirs $ \home root -> do
    ids <- seeded home root
    let path = projectLessonsPath root
        key = projectKeyOf root
    createDirectoryIfMissing True (root </> ".fisher")
    TIO.writeFile path (T.unlines [lessonsHeader, "1. " <> activeText, "", "## Yours", "- my own line"])
    out <- lessonsIn home root ["reset", "--project", root]
    out `shouldSatisfy` T.isInfixOf ("deleted 4 lessons of " <> T.pack root <> " (" <> key <> ")")
    out `shouldSatisfy` T.isInfixOf "mine --again"
    withRunlog (runlogPath home) $ \rl -> do
      listLessons rl (Just key) [] `shouldReturn` []
      -- a deleted key can be proposed again
      i <- upsertLesson rl (lesson key "unknown_identifier" "unknown_identifier:Nat.bar" droppedText Candidate ["s9"] 0)
      fmap lrStatus <$> getLesson rl i `shouldReturn` Just Candidate
    statusOf home (ids !! 4) `shouldReturn` Just Active
    f <- readLessonsFile path
    fmap lfGenerated f `shouldBe` Just []
    fmap lfYours f `shouldBe` Just ["- my own line"]

  it "an empty run log prints the import hint and makes no log" $ withDirs $ \home root -> do
    forM_ [["list"], ["show", "1"], ["mine"]] $ \argv -> do
      out <- lessonsIn home root argv
      T.lines out `shouldSatisfy` elem "no runs logged yet: fisher runs import ~/.cache/fisher/search"
    doesFileExist (runlogPath home) `shouldReturn` False

  it "fisher lessons --help lists the seven commands" $
    case execParserPure defaultPrefs (info (lessonsParser <**> helper) mempty) ["--help"] of
      Failure f -> do
        let (text, _) = renderFailure f "fisher lessons"
        forM_ ["list", "show", "pin", "unpin", "drop", "mine", "reset"] $ \c -> T.words (T.pack text) `shouldSatisfy` elem c
      _ -> expectationFailure "--help did not fail with the help text"
