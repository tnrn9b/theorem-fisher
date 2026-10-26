-- | @GET /api/runs-log@, @GET /api/runs-log/:id@ and @GET /api/learn@ over a
-- scratch FISHER_HOME whose runs.db holds two runs: a search and one of its
-- attempts. runs.db is only read: no @-wal@ or @-shm@ file appears beside
-- it, and a home with no runs.db reads as empty. @GET /api/lessons@ and
-- @POST /api/lessons@ over the same home with lessons of a scratch project
-- and a global one.
module Fisher.UI.RunlogSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as S
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Lessons.File (globalLessonsPath, projectLessonsPath)
import Fisher.Paths (configPathIn)
import Fisher.Learn.Policy (GoalPolicy (..), Policy (..), policyPath, writePolicy)
import Fisher.Runlog
import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Runlog (runlogApi)
import Fisher.UI.Db (newUiEnv)
import Fisher.UI.Server (serve)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

cookieH :: Header
cookieH = (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")

project :: FilePath
project = "/tmp/p"

run :: Text -> RunKind -> Maybe Text -> Maybe Text -> Double -> NewRun
run i k par backend t =
  NewRun
    { nrId = i, nrKind = k, nrParent = par, nrProject = Just project, nrProver = Just "lean", nrBackend = backend
    , nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = ["fisher", "search", "Demo.lean"]
    , nrStartedAt = t, nrRawDir = Nothing, nrCapture = Full }

-- | The search s-1 (started at 100, one goal) and its closed attempt a-1
-- (started at 200, backend fisher), plus @extra@ events on a-1.
seed :: FilePath -> Int -> IO ()
seed home extra = withRunlog (runlogPath home) $ \rl -> do
  startRun rl (run "s-1" SearchRun Nothing Nothing 100)
  upsertGoal rl "s-1" (GoalRow "g1" "1 + 1 = 2" "eq||Nat|small" "closed" (Just "fisher") (Just 30) (Just 0.5))
  event rl "s-1" 0 "portfolio" (A.object ["tactics" A..= (["simp"] :: [Text])])
  endRun rl "s-1" (RunEnd 300 (Just 0) "closed" Nothing Nothing Nothing Nothing)
  startRun rl (run "a-1" SearchAttempt (Just "s-1") (Just "fisher") 200)
  event rl "a-1" 0 "attempt" (A.object ["goal" A..= ("g1" :: Text), "shape" A..= ("eq||Nat|small" :: Text), "backend" A..= ("fisher" :: Text)])
  forM_ [1 .. extra] $ \i -> event rl "a-1" (fromIntegral i) "step" (A.object [])
  endRun rl "a-1" (RunEnd 230 (Just 0) "closed" (Just 0.5) Nothing Nothing Nothing)

withHome :: Maybe Int -> (FilePath -> IO a) -> IO a
withHome seeded k = withSystemTempDirectory "fisher-ui-runlog" $ \tmp -> do
  let home = tmp </> "home"
  createDirectoryIfMissing True home
  mapM_ (seed home) seeded
  k home

app :: FilePath -> IO Application
app home = do
  env <- newUiEnv home (home </> "bench") home
  pure (serve gate [runlogApi env])

get :: FilePath -> [Header] -> BC.ByteString -> IO (Int, A.Value)
get home hs path = do
  a <- app home
  let (p, q) = BC.break (== '?') path
      req =
        setPath
          defaultRequest
            { requestMethod = methodGet
            , requestHeaderHost = Just "127.0.0.1:8732"
            , requestHeaders = hs <> [("Host", "127.0.0.1:8732")] }
          (p <> q)
  r <- runSession (srequest (SRequest req "")) a
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r) :: Maybe A.Value))

-- | A POST with the cookie, the JSON body and these headers (the Origin one
-- or none).
post :: FilePath -> [Header] -> BLC.ByteString -> IO (Int, A.Value)
post home hs body = do
  a <- app home
  let req =
        setPath
          defaultRequest
            { requestMethod = methodPost
            , requestHeaderHost = Just "127.0.0.1:8732"
            , requestHeaders = hs <> [cookieH, (hContentType, "application/json"), ("Host", "127.0.0.1:8732")] }
          "/api/lessons"
  r <- runSession (srequest (SRequest req body)) a
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r) :: Maybe A.Value))

originH :: Header
originH = ("Origin", "http://127.0.0.1:8732")

lesson :: Text -> Text -> Text -> LessonStatus -> [Text] -> LessonRow
lesson pk k txt st runs =
  LessonRow
    { lrId = 0, lrProjectKey = pk, lrKind = "unknown_identifier", lrKey = k, lrText = txt, lrStatus = st
    , lrEvidenceRuns = runs, lrAgreements = 0, lrFirstSeen = 50, lrActivatedAt = if st == Candidate then Nothing else Just 60
    , lrRetiredAt = Nothing, lrRunsActive = [], lrHitsWhileActive = 0, lrAttemptsWhileActive = 0, lrHitsBefore = 0
    , lrAttemptsBefore = 0, lrRunsBefore = 0, lrBeforeWindow = [], lrAgreedAttempts = [] }

candText, activeText, droppedText, globalText :: Text
candText = "Run `lake build` before finishing."
activeText = "In this project `Nat.foo` does not exist."
droppedText = "An old lesson nobody wants."
globalText = "Always check the file compiles before you finish."

-- | The home with the two runs, a run r-1 of the project root beside it,
-- and for that root a candidate (two evidence runs), an active and a
-- dropped lesson, and a global active one; the root and their ids in that
-- order.
withLessons :: (FilePath -> FilePath -> [Int] -> IO a) -> IO a
withLessons k = withHome (Just 0) $ \home -> do
  let root = takeDirectory home </> "proj"
      pk = projectKeyOf root
  createDirectoryIfMissing True root
  is <- withRunlog (runlogPath home) $ \rl -> do
    startRun rl (run "r-1" SearchAttempt Nothing (Just "fisher") 500) {nrProject = Just root}
    mapM
      (upsertLesson rl)
      [ lesson pk "k:cand" candText Candidate ["r-1", "a-1"]
      , lesson pk "k:active" activeText Active ["r-1"]
      , lesson pk "k:dropped" droppedText Dropped ["a-1"]
      , lesson "*" "k:global" globalText Active ["a-1"] ]
  k home root is

lessonsQ :: FilePath -> BC.ByteString
lessonsQ root = "/api/lessons?project=" <> BC.pack root

actionBody :: Int -> Text -> FilePath -> BLC.ByteString
actionBody i act root = A.encode (A.object ["id" A..= i, "action" A..= act, "project" A..= root])

getOk :: FilePath -> BC.ByteString -> IO A.Value
getOk home path = do
  (s, v) <- get home [cookieH] path
  s `shouldBe` 200
  pure v

key :: Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

ids :: A.Value -> [A.Value]
ids = map (key "id") . arr

spec :: Spec
spec = do
  describe "GET /api/runs-log" $ do
    it "lists the runs newest first" $ withHome (Just 0) $ \home -> do
      v <- getOk home "/api/runs-log"
      ids v `shouldBe` ["a-1", "s-1"]
      map (key "kind") (arr v) `shouldBe` ["search-attempt", "search"]
      map (key "project") (arr v) `shouldBe` [A.String "/tmp/p", A.String "/tmp/p"]

    it "filters by kind, project, since and limit" $ withHome (Just 0) $ \home -> do
      (ids <$> getOk home "/api/runs-log?kind=search-attempt") `shouldReturn` ["a-1"]
      (ids <$> getOk home "/api/runs-log?project=/tmp/p&limit=1") `shouldReturn` ["a-1"]
      (ids <$> getOk home "/api/runs-log?project=/elsewhere") `shouldReturn` []
      (ids <$> getOk home "/api/runs-log?since=150") `shouldReturn` ["a-1"]

    it "refuses a kind it does not know, a since that is not a number and a relative project (400)" $ withHome (Just 0) $ \home -> do
      (fst <$> get home [cookieH] "/api/runs-log?kind=bogus") `shouldReturn` 400
      (fst <$> get home [cookieH] "/api/runs-log?since=yesterday") `shouldReturn` 400
      (fst <$> get home [cookieH] "/api/runs-log?project=relative/p") `shouldReturn` 400

    it "is empty when there is no runs.db, and makes none" $ withHome Nothing $ \home -> do
      (arr <$> getOk home "/api/runs-log") `shouldReturn` []
      listDirectory home `shouldReturn` []

    it "only reads runs.db: no -wal or -shm file appears" $ withHome (Just 0) $ \home -> do
      let dir = takeDirectory (runlogPath home)
      listed <- sort <$> listDirectory dir
      _ <- getOk home "/api/runs-log"
      _ <- getOk home "/api/runs-log/a-1"
      _ <- getOk home "/api/learn?project=/tmp/p"
      (sort <$> listDirectory dir) `shouldReturn` listed

    it "reads runs.db while a writer has it open (mode=ro over the live WAL), seeing its new rows" $ withHome (Just 0) $ \home ->
      withRunlog (runlogPath home) $ \rl -> do
        startRun rl (run "a-2" SearchAttempt (Just "s-1") (Just "codex") 400)
        event rl "a-2" 0 "attempt" (A.object ["goal" A..= ("g1" :: Text), "shape" A..= ("eq||Nat|small" :: Text)])
        endRun rl "a-2" (RunEnd 410 (Just 1) "failed" (Just 0.1) Nothing Nothing Nothing)
        (ids <$> getOk home "/api/runs-log") `shouldReturn` ["a-2", "a-1", "s-1"]
        v <- getOk home "/api/runs-log/a-2"
        map (key "kind") (arr (key "events" v)) `shouldBe` ["attempt"]
        w <- getOk home "/api/learn?project=/tmp/p"
        map (key "arm") (arr (key "backends" (key "*" (key "stats" w)))) `shouldBe` ["codex", "fisher"]
        -- and it writes again after the reads
        event rl "a-2" 1 "step" (A.object [])
        v' <- getOk home "/api/runs-log/a-2"
        length (arr (key "events" v')) `shouldBe` 2

    it "is 401 without the cookie" $ withHome (Just 0) $ \home -> do
      (fst <$> get home [] "/api/runs-log") `shouldReturn` 401
      (fst <$> get home [] "/api/runs-log/a-1") `shouldReturn` 401
      (fst <$> get home [] "/api/learn?project=/tmp/p") `shouldReturn` 401

  describe "GET /api/runs-log/:id" $ do
    it "gives the run with its goals and events" $ withHome (Just 0) $ \home -> do
      v <- getOk home "/api/runs-log/s-1"
      key "id" (key "run" v) `shouldBe` "s-1"
      map (key "name") (arr (key "goals" v)) `shouldBe` ["g1"]
      map (key "kind") (arr (key "events" v)) `shouldBe` ["portfolio"]
      key "truncated" v `shouldBe` A.Bool False

    it "gives the first 500 events only" $ withHome (Just 600) $ \home -> do
      v <- getOk home "/api/runs-log/a-1"
      length (arr (key "events" v)) `shouldBe` 500
      key "seq" (head (arr (key "events" v))) `shouldBe` A.Number 1
      key "truncated" v `shouldBe` A.Bool True

    it "is 404 for a run it does not have" $ withHome (Just 0) $ \home -> do
      (fst <$> get home [cookieH] "/api/runs-log/nope") `shouldReturn` 404

  describe "GET /api/learn" $ do
    it "gives the project's stats tables, no policy and no changes" $ withHome (Just 0) $ \home -> do
      v <- getOk home "/api/learn?project=/tmp/p"
      key "projects" v `shouldBe` A.toJSON ["/tmp/p" :: Text]
      key "policy" v `shouldBe` A.Null
      arr (key "changes" v) `shouldBe` []
      let backends sh = arr (key "backends" (key sh (key "stats" v)))
      map (key "arm") (backends "*") `shouldBe` ["fisher"]
      map (key "arm") (backends "eq||Nat|small") `shouldBe` ["fisher"]
      case backends "*" of
        row : _ -> do
          key "attempts" row `shouldBe` A.Number 1
          key "closes" row `shouldBe` A.Number 1
          key "cost_per_close" row `shouldSatisfy` (/= A.Null)
          key "rate" row `shouldSatisfy` (/= A.Null)
        [] -> expectationFailure "no backends row"
      arr (key "tactics" (key "*" (key "stats" v))) `shouldBe` []

    it "gives the policy file and the newest changes when there are some" $ withHome (Just 0) $ \home -> do
      let k = projectKeyOf project
      writePolicy (policyPath home k)
        (Policy 400 k (Map.fromList [("*", GoalPolicy ["codex", "fisher"] [] ["simp"] (Just 120) False (A.object []))]))
      withRunlog (runlogPath home) $ \rl ->
        forM_ [1 .. 60 :: Int] $ \i ->
          recordPolicyChange rl (fromIntegral i) k "*" "backends" (A.toJSON ["fisher" :: Text]) (A.toJSON ["codex" :: Text]) (A.object [])
      v <- getOk home "/api/learn?project=/tmp/p"
      key "backends" (key "*" (key "shapes" (key "policy" v))) `shouldBe` A.toJSON ["codex", "fisher" :: Text]
      -- the policy's order first, then the arms only the log has
      map (key "arm") (arr (key "backends" (key "*" (key "stats" v)))) `shouldBe` ["codex", "fisher"]
      length (arr (key "changes" v)) `shouldBe` 50
      key "at" (head (arr (key "changes" v))) `shouldBe` A.Number 60

    it "lists the projects and no stats without a project, and is empty with no runs.db" $ withHome Nothing $ \home -> do
      v <- getOk home "/api/learn?project=/tmp/p"
      key "projects" v `shouldBe` A.toJSON ([] :: [Text])
      key "stats" v `shouldBe` A.object []
      key "policy" v `shouldBe` A.Null
      w <- getOk home "/api/learn"
      key "stats" w `shouldBe` A.object []

    it "echoes the project without a trailing separator, with the same key" $ withHome (Just 0) $ \home -> do
      v <- getOk home "/api/learn?project=/tmp/p/"
      key "project" v `shouldBe` "/tmp/p"
      key "project_key" v `shouldBe` A.toJSON (projectKeyOf project)

    it "refuses a project that is not an absolute path (400)" $ withHome (Just 0) $ \home -> do
      (fst <$> get home [cookieH] "/api/learn?project=relative/p") `shouldReturn` 400

  describe "GET /api/lessons" $ do
    it "gives the project's lessons and the global ones" $ withLessons $ \home root is -> do
      v <- getOk home (lessonsQ root)
      key "project" v `shouldBe` A.toJSON root
      key "project_key" v `shouldBe` A.toJSON (projectKeyOf root)
      map (key "id") (arr (key "lessons" v)) `shouldBe` map A.toJSON (take 3 is)
      map (key "status") (arr (key "lessons" v)) `shouldBe` ["candidate", "active", "dropped"]
      map (key "evidence_runs") (arr (key "lessons" v)) `shouldBe` [A.toJSON ["r-1", "a-1" :: Text], A.toJSON ["r-1" :: Text], A.toJSON ["a-1" :: Text]]
      map (key "text") (arr (key "global" v)) `shouldBe` [A.String globalText]

    it "marks a judged or self candidate with the counts proposed (awaiting a pin), and no other row" $ withLessons $ \home root is -> do
      j <- withRunlog (runlogPath home) $ \rl -> do
        _ <- upsertLesson rl (lesson (projectKeyOf root) "k:short" "Look up `Foo.qux` first." Candidate ["r-1", "a-1", "b-1"]) {lrKind = "self", lrAgreements = 1}
        upsertLesson rl (lesson (projectKeyOf root) "k:judged" "Check `Foo.bar_baz` before using it." Candidate ["r-1", "a-1", "b-1"]) {lrKind = "judged", lrAgreements = 2}
      v <- getOk home (lessonsQ root)
      let flags = [(key "id" x, key "proposed" x) | x <- arr (key "lessons" v) <> arr (key "global" v)]
      [i | (i, A.Bool True) <- flags] `shouldBe` [A.toJSON j]
      length [() | (_, A.Bool False) <- flags] `shouldBe` length is + 1
      -- with fisher.lessons.auto_judged: true nothing waits for a pin
      TIO.writeFile (configPathIn home) "fisher:\n  lessons:\n    auto_judged: true\n"
      w <- getOk home (lessonsQ root)
      [x | x <- arr (key "lessons" w), key "proposed" x == A.Bool True] `shouldBe` []

    it "gives only the global ones without a project" $ withLessons $ \home _ _ -> do
      v <- getOk home "/api/lessons"
      key "project" v `shouldBe` A.Null
      arr (key "lessons" v) `shouldBe` []
      map (key "status") (arr (key "global" v)) `shouldBe` ["active"]

    it "is empty with no runs.db, and makes none" $ withHome Nothing $ \home -> do
      v <- getOk home "/api/lessons?project=/tmp/p"
      arr (key "lessons" v) `shouldBe` []
      arr (key "global" v) `shouldBe` []
      listDirectory home `shouldReturn` []

    it "is empty on an older runs.db without the lessons table, and only reads it" $ withHome (Just 0) $ \home -> do
      c <- S.open (runlogPath home)
      S.execute_ c "DROP TABLE lessons"
      S.close c
      let dir = takeDirectory (runlogPath home)
      listed <- sort <$> listDirectory dir
      v <- getOk home "/api/lessons?project=/tmp/p"
      arr (key "lessons" v) `shouldBe` []
      arr (key "global" v) `shouldBe` []
      (sort <$> listDirectory dir) `shouldReturn` listed

    it "refuses a relative project (400) and is 401 without the cookie" $ withLessons $ \home _ _ -> do
      (fst <$> get home [cookieH] "/api/lessons?project=relative/p") `shouldReturn` 400
      (fst <$> get home [] "/api/lessons") `shouldReturn` 401

  describe "POST /api/lessons" $ do
    it "unpins a judged lesson to a proposed candidate, and a pin answers it not proposed" $ withLessons $ \home root _ -> do
      j <- withRunlog (runlogPath home) $ \rl ->
        upsertLesson rl (lesson (projectKeyOf root) "judged:x" "Check `Foo.bar_baz` before using it." Pinned ["r-1", "a-1", "b-1"]) {lrKind = "judged", lrAgreements = 2}
      (s, v) <- post home [originH] (actionBody j "unpin" root)
      s `shouldBe` 200
      (key "status" v, key "proposed" v) `shouldBe` ("candidate", A.Bool True)
      (s2, v2) <- post home [originH] (actionBody j "pin" root)
      s2 `shouldBe` 200
      (key "status" v2, key "proposed" v2) `shouldBe` ("pinned", A.Bool False)
    it "pins a candidate, answers the row and regenerates the project's file" $ withLessons $ \home root is -> do
      let cand = head is
      (s, v) <- post home [originH] (actionBody cand "pin" root)
      s `shouldBe` 200
      key "id" v `shouldBe` A.toJSON cand
      key "status" v `shouldBe` "pinned"
      key "activated_at" v `shouldSatisfy` (/= A.Null)
      w <- getOk home (lessonsQ root)
      map (key "status") (arr (key "lessons" w)) `shouldBe` ["pinned", "active", "dropped"]
      file <- TIO.readFile (projectLessonsPath root)
      file `shouldSatisfy` T.isInfixOf candText
      file `shouldSatisfy` T.isInfixOf activeText
      file `shouldNotSatisfy` T.isInfixOf droppedText
      TIO.readFile (globalLessonsPath home) >>= (`shouldSatisfy` T.isInfixOf globalText)

    it "unpins a pinned lesson, and drops one, the file following" $ withLessons $ \home root is -> do
      let [cand, active, _, _] = is
      _ <- post home [originH] (actionBody cand "pin" root)
      (s, v) <- post home [originH] (actionBody cand "unpin" root)
      s `shouldBe` 200
      key "status" v `shouldBe` "active"
      (s', v') <- post home [originH] (actionBody active "drop" root)
      s' `shouldBe` 200
      key "status" v' `shouldBe` "dropped"
      TIO.readFile (projectLessonsPath root) >>= (`shouldNotSatisfy` T.isInfixOf activeText)

    it "finds the project's root from the lesson's runs when the body names none" $ withLessons $ \home root is -> do
      (s, v) <- post home [originH] (A.encode (A.object ["id" A..= head is, "action" A..= ("pin" :: Text)]))
      s `shouldBe` 200
      key "status" v `shouldBe` "pinned"
      TIO.readFile (projectLessonsPath root) >>= (`shouldSatisfy` T.isInfixOf candText)
      doesFileExist (globalLessonsPath home) `shouldReturn` True

    it "refuses a transition the rules do not allow (409) with the reason, changing nothing" $ withLessons $ \home root is -> do
      let [cand, _, dropped, _] = is
      (s, v) <- post home [originH] (actionBody dropped "pin" root)
      s `shouldBe` 409
      key "error" v `shouldBe` "dropped"
      (s', v') <- post home [originH] (actionBody cand "unpin" root)
      s' `shouldBe` 409
      key "error" v' `shouldBe` "not pinned"
      (s'', _) <- post home [originH] (actionBody dropped "drop" root)
      s'' `shouldBe` 409
      w <- getOk home (lessonsQ root)
      map (key "status") (arr (key "lessons" w)) `shouldBe` ["candidate", "active", "dropped"]

    it "is 404 for an unknown id, and with no runs.db makes none" $ do
      withLessons $ \home root _ -> (fst <$> post home [originH] (actionBody 999 "pin" root)) `shouldReturn` 404
      withHome Nothing $ \home -> do
        (fst <$> post home [originH] (actionBody 1 "pin" "/tmp/p")) `shouldReturn` 404
        listDirectory home `shouldReturn` []

    it "refuses a bad body or an unknown action (400)" $ withLessons $ \home root is -> do
      (fst <$> post home [originH] "not json") `shouldReturn` 400
      (fst <$> post home [originH] (A.encode (A.object ["action" A..= ("pin" :: Text)]))) `shouldReturn` 400
      (fst <$> post home [originH] (actionBody (head is) "promote" root)) `shouldReturn` 400
      (fst <$> post home [originH] (actionBody (head is) "pin" "relative/p")) `shouldReturn` 400

    it "is 403 without the Origin header, changing nothing" $ withLessons $ \home root is -> do
      (fst <$> post home [] (actionBody (head is) "pin" root)) `shouldReturn` 403
      w <- getOk home (lessonsQ root)
      map (key "status") (arr (key "lessons" w)) `shouldBe` ["candidate", "active", "dropped"]
      doesFileExist (projectLessonsPath root) `shouldReturn` False

    it "is 404 on an older runs.db without the lessons table, and no crash" $ withHome (Just 0) $ \home -> do
      c <- S.open (runlogPath home)
      S.execute_ c "DROP TABLE lessons"
      S.close c
      (fst <$> post home [originH] (actionBody 1 "pin" "/tmp/p")) `shouldReturn` 404
      (fst <$> get home [cookieH] "/api/lessons?project=/tmp/p") `shouldReturn` 200

    it "for a global lesson with a project that is not its own, writes the global file and makes no file there" $ withLessons $ \home root is -> do
      let other = takeDirectory root </> "other"
      createDirectoryIfMissing True other
      (s, v) <- post home [originH] (actionBody (is !! 3) "pin" other)
      s `shouldBe` 200
      key "status" v `shouldBe` "pinned"
      TIO.readFile (globalLessonsPath home) >>= (`shouldSatisfy` T.isInfixOf globalText)
      doesFileExist (projectLessonsPath other) `shouldReturn` False
      doesFileExist (projectLessonsPath root) `shouldReturn` False

    it "refuses a body over 4 KB (400), changing nothing" $ withLessons $ \home root is -> do
      let big = A.encode (A.object ["id" A..= head is, "action" A..= ("pin" :: Text), "project" A..= root, "pad" A..= T.replicate 5000 "x"])
      (fst <$> post home [originH] big) `shouldReturn` 400
      w <- getOk home (lessonsQ root)
      map (key "status") (arr (key "lessons" w)) `shouldBe` ["candidate", "active", "dropped"]
