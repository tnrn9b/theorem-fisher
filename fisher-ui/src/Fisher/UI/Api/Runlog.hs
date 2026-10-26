-- | The run log ("Fisher.Runlog") and the learned policy
-- ("Fisher.Learn.Policy") as the web app's Run log and Learn views read
-- them:
--
--   * @GET /api/runs-log?since&project&kind&limit@: the runs, newest first,
--     each as 'runRowJson'. @since@ is seconds since the epoch, @project@ a
--     project's root (matched by its key, 'projectKeyOf'), @kind@ a run
--     kind's name (@search-attempt@), @limit@ at most 1000 (default 200).
--   * @GET /api/runs-log/:id@: @{run, goals, events, truncated}@, the
--     events as stored (they are redacted when written) and only the first
--     500 of them; @truncated@ says whether there are more.
--   * @GET /api/learn?project=ROOT@: @{project, project_key, projects,
--     stats, policy, policy_error, changes}@. @stats@ is @{shape: {knob:
--     [{arm, attempts, closes, rate, cost_per_close, closes_per_hour}]}}@
--     with @"*"@ for the project as a whole ('statsTable', the tables of
--     @fisher learn stats@, the arms in the policy's order then the
--     others seen); @policy@ the project's policy file or null;
--     @changes@ its newest 50 recorded policy changes. @projects@ are the
--     projects of the logged runs, latest first (the view's picker).
--   * @GET /api/lessons?project=ROOT@: @{project, project_key, lessons,
--     global}@, the project's lessons (every status) and the global (@*@)
--     ones, each as 'lessonRowJson' with @proposed@: true for a judged or
--     self candidate with the counts that would promote a rule
--     ('awaitingApproval': it waits for a Pin; never with
--     @fisher.lessons.auto_judged: true@ in the home's config.yaml), else
--     false; @lessons@ is empty without a project.
--   * @POST /api/lessons@ @{id, action, project?}@: the action (@pin@,
--     @unpin@, @drop@; 'applyAction', as @fisher lessons@ applies it) on
--     the lesson, then the lessons files regenerated: the project's (under
--     @project@ when it is the lesson's project, else the root of a run of
--     its evidence) and the global one. Answers the row as stored (with
--     @proposed@, as the listing has it: an unpinned judged or self lesson
--     is a candidate, proposed again), with
--     @regenerate_error@ when a file could not be written; 400 for a bad
--     body or an unknown action, 404 for an unknown id (or no runs.db), 409
--     with the reason for an action the lesson's status refuses.
--
-- runs.db is opened read-only, as state.db is ('withRo'); a home with no
-- runs.db reads as no runs, and none is made. Only @POST /api/lessons@
-- opens it writable ('withRunlog'), and only when it is there; an older
-- runs.db without the lessons table reads as no lessons.
module Fisher.UI.Api.Runlog
  ( runlogApi
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Database.SQLite.Simple (SQLError (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (badRequest400, conflict409, internalServerError500, methodGet, methodNotAllowed405, methodPost, notFound404, ok200)
import Network.Wai (Request (..), Response)
import System.Directory (doesFileExist)
import System.FilePath (dropTrailingPathSeparator, isAbsolute, normalise)
import Text.Read (readMaybe)

import Fisher.Config (lessonsAutoJudged, loadConfigAt)
import Fisher.Learn.Lessons (Limits (..), applyAction, awaitingApproval, defaultLimits, globalKey, lessonActions, lessonRoot, regenerate, regenerateGlobal)
import Fisher.Learn.Policy (Policy (..), knobArms, policyFor, policyPath, readPolicy)
import Fisher.Learn.Stats
import Fisher.Runlog
import Fisher.UI.Api.Live (bad, jsonBodyWith, textField)
import Fisher.Paths (configPathIn)
import Fisher.UI.Db (UiEnv (..), withRo)
import Fisher.UI.Response (Api, jsonResponse)

-- | The events a run's page gets at most.
eventCap :: Int
eventCap = 500

-- | The policy changes the Learn view gets at most.
changeCap :: Int
changeCap = 50

runlogApi :: UiEnv -> Api
runlogApi env r = case pathInfo r of
  ["api", "runs-log"] -> only (list env r)
  ["api", "runs-log", rid] -> only (detail env rid)
  ["api", "learn"] -> only (learn env r)
  ["api", "lessons"]
    | requestMethod r == methodPost -> Just <$> lessonPost env r
    | requestMethod r == methodGet -> Just <$> lessonsGet env r
    | otherwise -> pure (Just (bad methodNotAllowed405 "GET, POST"))
  _ -> pure Nothing
  where
    only act
      | requestMethod r == methodGet = Just <$> act
      | otherwise = pure (Just (bad methodNotAllowed405 "GET"))

-- | A query parameter, when given and not empty (bytes that are not UTF-8
-- are decoded leniently and so match nothing).
param :: Request -> Text -> Maybe Text
param r k = case lookup (TE.encodeUtf8 k) (queryString r) of
  Just (Just v) | v /= "" -> Just (TE.decodeUtf8With TEE.lenientDecode v)
  _ -> Nothing

-- | The action over runs.db opened read-only, or the default when there is
-- no runs.db. A database that cannot be read is a 500 naming the error.
withLog :: UiEnv -> a -> (Runlog -> IO a) -> IO (Either Response a)
withLog env none k = do
  let path = runlogPath (ueHome env)
  there <- doesFileExist path
  if not there
    then pure (Right none)
    else do
      r <- try (withRo path (\c -> runlogOn c >>= k))
      case r of
        Right a -> pure (Right a)
        Left e
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | Just q <- fromException e -> pure (Left (failed (sqlErrorDetails q)))
          | otherwise -> pure (Left (failed (T.pack (show e))))
  where
    failed m = bad internalServerError500 ("runs.db: " <> m)

respond :: A.ToJSON a => Either Response a -> Response
respond = either id (jsonResponse ok200)

-- | A @project@ that is not an absolute path.
relativeProject :: Request -> Bool
relativeProject r = maybe False (not . isAbsolute . T.unpack) (param r "project")

notAbsolute :: Response
notAbsolute = bad badRequest400 "project is the absolute path of a project's root"

list :: UiEnv -> Request -> IO Response
list env r
  | relativeProject r = pure notAbsolute
  | otherwise = case (traverse num (param r "since"), traverse kind (param r "kind"), traverse lim (param r "limit")) of
    (Nothing, _, _) -> pure (bad badRequest400 "since is seconds since the epoch")
    (_, Nothing, _) -> pure (bad badRequest400 ("kind is one of " <> T.intercalate ", " (map renderRunKind [minBound .. maxBound])))
    (_, _, Nothing) -> pure (bad badRequest400 "limit is a whole number")
    (Just since, Just k, Just l) -> do
      let key = projectKeyOf . T.unpack <$> param r "project"
          n = maybe 200 (max 1 . min 1000) l
      respond <$> withLog env [] (\rl -> map runRowJson <$> listRuns rl since key k n)
  where
    num t = readMaybe (T.unpack t) :: Maybe Double
    kind = parseRunKind
    lim t = readMaybe (T.unpack t) :: Maybe Int

detail :: UiEnv -> Text -> IO Response
detail env rid = do
  r <- withLog env Nothing $ \rl ->
    getRun rl rid >>= \case
      Nothing -> pure Nothing
      Just row -> do
        gs <- runGoals rl rid
        es <- runEventsUpTo rl rid (eventCap + 1)
        pure . Just $
          A.object
            [ "run" A..= runRowJson row
            , "goals" A..= map goalRowJson gs
            , "events" A..= [A.object ["seq" A..= s, "t" A..= t, "kind" A..= k, "data" A..= d] | (s, t, k, d) <- take eventCap es]
            , "truncated" A..= (length es > eventCap) ]
  pure $ case r of
    Left e -> e
    Right Nothing -> bad notFound404 "no such run"
    Right (Just v) -> jsonResponse ok200 v

-- | The @project@ parameter as a root (no trailing separator).
rootParam :: Request -> Maybe FilePath
rootParam r = canonRoot . T.unpack <$> param r "project"

canonRoot :: FilePath -> FilePath
canonRoot = dropTrailingPathSeparator . normalise

learn :: UiEnv -> Request -> IO Response
learn env r
  | relativeProject r = pure notAbsolute
  | otherwise = do
    let root = rootParam r
        key = projectKeyOf <$> root
    pol <- case key of
      Nothing -> pure (Right Nothing)
      Just k -> readPolicy (policyPath (ueHome env) k)
    let policy = either (const Nothing) id pol
    logged <- withLog env ([], A.object [], []) $ \rl -> do
      projects <- runProjects rl
      case key of
        Nothing -> pure (projects, A.object [], [])
        Just k -> do
          st <- loadStats rl k
          changes <- listPolicyChanges rl k changeCap
          pure (projects, tables st policy, map change changes)
    pure $ case logged of
      Left e -> e
      Right (projects, stats, changes) ->
        jsonResponse ok200 $
          A.object
            [ "project" A..= root
            , "project_key" A..= key
            , "projects" A..= projects
            , "stats" A..= stats
            , "policy" A..= policy
            , "policy_error" A..= either Just (const Nothing) pol
            , "changes" A..= changes ]
  where
    change (at, sh, k, old, new, ev) =
      A.object ["at" A..= at, "shape" A..= sh, "knob" A..= k, "old" A..= old, "new" A..= new, "evidence" A..= ev]

-- | Per shape (@"*"@ first, then the project's shapes and the policy's),
-- per knob, the arms' table.
tables :: Stats -> Maybe Policy -> A.Value
tables st policy =
  A.object
    [ K.fromText sh A..= A.object [K.fromText (renderKnob knob) A..= map arm (table sh knob) | knob <- [minBound .. maxBound]]
    | sh <- shapes ]
  where
    shapes = nub ("*" : tableShapes st <> maybe [] (filter (/= "*") . Map.keys . pShapes) policy)
    table sh knob =
      let lvl = if sh == "*" then LProject else LShape
          ordered = maybe [] (\p -> knobArms knob (policyFor p sh)) policy
       in statsTable st lvl sh knob ordered
    arm a =
      A.object
        [ "arm" A..= arArm a, "attempts" A..= arAttempts a, "closes" A..= arCloses a, "rate" A..= arRate a
        , "cost_per_close" A..= arCostPerClose a, "closes_per_hour" A..= arClosesPerHour a ]

lessonsGet :: UiEnv -> Request -> IO Response
lessonsGet env r
  | relativeProject r = pure notAbsolute
  | otherwise = do
    let root = rootParam r
        key = projectKeyOf <$> root
    logged <- withLog env ([], []) $ \rl -> do
      own <- maybe (pure []) (\k -> if k == globalKey then pure [] else listLessons rl (Just k) []) key
      glob <- listLessons rl (Just globalKey) []
      pure (own, glob)
    lim <- lessonLimits (ueHome env)
    let rowJson = withProposed lim
    pure $ case logged of
      Left e -> e
      Right (own, glob) ->
        jsonResponse ok200 $
          A.object
            [ "project" A..= root
            , "project_key" A..= key
            , "lessons" A..= map rowJson own
            , "global" A..= map rowJson glob ]

-- | A row as 'lessonRowJson' with @proposed@ ('awaitingApproval').
withProposed :: Limits -> LessonRow -> A.Value
withProposed lim l = case lessonRowJson l of
  A.Object o -> A.Object (KM.insert "proposed" (A.Bool (awaitingApproval lim l)) o)
  v -> v

-- | The limits the panel marks proposed rows by: the defaults, with
-- @fisher.lessons.auto_judged@ from the home's config.yaml when there is
-- one (never made here; one that cannot be read is the default).
lessonLimits :: FilePath -> IO Limits
lessonLimits home = do
  there <- doesFileExist (configPathIn home)
  cfg <- if not there then pure A.Null else try (loadConfigAt home) >>= \case
    Right (v, _) -> pure v
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure A.Null
  pure defaultLimits {lAutoJudged = lessonsAutoJudged cfg}

lessonPost :: UiEnv -> Request -> IO Response
lessonPost env r =
  jsonBodyWith 4096 r >>= \case
    Nothing -> pure badBody
    Just o -> case (lessonId o, textField "action" o, KM.lookup "project" o) of
      (Nothing, _, _) -> pure badBody
      (_, Nothing, _) -> pure badBody
      (Just i, Just action, proj)
        | action `notElem` lessonActions ->
            pure (bad badRequest400 ("action is one of " <> T.intercalate ", " lessonActions))
        | otherwise -> case proj of
            Nothing -> act i action Nothing
            Just A.Null -> act i action Nothing
            Just (A.String p) | isAbsolute (T.unpack p) -> act i action (Just (canonRoot (T.unpack p)))
            Just _ -> pure notAbsolute
  where
    badBody = bad badRequest400 "send {id, action, project} as JSON (action one of pin, unpin, drop)"
    -- a whole number (aeson's Int refuses a fraction and one out of range)
    lessonId o = case A.fromJSON <$> KM.lookup "id" o of
      Just (A.Success i) -> Just (i :: Int)
      _ -> Nothing
    path = runlogPath (ueHome env)
    act i action proj = do
      there <- doesFileExist path
      if not there
        then pure noLesson
        else withRunlog path $ \rl -> do
          now <- realToFrac <$> getPOSIXTime
          applyAction rl now action i >>= \case
            Left why -> pure (bad conflict409 why)
            Right Nothing -> pure noLesson
            Right (Just l) -> do
              root <- case proj of
                Just p | lrProjectKey l == globalKey || lrProjectKey l == projectKeyOf p -> pure (Just p)
                _ -> lessonRoot rl l
              regen <- case root of
                Just p -> regenerate rl (ueHome env) p (projectKeyOf p)
                Nothing -> regenerateGlobal rl (ueHome env)
              lim <- lessonLimits (ueHome env)
              pure . jsonResponse ok200 $ case (regen, withProposed lim l) of
                (Left e, A.Object row) -> A.Object (KM.insert "regenerate_error" (A.String e) row)
                (_, row) -> row
    noLesson = bad notFound404 "no such lesson"
