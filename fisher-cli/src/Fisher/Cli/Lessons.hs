-- | @fisher lessons list|show|pin|unpin|drop|mine|reset@: the lessons
-- mined from the run log ("Fisher.Learn.Mine", "Fisher.Learn.Lessons"),
-- for a project: @--project P@, else the Lake project the working
-- directory is in (the working directory itself when none), as
-- @fisher learn@ finds it.
--
-- * @list [--all]@: the project's active and pinned lessons (with
--   @--all@ the candidates too), then, when there are any, the proposed
--   ones (judged and self candidates with the counts that would promote a
--   rule, 'awaitingApproval': they wait for a pin, and are listed only
--   there, each with its whole text, wrapped, since that is what a pin
--   approves), then the global (@*@) ones; per row the id, status, evidence
--   runs, agreements (judged and self lessons only) and the text cut to
--   100 characters.
-- * @show ID@: one lesson in full, with its evidence runs and counters.
-- * @pin ID@: pinned (shown, never retired), the approval a judged or
--   self lesson needs (said so on a line of its own when it was proposed). A candidate is so activated
--   (now), an active one keeps its activation, a retired one is activated
--   afresh (now, its runs in force and while-active counters cleared, so
--   the measurement starts over); a dropped one is refused (the drop
--   stands). @unpin ID@: a pinned lesson back to active (a judged or
--   self one back to candidate: proposed again until it is pinned, said so). @drop ID@:
--   dropped, never proposed again. Each regenerates the lessons files. The
--   cap on a file's lines applies at the next mining pass.
-- * @mine [--since D] [--again]@: the mining pass over the project's runs
--   since a date or age (all of them without), with the judge when
--   @fisher.lessons.judge@ is on; prints the pass's counts and the day's
--   judge spend.
-- * @reset@: every lesson of the project is deleted, whatever its status,
--   so its keys can be proposed again, and the files are regenerated (only
--   the @## Yours@ lines are left in the project's). Attempts already mined
--   are mined again only by @mine --again@.
--
-- Reading never makes a run log that is not there; an empty one prints
-- the import hint.
module Fisher.Cli.Lessons
  ( LessonsCmd (..)
  , LessonsEnv (..)
  , lessonsParser
  , runLessons
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Numeric (showFFloat)
import System.Directory (doesFileExist)

import Options.Applicative

import Fisher.Cli.Learn (projectRootFor)
import Fisher.Cli.Mine (judgeCaps, lessonsConfigOn, lessonsJudgeOn, lessonsLimits, mineEnvFor)
import Fisher.Cli.Runs (emptyHint, parseSince)
import Fisher.Learn.Lessons (applyAction, awaitingApproval, globalKey, lessonRoot, regenerate)
import Fisher.Learn.Mine (MineEnv (..), MineReport (..), mineSince)
import Fisher.Lessons.File (projectLessonsPath)
import Fisher.Runlog
import Fisher.Search.GraphJobs (Ask)

data LessonsCmd
  = LsList Bool (Maybe FilePath)
  -- ^ with the candidates; project
  | LsShow Int
  | LsPin Int
  | LsUnpin Int
  | LsDrop Int
  | LsMine (Maybe Text) (Maybe FilePath) Bool
  -- ^ since; project; again
  | LsReset (Maybe FilePath)
  deriving (Eq, Show)

-- | The run log, the Fisher home (the global file, the templates), the
-- working directory, the configuration (the lessons' keys), the clock,
-- how to print, and how to build the judge (asked only by @mine@ with the
-- judge on; Nothing for rules only).
data LessonsEnv = LessonsEnv
  { lsRunlogPath :: FilePath
  , lsFisherHome :: FilePath
  , lsCwd :: FilePath
  , lsConfig :: Value
  , lsNow :: IO Double
  , lsPut :: Text -> IO ()
  , lsJudge :: IO (Maybe Ask)
  }

runLessons :: LessonsEnv -> LessonsCmd -> IO ()
runLessons env = \case
  LsList withCandidates project -> do
    root <- projectRootFor (lsCwd env) project
    let key = projectKeyOf root
    logged $ \rl -> do
      let statuses = [Active, Pinned, Candidate]
          wanted l = (withCandidates || lrStatus l /= Candidate) && not (proposed l)
      ownAll <- listLessons rl (Just key) statuses
      globAll <- if key == globalKey then pure [] else listLessons rl (Just globalKey) statuses
      let own = filter wanted ownAll
          glob = filter wanted globAll
          waiting = filter proposed (ownAll <> globAll)
      put ("project " <> T.pack root <> " (" <> key <> ")")
      table own
      put ""
      case waiting of
        [] -> pure ()
        _ -> do
          put "proposed (judged and self lessons, never shown until you approve one)"
          put tableHeader
          mapM_ (mapM_ put . proposedLines) waiting
          put "  read one in full with: fisher lessons show ID, then approve it with: fisher lessons pin ID (or: fisher lessons drop ID)"
          put ""
      put "global (*)"
      table glob
  LsShow i -> logged $ \rl ->
    getLesson rl i >>= \case
      Nothing -> put (noLesson i)
      Just l -> mapM_ put (showLines l)
  LsPin i -> change i "pin"
  LsUnpin i -> change i "unpin"
  LsDrop i -> change i "drop"
  LsMine since project again
    | not (lessonsConfigOn cfg) -> put "lessons are off: fisher.lessons.enabled is false"
    | otherwise -> do
        root <- projectRootFor (lsCwd env) project
        now <- lsNow env
        case traverse (parseSince now) since of
          Left e -> put e
          Right since' -> logged $ \rl -> do
            judge <- if lessonsJudgeOn cfg then lsJudge env else pure Nothing
            menv <- mineEnvFor warn cfg (lsFisherHome env) root judge rl
            put ("project " <> T.pack root <> " (" <> projectKeyOf root <> ")" <> maybe ", rules only" (const "") judge)
            rep <- mineSince menv {meNow = now} since' again
            put ("mined " <> tshow (mrMined rep) <> ", judged " <> tshow (mrJudged rep) <> ", unjudged " <> tshow (mrUnjudged rep))
            put ("promoted " <> ids (mrPromoted rep) <> ", retired " <> ids (mrRetired rep))
            let day = utcDay now
                (_, dayCap) = judgeCaps cfg
            spent <- spendOn rl day
            put ("judge spend " <> day <> ": " <> dollars spent <> " of " <> dollars dayCap)
  LsReset project -> do
    root <- projectRootFor (lsCwd env) project
    let key = projectKeyOf root
    logged $ \rl -> do
      n <- deleteLessons rl key
      put ("deleted " <> tshow n <> " lessons of " <> T.pack root <> " (" <> key <> ")")
      files rl root
      put "attempts already mined are mined again only with: fisher lessons mine --again"
  where
    put = lsPut env
    cfg = lsConfig env
    proposed = awaitingApproval (lessonsLimits cfg)
    warn m = put ("fisher lessons: " <> m)
    noLesson i = "no lesson " <> tshow i

    -- the log when there is one and it has runs (never made here), else
    -- the import hint; one that cannot be opened is one line
    logged :: (Runlog -> IO ()) -> IO ()
    logged k = do
      there <- doesFileExist (lsRunlogPath env)
      if not there
        then put emptyHint
        else
          openRunlog (lsRunlogPath env) >>= \case
            Left e -> put ("fisher lessons: " <> e)
            Right rl -> flip finally (closeRunlog rl) $ do
              anyRun <- listRuns rl Nothing Nothing Nothing 1
              if null anyRun then put emptyHint else k rl

    -- a status change by id ('applyAction'): the old and new status, or
    -- why the command does not apply to the row (said, nothing written);
    -- then the files regenerated
    change :: Int -> Text -> IO ()
    change i action = logged $ \rl ->
      getLesson rl i >>= \case
        Nothing -> put (noLesson i)
        Just l -> do
          now <- lsNow env
          applyAction rl now action i >>= \case
            Left why -> put ("lesson " <> tshow i <> ": " <> why <> ", nothing changed")
            Right Nothing -> put (noLesson i)
            Right (Just l') -> do
              put ("lesson " <> tshow i <> ": " <> renderLessonStatus (lrStatus l) <> " -> " <> renderLessonStatus (lrStatus l'))
              when (action == "unpin" && lrStatus l' == Candidate) $
                put ("lesson " <> tshow i <> ": unpinned; it is proposed again until you pin it")
              when (action == "pin" && proposed l) $
                put ("lesson " <> tshow i <> ": approved; a " <> lrKind l <> " lesson is shown to the agents only once a human pins it")
              cwdRoot <- projectRootFor (lsCwd env) Nothing
              root <-
                if lrProjectKey l == globalKey || lrProjectKey l == projectKeyOf cwdRoot
                  then pure (Just cwdRoot)
                  else lessonRoot rl l
              case root of
                Just r -> files rl r
                Nothing -> do
                  -- the global file still follows; the project's at its next pass
                  files rl cwdRoot
                  put ("the file of project " <> lrProjectKey l <> " is regenerated at its next mining pass")

    files :: Runlog -> FilePath -> IO ()
    files rl root =
      regenerate rl (lsFisherHome env) root (projectKeyOf root) >>= \case
        Right () -> put ("regenerated " <> T.pack (projectLessonsPath root) <> " and the global file")
        Left e -> warn e

    table :: [LessonRow] -> IO ()
    table = \case
      [] -> put "  (none)"
      rows -> put tableHeader >> mapM_ (put . lessonLine) rows

tableHeader :: Text
tableHeader = row ["id", "status", "runs", "agree", "text"]

row :: [Text] -> Text
row cols = T.stripEnd ("  " <> T.intercalate "  " (zipWith (\w c -> T.justifyLeft w ' ' c) [5, 9, 4, 5, 0] cols))

-- | A list row: id, status, evidence runs, agreements (judged and self
-- only), the text on one line cut to 100 characters.
lessonLine :: LessonRow -> Text
lessonLine l =
  row
    [ tshow (lrId l), renderLessonStatus (lrStatus l), tshow (length (lrEvidenceRuns l))
    , if judgeKind l then tshow (lrAgreements l) else "-"
    , T.take 100 (oneLine (lrText l)) ]

-- | A proposed row: its list row with the whole text, wrapped at 100
-- characters under the text column.
proposedLines :: LessonRow -> [Text]
proposedLines l = case wrap 100 (oneLine (lrText l)) of
  [] -> [lessonLine l]
  first : rest ->
    row [tshow (lrId l), renderLessonStatus (lrStatus l), tshow (length (lrEvidenceRuns l)), tshow (lrAgreements l), first]
      : map (T.replicate 33 " " <>) rest

-- Words into lines of at most a width (a longer word on a line of its own).
wrap :: Int -> Text -> [Text]
wrap w = go [] . T.words
  where
    go acc [] = [T.unwords (reverse acc) | not (null acc)]
    go [] (x : xs) = go [x] xs
    go acc (x : xs)
      | T.length (T.unwords (reverse (x : acc))) <= w = go (x : acc) xs
      | otherwise = T.unwords (reverse acc) : go [x] xs

judgeKind :: LessonRow -> Bool
judgeKind l = lrKind l `elem` ["judged", "self"]

showLines :: LessonRow -> [Text]
showLines l =
  [ "lesson " <> tshow (lrId l) <> " (" <> renderLessonStatus (lrStatus l) <> ")"
  , "  " <> lrText l
  , "  kind " <> lrKind l <> ", key " <> lrKey l
  , "  project " <> (if lrProjectKey l == globalKey then "* (global)" else lrProjectKey l)
  , "  evidence runs (" <> tshow (length (lrEvidenceRuns l)) <> "): " <> listed (lrEvidenceRuns l) ]
    <> ["  agreements " <> tshow (lrAgreements l) | judgeKind l]
    <> [ "  first seen " <> date (lrFirstSeen l) <> ", activated " <> maybe "-" date (lrActivatedAt l)
           <> ", retired " <> maybe "-" date (lrRetiredAt l)
       , "  in force: " <> tshow (length (lrRunsActive l)) <> " runs, " <> tshow (lrHitsWhileActive l) <> " hits in "
           <> tshow (lrAttemptsWhileActive l) <> " attempts"
       , "  before: " <> tshow (lrRunsBefore l) <> " runs, " <> tshow (lrHitsBefore l) <> " hits in "
           <> tshow (lrAttemptsBefore l) <> " attempts" ]

oneLine :: Text -> Text
oneLine = T.unwords . T.words

listed :: [Text] -> Text
listed = \case
  [] -> "(none)"
  xs -> T.intercalate ", " xs

ids :: [Int] -> Text
ids = \case
  [] -> "(none)"
  xs -> T.intercalate ", " (map tshow xs)

dollars :: Double -> Text
dollars x = "$" <> T.pack (showFFloat (Just 2) x "")

date :: Double -> Text
date t = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (posixSecondsToUTCTime (realToFrac t)))

utcDay :: Double -> Text
utcDay t = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (posixSecondsToUTCTime (realToFrac t)))

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @fisher lessons@'s arguments.
lessonsParser :: Parser LessonsCmd
lessonsParser = hsubparser
  ( command "list"
      (info (LsList <$> switch (long "all" <> help "Also the candidates") <*> project)
            (progDesc "The project's active and pinned lessons, then the global ones"))
 <> command "show" (info (LsShow <$> lessonId) (progDesc "One lesson: its text, status, evidence and counters"))
 <> command "pin" (info (LsPin <$> lessonId) (progDesc "Pin a lesson: shown and never retired (a candidate or retired one is activated; the file's cap applies at the next mining pass)"))
 <> command "unpin" (info (LsUnpin <$> lessonId) (progDesc "Unpin a lesson: back to active (a judged or self one is proposed again)"))
 <> command "drop" (info (LsDrop <$> lessonId) (progDesc "Drop a lesson: never shown or proposed again"))
 <> command "mine"
      (info (LsMine <$> optional (T.pack <$> strOption (long "since" <> metavar "D" <> help "Only runs since a date (2026-10-01) or an age (12h, 7d, 2w)"))
                    <*> project
                    <*> switch (long "again" <> help "Run the rules again over attempts already mined"))
            (progDesc "Mine the project's logged runs for mistakes and lessons"))
 <> command "reset" (info (LsReset <$> project) (progDesc "Delete every lesson of the project and regenerate its file (re-mine old attempts with mine --again)")) )
  where
    project = optional (strOption (long "project" <> metavar "P" <> help "The project's root (default: the Lake project of the working directory)"))
    lessonId = argument auto (metavar "ID" <> help "The lesson's id (from list)")
