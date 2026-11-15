-- | The lessons block a backend's startup prompt carries, built from what
-- @runs.db@ says is shown, not from the files: the numbered lines are the
-- shown rows ('shownRows': the project's active and pinned lessons and the
-- global @*@ ones), and only the @## Yours@ lines are read from the two
-- lessons files. The files stay the human-readable view, regenerated from
-- the same rows, but a numbered line written into one by hand or by any
-- backend's own tools never reaches a prompt. Every line still passes
-- 'sanitiseLesson' ('blockFrom').
--
-- A file whose first line is not the header is the user's alone
-- ("foreign"): its lines are Yours lines, and the rows that file would
-- show are left out of the block, as they were when the block was read
-- from the files.
--
-- At launch runs.db is opened read-only ('withRunlogRo'): nothing is
-- migrated, locked for writing or moved aside. So the shown rows must not
-- depend on what a writable open would have changed: with
-- @fisher.lessons.auto_judged@ off an active (not pinned) @judged@ or
-- @self@ row is left out here as the mining pass would demote it, and no
-- comparison depends on a judged key's prefix.
module Fisher.Lessons.Prompt
  ( PromptLessons (..)
  , globalKey
  , shownRows
  , shownRowsWith
  , promptLessons
  , promptLessonsAt
  ) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)

import Fisher.Lessons.File (LessonsFile (..), blockFrom, globalLessonsPath, projectLessonsPath, readLessonsFile, renderLessonsBlock)
import Fisher.Role (PromptPart)
import Fisher.Runlog

-- | The block, the ids of the rows whose lines it kept (after the
-- sanitiser and the caps), project ones first, and why runs.db could not
-- be read when it could not (the block then has the Yours lines only).
data PromptLessons = PromptLessons
  { plBlock :: PromptPart
  , plIds :: [Int]
  , plWarning :: Maybe Text
  }
  deriving (Eq, Show)

-- | The project key of the global lessons.
globalKey :: Text
globalKey = "*"

-- | The rows a file of a project key shows: for @*@ its active and pinned
-- rows; for a project's, its active and pinned rows less the active ones
-- whose key has a @*@ row that is not a candidate (that key is the
-- global row's), oldest first.
shownRows :: Runlog -> Text -> IO [LessonRow]
shownRows = shownRowsWith (const True)

-- | 'shownRows' of the rows that pass a gate: a row the gate refuses is
-- not shown, and a @*@ row it refuses blocks no project row (a hidden
-- judged @*@ row on a bare key never hides the project's rule lesson of
-- that key).
shownRowsWith :: (LessonRow -> Bool) -> Runlog -> Text -> IO [LessonRow]
shownRowsWith gate rl pk
  | pk == globalKey = filter gate <$> listLessons rl (Just globalKey) [Active, Pinned]
  | otherwise = do
      stars <- filter gate <$> listLessons rl (Just globalKey) []
      let blocked = [lrKey s | s <- stars, lrStatus s /= Candidate]
      rows <- filter gate <$> listLessons rl (Just pk) [Active, Pinned]
      pure [r | r <- rows, lrStatus r == Pinned || lrKey r `notElem` blocked]

-- | The block for a run in the project at a root, under a Fisher home,
-- from an open run log, with @auto_judged@ on or off.
promptLessons :: Runlog -> Bool -> FilePath -> FilePath -> IO PromptLessons
promptLessons rl autoJudged home root = do
  let pk = projectKeyOf root
      gate r = autoJudged || lrStatus r == Pinned || lrKind r `notElem` ["judged", "self"]
  own <- if pk == globalKey then pure [] else shownRowsWith gate rl pk
  glob <- shownRowsWith gate rl globalKey
  fromRows own glob Nothing home root

-- | 'promptLessons' over the home's run log opened read-only. With no run
-- log the block has the Yours lines of the files only; with one that
-- cannot be opened or read, the same, and 'plWarning' says why. Never
-- makes, migrates or moves a run log.
promptLessonsAt :: Bool -> FilePath -> FilePath -> IO PromptLessons
promptLessonsAt autoJudged home root = do
  let path = runlogPath home
  there <- doesFileExist path
  if not there
    then fromRows [] [] Nothing home root
    else try (withRunlogRo path (\rl -> promptLessons rl autoJudged home root)) >>= \case
      Right pl -> pure pl
      Left (e :: SomeException)
        | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        | otherwise -> fromRows [] [] (Just (T.pack (displayException e))) home root

fromRows :: [LessonRow] -> [LessonRow] -> Maybe Text -> FilePath -> FilePath -> IO PromptLessons
fromRows own glob warning home root = do
  pf <- readLessonsFile (projectLessonsPath root)
  gf <- readLessonsFile (globalLessonsPath home)
  let rowsOf f rs = if maybe False lfForeign f then [] else [(lrId r, lrText r) | r <- rs]
      (p, g, y) = blockFrom (rowsOf pf own) (rowsOf gf glob) (concatMap lfYours [f | Just f <- [pf, gf]])
  pure PromptLessons
    { plBlock = renderLessonsBlock (map snd p, map snd g, y)
    , plIds = nub (map fst p <> map fst g)
    , plWarning = warning }
