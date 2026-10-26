-- | What the searches need from the index, the facts and the library: one
-- goal's retrieval (its facts and its offer from the library, cut off at
-- 'reCutoff' seconds with what was ready), which offered lemmas a goal
-- used, and the harvest into the library at the end of a run. Nothing
-- here can stop a search: a failure gives an empty retrieval, or a
-- 'Left' for the caller to log.
module Fisher.Search.Retrieval
  ( RetrievalEnv (..)
  , retrievalCutoff
  , Query (..)
  , Retrieved (..)
  , newRetriever
  , joinBrief
  , smtFacts
  , GoalRetrieval (..)
  , RetrievalReport (..)
  , usedOf
  , helperStatements
  , goalLine
  , saveRun
  , once
  , Terminated (..)
  , harvestOnException
  , interruptHarvestSecs
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (Exception (..), SomeAsyncException, SomeException, asyncExceptionFromException, asyncExceptionToException, evaluate, fromException, mask_, onException, throwIO, try)
import Control.Monad (forM_)
import Data.Char (isAlphaNum)
import Data.IORef
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (normalise)
import System.Timeout (timeout)

import Fisher.Search.Facts
import Fisher.Search.Index
import Fisher.Search.Lean (Decl (..))
import Fisher.Search.Library

data RetrievalEnv = RetrievalEnv
  { reIndex :: Index
  -- ^ the project's index, built once for the run
  , reFacts :: Maybe FactsConfig
  -- ^ Nothing: no facts (@--no-retrieval@)
  , reLibrary :: Maybe Library
  -- ^ the library as loaded at the start; Nothing: no library
  -- (@--no-library@), so nothing offered and nothing saved
  , reHarvest :: Bool
  -- ^ save the run's lemmas and notes to the library at the end (False:
  -- @--library-read-only@, offer only)
  , reLibDir :: FilePath
  -- ^ where the library is saved
  , reRoot :: FilePath
  -- ^ the project's package root (absolute; the library's key)
  , reRun :: FilePath
  -- ^ the run directory, kept with each harvested lemma and note
  , reCutoff :: Double
  -- ^ seconds one goal's retrieval may take ('retrievalCutoff')
  , reExtra :: Text
  -- ^ project-wide text every brief ends with (the smt facts,
  -- 'smtFacts'); empty for none
  }

-- | A goal's retrieval is cut off after this many seconds.
retrievalCutoff :: Double
retrievalCutoff = 10

-- | One goal (or graph node) to retrieve for.
data Query = Query
  { qGoal :: Text
  -- ^ the goal or node
  , qAnchor :: Text
  -- ^ the declaration of the target file on disk whose keyword line is
  -- the goal's line for visibility: the goal itself, or a graph node's root
  , qLine :: Int
  -- ^ the 1-based line used when the index does not have the anchor in
  -- the target file
  , qStatement :: Text
  , qType :: Maybe Text
  -- ^ its @pp.all@ type, when printed
  , qOpen :: [Text]
  -- ^ the open goals' statements, the goal's own included
  }

data Retrieved = Retrieved
  { rtFacts :: Int
  , rtFactsChars :: Int
  , rtOffered :: [Text]
  -- ^ the names of the lemmas the brief shows
  , rtLemmas :: [(Text, Text)]
  -- ^ those lemmas' names and statements
  , rtNotes :: Int
  , rtBrief :: Text
  -- ^ the facts section, the library section, then 'reExtra' (any may be
  -- empty)
  , rtCutOff :: Bool
  }
  deriving (Eq, Show)

-- | A function giving each goal of the target file (relative to the
-- package root) its retrieval. Call it once per run: the facts' BM25
-- corpus over the index, the index's tables and the library's corpora
-- are made (lazily, under the first goal's cut-off) by the closures bound
-- here, outside the per-goal function, so every goal shares them.
newRetriever :: RetrievalEnv -> FilePath -> IO (Query -> IO Retrieved)
newRetriever env file = do
  let ix = reIndex env
      factsOf = fmap (`factsFor` ix) (reFacts env)
      offerOf = fmap (offerer ix) (reLibrary env)
  pure (retrieveOne env ix file factsOf offerOf)

retrieveOne
  :: RetrievalEnv -> Index -> FilePath
  -> Maybe (Maybe Text -> FilePath -> Int -> Text -> Maybe Text -> [Fact])
  -> Maybe (FilePath -> [Text] -> Text -> [Text] -> Library)
  -> Query -> IO Retrieved
retrieveOne env ix file factsOf offerOf q = do
  factsRef <- newIORef []
  libRef <- newIORef ([], [])
  let body = do
        let line = goalLine ix file (qAnchor q) (qLine q)
        forM_ factsOf $ \f -> do
          let fs = f Nothing file line (qStatement q) (qType q)
          _ <- evaluate (T.length (factsBrief fs))
          writeIORef factsRef fs
        forM_ offerOf $ \pick -> do
          let consts = maybe [] ppConstants (qType q) <> ownUses ix file (qGoal q)
              o = pick file consts (qStatement q) (qOpen q)
          _ <- evaluate (T.length (libraryBrief o) + sum (map (T.length . lmName) (fst o)) + length (snd o))
          writeIORef libRef o
  r <- try (timeout (max 1 (round (reCutoff env * 1000000))) body)
  cut <- case r of
    Left e
      | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
      | otherwise -> pure False
    Right Nothing -> pure True
    Right (Just ()) -> pure False
  fs <- readIORef factsRef
  o <- readIORef libRef
  let fb = factsBrief fs
      lb = libraryBrief o
      shownL = [l | l <- fst o, ("-- " <> lmName l <> " (") `T.isInfixOf` lb]
  pure Retrieved
    { rtFacts = length fs
    , rtFactsChars = T.length fb
    , rtOffered = map lmName shownL
    , rtLemmas = [(lmName l, lmStatement l) | l <- shownL]
    , rtNotes = length (snd o)
    , rtBrief = joinBrief [fb, lb, reExtra env]
    , rtCutOff = cut }

-- | The brief's paragraph on lean-smt, given whether the project has it
-- and the standalone solver found on PATH (@z3@ or @cvc5@), if any; empty
-- without lean-smt.
smtFacts :: Bool -> Maybe Text -> Text
smtFacts hasLeanSmt solver
  | not hasLeanSmt = ""
  | otherwise =
      "`smt` (lean-smt, cvc5) is available: `import Smt`, `by smt`, `by smt [h\8321, h\8322]` to pass hypotheses, `smt +mono` for polymorphic goals; it covers linear integer and real arithmetic, bitvectors, uninterpreted functions and quantifier-free logic over them."
        <> maybe "" (\n -> " `" <> n <> "` is on PATH for SMT-LIB queries.") solver

-- | Brief parts in order, blank lines between, empty ones left out.
joinBrief :: [Text] -> Text
joinBrief = T.intercalate "\n\n" . map T.stripEnd . filter (not . T.null . T.strip)

-- | The 1-based keyword line of a declaration of the target file as the
-- index has it (the file on disk), else the fallback.
goalLine :: Index -> FilePath -> Text -> Int -> Int
goalLine ix file name fallback =
  maybe fallback eLine (listToMaybe [e | e <- ixEntries ix, eName e == name, normalise (eFile e) == normalise file])

-- | The project constants the index says a declaration of the target
-- file mentions.
ownUses :: Index -> FilePath -> Text -> [Text]
ownUses ix file name =
  concat [eUses e | e <- ixEntries ix, eName e == name, normalise (eFile e) == normalise file]

-- | What one goal or node got, and which of the offered lemmas it used.
data GoalRetrieval = GoalRetrieval
  { rgFacts :: Int
  , rgOffered :: [Text]
  , rgUsed :: [Text]
  }
  deriving (Eq, Show)

-- | The retrieval of a run: per goal (or node), and what the run saved
-- to the library (lemmas, notes).
data RetrievalReport = RetrievalReport
  { rrGoals :: [(Text, GoalRetrieval)]
  , rrSaved :: (Int, Int)
  }
  deriving (Eq, Show)

-- | The offered lemmas (name, statement) a goal used, given its merged
-- proof and the helpers (name, statement) merged with it: one named in
-- the proof by its full name (as an identifier), or merged as a helper
-- under its full name, or under any other name with the lemma's statement
-- ('normalStatement'), since a pasted helper may sit in another namespace
-- or be renamed. A short name alone never counts.
usedOf :: [(Text, Text)] -> Text -> [(Text, Text)] -> [Text]
usedOf offered proof helpers = [n | (n, st) <- offered, byHelper n st || inProof n]
  where
    byHelper n st = any (\(h, hs) -> h == n || normalStatement hs == normalStatement st) helpers
    idents = Set.fromList (T.split (\c -> not (isAlphaNum c || c `elem` ("_.'!?" :: String))) proof)
    inProof n = Set.member n idents

-- | Each merged helper's statement in the final text, for 'usedOf': the
-- declaration of that name, else the one whose name ends in @.<name>@
-- (the check may name a helper without its namespace); empty when none,
-- or when more than one ends so.
helperStatements :: [Decl] -> [Text] -> [(Text, Text)]
helperStatements decls hs = [(h, maybe "" dStatement (pick h)) | h <- hs]
  where
    pick h = case [d | d <- decls, dName d == h] of
      d : _ -> Just d
      [] -> case [d | d <- decls, ("." <> h) `T.isSuffixOf` dName d] of
        [d] -> Just d
        _ -> Nothing

-- | The harvest at the end of a run, saved to the library: the new
-- sorry-free declarations of the merged text and the notes, (goal, text).
-- Nothing with the library off or read-only; Left with why when the save
-- failed.
saveRun :: RetrievalEnv -> FilePath -> Text -> Text -> [(Text, Text)] -> IO (Maybe (Either Text (Int, Int)))
saveRun env file before after notes = case reLibrary env of
  Just _ | reHarvest env -> do
    r <- try $ do
      let (ls, _) = harvest (reIndex env) file before after (reRun env)
          ns = harvestNotes file (reRun env) notes
      saveLibrary (reLibDir env) (reRoot env) ls ns
    case r of
      Left e
        | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
        | otherwise -> pure (Just (Left ("library not saved: " <> T.take 300 (T.pack (show (e :: SomeException))))))
      Right x -> pure (Just (Right x))
  _ -> pure Nothing

-- | An action that runs at most once to its end: later calls give the
-- first result. One cut off by an exception has not run, and the next
-- call runs it again (so an interrupt during the end-of-run harvest leaves
-- it to the interrupt's handler).
once :: IO a -> IO (IO a)
once act = do
  v <- newMVar Nothing
  pure $ modifyMVar v $ \case
    Just r -> pure (Just r, r)
    Nothing -> act >>= \r -> pure (Just r, r)

-- | The harvest may take this long when a run ends in an exception.
interruptHarvestSecs :: Int
interruptHarvestSecs = 20

-- | Run a search; when it ends in an exception (Ctrl-C, the SIGTERM the
-- CLI turns into one, a crash), first the harvest, masked and cut off
-- after 'interruptHarvestSecs', then the cleanup (at most 5 s), then the
-- exception goes on. Failures of either are ignored.
harvestOnException :: IO () -> IO () -> IO a -> IO a
harvestOnException harvestNow cleanup act =
  act `onException` mask_ (do
    _ <- timeout (interruptHarvestSecs * 1000000) (try harvestNow :: IO (Either SomeException ()))
    _ <- timeout 5000000 (try cleanup :: IO (Either SomeException ()))
    pure ())

-- | What SIGTERM throws to the main thread (the CLI turns it into exit
-- code 143 at the top). An asynchronous exception, like Ctrl-C's, so code
-- that catches failures and keeps going (a retrieval, an Auto step, a
-- save) rethrows it and the run reaches its harvest.
data Terminated = Terminated
  deriving (Eq, Show)

instance Exception Terminated where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

