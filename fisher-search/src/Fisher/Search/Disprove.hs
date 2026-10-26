-- | Disproving a goal: two sources look for a counterexample at the
-- goal's hole through the 'Lsp'. Mathlib's @plausible@ samples values;
-- lean-smt's @smt_show@ prints the goal's SMT-LIB query (which asserts
-- the goal's negation) and a standalone solver ('Fisher.Smt.runQuery')
-- looks for a model of it. A counterexample is then certified, when it
-- can be, by a kernel-checked
-- @theorem _fisher_neg_\<k\> : not (type_of% \@NAME) := by \<tactic\>@ placed
-- after the goal's declaration, its axioms printed and held to the axiom
-- policy; a solver's model first gets a witness tactic built from its
-- values ('witnessTactic'). Only a certified negation is 'DFalse'; a
-- counterexample or a model alone is a 'DSuspect' (a hint, never a
-- verdict).
module Fisher.Search.Disprove
  ( Disproof (..)
  , DisproveSource (..)
  , sourceName
  , DisproveInfo (..)
  , SmtEnv (..)
  , parsePlausible
  , negationTheorem
  , certifyTactics
  , queryTactic
  , parseSmtQuery
  , boundVariables
  , witnessTactic
  , declaredConsts
  , leanName
  , modelText
  , disprove
  , disproveWitnessOnly
  , disproveAfterSuspect
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Fisher.Search.Auto
import Fisher.Search.Check (AxiomPolicy, LeanRun, Probe (..), axiomAllowed, probe)
import Fisher.Search.Lean (Decl (..), declarations, findDecl)
import Fisher.Smt (Model (..), SmtResult (..), Solver, Value (..), prepareQuery)

data Disproof
  = DFalse Text
  -- ^ the tactic that proved the negation
  | DSuspect Text
  -- ^ the counterexample text, not certified
  | DUnknown
  deriving (Eq, Show)

-- | Where a counterexample comes from.
data DisproveSource = ViaPlausible | ViaSmt
  deriving (Eq, Show)

-- | The source as the events name it: @plausible@ or @smt@.
sourceName :: DisproveSource -> Text
sourceName ViaPlausible = "plausible"
sourceName ViaSmt = "smt"

-- | What a 'disprove' call did, for the caller's events.
data DisproveInfo = DisproveInfo
  { diSteps :: [(DisproveSource, Text)]
  -- ^ each source that ran, in order, with its result: @counterexample@
  -- or @none@ for plausible; @unsat@, @sat@, @unknown@, @error@ or @none@
  -- (no query) for smt
  , diWitness :: Maybe Text
  -- ^ the model text, when the witness tactic built from it certified
  -- the negation
  }
  deriving (Eq, Show)

-- | The SMT source's settings: the solver, its cap in seconds, the query
-- tactic (@fisher.smt.query_tactic@) and the runner ('Fisher.Smt.runQuery'
-- in production).
data SmtEnv = SmtEnv
  { smSolver :: Solver
  , smTimeout :: Int
  , smQueryTactic :: Text
  , smRun :: Solver -> Int -> Text -> IO SmtResult
  }

-- | What @plausible@ said: 'DSuspect' with the lines after its
-- @Found a counter-example!@ heading, else 'DUnknown' (no counterexample,
-- or the generators are missing). Never 'DFalse'.
parsePlausible :: Trial -> Disproof
parsePlausible tr =
  case [ T.strip (T.intercalate "\n" (drop 1 (T.lines hit)))
       | Diag _ _ _ m <- tDiags tr
       , let (_, hit) = T.breakOn marker m
       , not (T.null hit) ] of
    (c : _) -> DSuspect c
    [] -> DUnknown
  where
    marker = "Found a counter-example!"

-- | The negation of a goal's statement as theorem @_fisher_neg_\<k\>@, to
-- be proved by the tactic.
negationTheorem :: Int -> Text -> Text -> Text
negationTheorem k name tactic =
  "theorem _fisher_neg_" <> T.pack (show k) <> " : ¬ (type_of% @" <> name <> ") := by " <> wrapSnippet 200000 tactic

certifyTactics :: [Text]
certifyTactics = ["decide", "simp_all", "norm_num", "omega"]

-- | The tactic that prints the goal's query, by @fisher.smt.query_tactic@:
-- @trace@ is the trace fallback, anything else @smt_show@.
queryTactic :: Text -> Text
queryTactic "trace" = "set_option trace.smt.translate.query true in smt"
queryTactic _ = "smt_show"

-- | The SMT-LIB query in a trial's messages, by the query tactic's name:
-- for @smt_show@ the body of its info message (after its @query:@
-- heading, when there is one); for @trace@ the text after the
-- @[smt.translate.query]@ marker. Nothing when no message holds a query.
parseSmtQuery :: Text -> Trial -> Maybe Text
parseSmtQuery tac tr = case filter looksLikeQuery (concatMap fromDiag (tDiags tr)) of
  (q : _) -> Just q
  [] -> Nothing
  where
    fromDiag (Diag _ _ sev m)
      | tac == "trace" =
          let (_, hit) = T.breakOn traceMarker m
           in [T.strip (T.drop (T.length traceMarker) hit) | not (T.null hit)]
      | sev == SevInfo =
          let (_, hit) = T.breakOn "query:" m
           in if T.null hit then [T.strip m] else [T.strip (T.drop (T.length "query:") hit)]
      | otherwise = []
    traceMarker = "[smt.translate.query]"
    looksLikeQuery q = "(" `T.isPrefixOf` q

-- | A goal's leading bound variables with their types as written: the
-- @(x : T)@ and @{x : T}@ binders of a declaration's statement before its
-- colon, then the leading @∀ x y : T,@ (or @∀ (x : T) (y : U),@) binders
-- of its type, before the first @→@ or the @,@ that ends them. A
-- statement without the keyword is read as a type. A binder without a
-- type has an empty one; an instance binder or anything else unexpected
-- ends the list.
boundVariables :: Text -> [(Text, Text)]
boundVariables stmt0 = case keyword (T.strip stmt0) of
  Just rest -> beforeColon (dropName rest)
  Nothing -> foralls (stripAssign (T.strip stmt0))
  where
    keyword s =
      let s1 = foldr (\m acc -> maybe acc T.stripStart (T.stripPrefix m acc)) s ["private ", "protected "]
       in case T.words s1 of
            (k : _) | k `elem` ["theorem", "lemma"] -> Just (T.stripStart (T.drop (T.length k) s1))
            _ -> Nothing
    dropName = T.stripStart . T.dropWhile (\c -> not (isSpace c || c `elem` ("({[:" :: String)))
    beforeColon s = case T.uncons s of
      Just (c, _)
        | c `elem` ("({" :: String) -> case group s of
            Just (bs, rest) -> bs <> beforeColon (T.stripStart rest)
            Nothing -> []
        | c == ':' -> foralls (stripAssign (T.strip (T.drop 1 s)))
      _ -> []
    foralls s = case T.stripPrefix "∀" s <|> T.stripPrefix "forall " s of
      Nothing -> []
      Just r -> case topSplit (== ',') r of
        Nothing -> []
        Just (bs, body) -> case forallGroup (T.strip bs) of
          Just vs -> vs <> foralls (T.strip body)
          Nothing -> []
    -- @x y : T@, @x y@, or a run of parenthesised groups
    forallGroup b = case T.uncons b of
      Just (c, _) | c `elem` ("({" :: String) -> groups b
      _ -> namesTyped b
    groups b
      | T.null b = Just []
      | otherwise = do
          (vs, rest) <- group b
          (vs <>) <$> groups (T.strip rest)
    -- one bracketed group at the front: its binders and the rest
    group s = do
      (inner, rest) <- bracketed s
      vs <- namesTyped inner
      pure (vs, rest)
    namesTyped b =
      let (ns, ty) = case topSplit (== ':') b of
            Just (a, t) | not ("=" `T.isPrefixOf` t) -> (a, T.unwords (T.words t))
            _ -> (b, "")
          names = T.words ns
       in if not (null names) && all isIdent names then Just [(n, ty) | n <- names] else Nothing
    isIdent n = case T.uncons n of
      Just (c, _) -> not (isDigit c) && T.all (\x -> isAlphaNum x || x `elem` ("_'!?." :: String)) n
      Nothing -> False
    stripAssign s =
      let s1 = T.stripEnd s
          s2 = maybe s1 T.stripEnd (T.stripSuffix "by" s1)
       in T.strip (fromMaybe s1 (T.stripSuffix ":=" s2))

-- | The text inside the bracket group at the front of @s@ and what
-- follows it.
bracketed :: Text -> Maybe (Text, Text)
bracketed s = go (0 :: Int) 0 (T.unpack s)
  where
    go _ _ [] = Nothing
    go d i (c : cs)
      | c `elem` opens = go (d + 1) (i + 1) cs
      | c `elem` closes, d == 1 = Just (T.take (i - 1) (T.drop 1 s), T.drop (i + 1) s)
      | c `elem` closes = go (d - 1) (i + 1) cs
      | otherwise = go d (i + 1) cs

-- | Split at the first character at bracket depth zero that satisfies
-- the predicate.
topSplit :: (Char -> Bool) -> Text -> Maybe (Text, Text)
topSplit p s = go (0 :: Int) 0 (T.unpack s)
  where
    go _ _ [] = Nothing
    go d i (c : cs)
      | c `elem` opens = go (d + 1) (i + 1) cs
      | c `elem` closes = go (max 0 (d - 1)) (i + 1) cs
      | d == 0 && p c = Just (T.take i s, T.drop (i + 1) s)
      | otherwise = go d (i + 1) cs

opens, closes :: String
opens = "([{⟨⦃"
closes = ")]}⟩⦄"

-- | The model's name for a variable: @|x|@ is @x@.
plainName :: Text -> Text
plainName n = fromMaybe n (T.stripPrefix "|" n >>= T.stripSuffix "|")

-- | A solver name as the Lean binder it stands for: the @|...|@ quoting,
-- then trailing superscript digits and the inaccessible-name dagger
-- removed (lean-smt names the constant of @∀ x@ @|x✝|@, a second one
-- @|x✝¹|@).
leanName :: Text -> Text
leanName = T.dropWhileEnd (== '✝') . T.dropWhileEnd (`elem` ("⁰¹²³⁴⁵⁶⁷⁸⁹" :: String)) . plainName

-- | The names a query declares with @declare-const@, in order, as written
-- (a quoted name keeps its bars).
declaredConsts :: Text -> [Text]
declaredConsts q = [n | (_, hit) <- T.breakOnAll "(declare-const" q, Just n <- [nameAfter (T.drop (T.length "(declare-const") hit)]]
  where
    nameAfter r0 = case T.uncons r0 of
      Just (c, _) | isSpace c -> case T.uncons (T.stripStart r0) of
        Just ('|', r) -> let (a, b) = T.breakOn "|" r in if T.null b then Nothing else Just ("|" <> a <> "|")
        Just _ -> let n = T.takeWhile (\x -> not (isSpace x) && x `notElem` ("()" :: String)) (T.stripStart r0) in if T.null n then Nothing else Just n
        Nothing -> Nothing
      _ -> Nothing

-- | The witness tactic for a model: the model's values for the leading
-- bound variables (name and Lean type, in order) as Lean literals in
-- @push_neg; refine ⟨v₁, …, vₙ, ?_⟩; first | decide | norm_num | omega | simp_all@.
-- A variable's value is the model constant whose 'leanName' is the
-- variable's name; when that does not pick exactly one constant for every
-- variable, the variables are paired in order with the first constants of
-- @decls@ (the query's 'declaredConsts'), whose values the kernel probe
-- then checks like any other. Nothing when there are no variables, the
-- model has functions, or a variable has no value either way, is
-- 'VOther', is a negative @Nat@, or has a value whose sort does not fit
-- its type.
witnessTactic :: Model -> [Text] -> [(Text, Text)] -> Maybe Text
witnessTactic m decls vars
  | null vars || not (null (mFunctions m)) = Nothing
  | otherwise = do
      vals <- maybe positional Just byName
      vs <- mapM (uncurry literal) (zip (map snd vars) vals)
      pure ("push_neg; refine ⟨" <> T.intercalate ", " (vs <> ["?_"]) <> "⟩; first | decide | norm_num | omega | simp_all")
  where
    byName = mapM (\(n, _) -> case [val | (x, _, val) <- mAssign m, leanName x == n] of
                                 [v] -> Just v
                                 _ -> Nothing) vars
    positional
      | length decls < length vars = Nothing
      | otherwise = mapM (\d -> lookup (plainName d) [(plainName x, val) | (x, _, val) <- mAssign m]) (take (length vars) decls)
    literal ty v = case v of
      VInt k
        | ty `elem` ["Nat", "ℕ"] -> if k < 0 then Nothing else Just ("(" <> tshow k <> " : Nat)")
        | ty `elem` ["Int", "ℤ"] -> Just ("(" <> tshow k <> " : Int)")
        | T.null ty -> Just (if k < 0 then "(" <> tshow k <> ")" else tshow k)
        | otherwise -> Nothing
      VBool b
        | ty `elem` ["Bool", ""] -> Just (if b then "true" else "false")
        | ty == "Prop" -> Just (if b then "True" else "False")
        | otherwise -> Nothing
      VBitVec w k
        | T.null ty || ty == "BitVec " <> tshow w -> Just ("0x" <> T.pack (showHex k "") <> "#" <> tshow w)
        | otherwise -> Nothing
      VOther _ -> Nothing

-- | A model as text, one @name := value@ line per constant, then one line
-- per function.
modelText :: Model -> Text
modelText m =
  T.intercalate "\n" $
    [plainName n <> " := " <> shown v | (n, _, v) <- mAssign m] <> [plainName f <> " := (a function)" | f <- mFunctions m]
  where
    shown = \case
      VInt k -> tshow k
      VBool b -> if b then "true" else "false"
      VBitVec w k -> "0x" <> T.pack (showHex k "") <> "#" <> tshow w
      VOther t -> t

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | The next negation number: unique in the process, so no two probes
-- name their negation alike.
negCounter :: IORef Int
negCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE negCounter #-}

nextNeg :: IO Int
nextNeg = atomicModifyIORef' negCounter (\k -> (k + 1, k + 1))

-- | The sources in order at the goal's first hole; each one's
-- counterexample is certified through 'probe' on the base cut after the
-- goal's declaration, with the negation after it. @plausible@ and the
-- query tactic get the per-hole wall cap, so does each probe, and the
-- solver gets the 'SmtEnv''s. A negation certifies when the probe has no
-- error, exits 0, no @sorry@ warning on its line, and its printed axioms
-- hold no @sorryAx@ and nothing the policy does not allow.
--
-- A solver's model is certified by its witness tactic first, then by
-- 'certifyTactics' unless an earlier source already tried them (they do
-- not depend on the counterexample). The first certified negation is the
-- result, else the first suspect, else 'DUnknown'; with it, the source
-- that gave it (for 'DUnknown' the last that ran) and what each source
-- said. The SMT source needs the 'SmtEnv' and is skipped without it; the
-- query goes through 'prepareQuery' and then 'smRun'. A goal that is
-- missing or has no hole is 'Missed'; a server failure is 'LspDown'.
disprove :: Maybe SmtEnv -> [DisproveSource] -> AutoConfig -> Lsp -> LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text
         -> IO (Either AutoFail (Disproof, Maybe DisproveSource, DisproveInfo))
disprove = disproveFrom OnCounterexample

-- | 'disprove' that never probes 'certifyTactics': a solver's model is
-- certified by its witness tactic alone (one probe at most, none when the
-- model gives no witness). The flat search's step before a goal's first
-- attempt.
disproveWitnessOnly :: Maybe SmtEnv -> [DisproveSource] -> AutoConfig -> Lsp -> LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text
                    -> IO (Either AutoFail (Disproof, Maybe DisproveSource, DisproveInfo))
disproveWitnessOnly = disproveFrom WitnessOnly

-- | 'disprove' for a goal that already holds a solver's model the early
-- step could not certify (it never probes 'certifyTactics'): when no
-- source here certifies and this call has not probed 'certifyTactics',
-- they are probed once at the end, whatever the sources said, and a pass
-- is a 'DFalse' from 'ViaSmt' (through the same check as any other).
disproveAfterSuspect :: Maybe SmtEnv -> [DisproveSource] -> AutoConfig -> Lsp -> LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text
                     -> IO (Either AutoFail (Disproof, Maybe DisproveSource, DisproveInfo))
disproveAfterSuspect = disproveFrom CertifyAtEnd

-- | How a run uses 'certifyTactics': after a counterexample ('disprove'),
-- never ('disproveWitnessOnly'), or also once at the end when nothing
-- probed them ('disproveAfterSuspect').
data TacticsMode = OnCounterexample | WitnessOnly | CertifyAtEnd
  deriving (Eq)

-- | The three above.
disproveFrom :: TacticsMode -> Maybe SmtEnv -> [DisproveSource] -> AutoConfig -> Lsp -> LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text
             -> IO (Either AutoFail (Disproof, Maybe DisproveSource, DisproveInfo))
disproveFrom mode menv sources cfg lsp lr pol root rel base name =
  case findDecl name ds of
    Nothing -> pure (Left Missed)
    Just d -> case holesIn base d of
      [] -> pure (Left Missed)
      (h : _) -> run d h (mode == WitnessOnly) Nothing Nothing [] sources
  where
    ds = declarations base
    -- tried: certifyTactics already ran (or are skipped); best: the first suspect and its
    -- source; lastSrc: the last source that ran; steps: newest first
    run d _ tried best lastSrc steps [] = do
      let info = DisproveInfo (reverse steps) Nothing
      hit <- if mode == CertifyAtEnd && not tried then certifyWith d certifyTactics else pure Nothing
      pure . Right $ case (hit, best) of
        (Just t, _) -> (DFalse t, Just ViaSmt, info)
        (Nothing, Just (c, src)) -> (DSuspect c, Just src, info)
        (Nothing, Nothing) -> (DUnknown, lastSrc, info)
    run d h tried best lastSrc steps (src : rest) = case src of
      ViaPlausible -> do
        r <- guarded lsp (acSeconds cfg) (lspAttempts lsp (cutAfter base d) h [wrapSnippet (acHeartbeats cfg) "plausible"])
        case r of
          Left e -> pure (Left e)
          Right (Just (tr : _)) | DSuspect c <- parsePlausible tr -> do
            let steps' = (ViaPlausible, "counterexample") : steps
            hit <- if tried then pure Nothing else certifyWith d certifyTactics
            case hit of
              Just t -> pure (Right (DFalse t, Just ViaPlausible, DisproveInfo (reverse steps') Nothing))
              Nothing -> run d h True (orElse best (c, ViaPlausible)) (Just ViaPlausible) steps' rest
          Right _ -> run d h tried best (Just ViaPlausible) ((ViaPlausible, "none") : steps) rest
      ViaSmt -> case menv of
        Nothing -> run d h tried best lastSrc steps rest
        Just env -> do
          let tac = smQueryTactic env
          r <- guarded lsp (acSeconds cfg) (lspAttempts lsp (cutAfter base d) h [wrapSnippet (acHeartbeats cfg) (queryTactic tac)])
          case r of
            Left e -> pure (Left e)
            Right mtrs -> case parseSmtQuery tac =<< (firstTrial =<< mtrs) of
              Nothing -> run d h tried best (Just ViaSmt) ((ViaSmt, "none") : steps) rest
              Just q -> do
                res <- solve env (fst (prepareQuery True q))
                let next k = run d h tried best (Just ViaSmt) ((ViaSmt, k) : steps) rest
                case res of
                  Unsat -> next "unsat"
                  Unknown _ -> next "unknown"
                  SolverError _ -> next "error"
                  Sat m -> do
                    let steps' = (ViaSmt, "sat") : steps
                        mt = modelText m
                        witness = maybe [] pure (witnessTactic m (declaredConsts q) (boundVariables (dStatement d)))
                    hitW <- certifyWith d witness
                    case hitW of
                      Just t -> pure (Right (DFalse t, Just ViaSmt, DisproveInfo (reverse steps') (Just mt)))
                      Nothing -> do
                        hit <- if tried then pure Nothing else certifyWith d certifyTactics
                        case hit of
                          Just t -> pure (Right (DFalse t, Just ViaSmt, DisproveInfo (reverse steps') Nothing))
                          Nothing -> run d h True (orElse best (mt, ViaSmt)) (Just ViaSmt) steps' rest
    orElse best x = best <|> Just x
    firstTrial = \case
      (tr : _) -> Just tr
      [] -> Nothing
    -- the solver, a synchronous exception from it an error
    solve env q = do
      r <- try (smRun env (smSolver env) (smTimeout env) q)
      case r of
        Left ex
          | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
          | otherwise -> pure (SolverError (T.pack (show (ex :: SomeException))))
        Right x -> pure x
    -- the first tactic whose negation certifies
    certifyWith _ [] = pure Nothing
    certifyWith d (t : ts) = do
      k <- nextNeg
      let (text, line) = insertAfter d (negationTheorem k name t)
          neg = maybe ("_fisher_neg_" <> T.pack (show k)) dName (find (\x -> dStart x == line - 1) (declarations text))
      r <- try (timeout (max 1 (ceiling (acSeconds cfg * 1000000) :: Int)) (probe lr root rel text [neg]))
      case r of
        Left ex
          | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
          | otherwise -> const (certifyWith d ts) (ex :: SomeException)
        Right (Just pr) | passes line neg pr -> pure (Just t)
        Right _ -> certifyWith d ts
    passes line neg pr =
      null (prErrors pr) && prExit pr == 0 && line `notElem` prSorryLines pr
        && case Map.lookup neg (prAxioms pr) of
             Just axs -> "sorryAx" `notElem` axs && all (axiomAllowed pol) axs
             Nothing -> False
    -- the base cut after the declaration, a blank line and the negation,
    -- and the 1-based line the negation is on
    insertAfter d ex =
      let pre = take (dEnd d) (T.splitOn "\n" base)
       in (T.intercalate "\n" (pre <> ["", ex]), length pre + 2)
