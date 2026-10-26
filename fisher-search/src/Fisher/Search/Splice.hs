-- | Carry one goal's proof from an attempt's file into the current base.
--
-- An attempt starts from a snapshot of the base (@start@) and ends with
-- the agent's file (@agent@). What it contributes for goal @g@ is g's
-- declaration as the agent left it, and whatever top-level material it
-- added (helper lemmas, @open@, @set_option ... in@). Its edits inside other
-- declarations are dropped: another attempt owns those. Anything else it
-- changed or removed outside declarations (definitions, imports, the
-- statement) refuses the splice. The base may have moved on since the
-- attempt started (other goals merged); the splice applies to the base as
-- it is now, with the helpers placed just above g.
module Fisher.Search.Splice
  ( SpliceError (..)
  , Spliced (..)
  , splice
  , deniedLine
  , usesSorry
  , smtKept
  , applySmtImport
  ) where

import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Search.Lean
import Fisher.Smt.Lake (stripImportSmt)

data SpliceError
  = TargetMissing Text
  | StatementChanged
  | TargetStillOpen
  | OutsideChanged Text
  | NameClash Text
  | Denied Text
  -- ^ an added axiom, @#exit@ or an option that turns the kernel's checks off
  deriving (Eq, Show)

data Spliced = Spliced
  { spText :: Text
  , spHelpers :: [Text]
  -- ^ the names of the declarations added above the goal
  }
  deriving (Eq, Show)

-- | The file split into the parts outside declarations (one entry per
-- line) and a marker per declaration, so that two versions can be compared
-- outside the declarations they share.
data Piece = Line Text | Marker Text
  deriving (Eq, Show)

skeleton :: Set.Set Text -> [Text] -> [Decl] -> [Piece]
skeleton shared ls ds = go 0 (sortOn dStart [d | d <- ds, dName d `Set.member` shared])
  where
    go i [] = map Line (drop i ls)
    go i (d : rest) = map Line (take (dStart d - i) (drop i ls)) <> [Marker (dName d)] <> go (dEnd d) rest

-- | Greedy in-order match of @old@ in @new@: the pieces of @old@ not found,
-- and the runs of @new@ between matches (each run with its position:
-- the index of the matched piece of @old@ it precedes, or the end).
match :: [Piece] -> [Piece] -> ([Piece], [[Piece]])
match old new = go old new [] []
  where
    go [] rest missing runs = (reverse missing, reverse (rest : runs))
    go (o : os) ns missing runs = case break (== o) ns of
      (_, []) -> go os ns (o : missing) runs
      (before, _ : after) -> go os after missing (before : runs)

splice :: Text -> Text -> Text -> Text -> Either SpliceError Spliced
splice start agent base g = do
  let lsS = T.splitOn "\n" start
      lsA = T.splitOn "\n" agent
      lsB = T.splitOn "\n" base
      dsS = declarations start
      dsA = declarations agent
      dsB = declarations base
  gS <- maybe (Left (TargetMissing "in the attempt's starting file")) Right (findDecl g dsS)
  gA <- maybe (Left (TargetMissing "in the agent's file")) Right (findDecl g dsA)
  gB <- maybe (Left (TargetMissing "in the base")) Right (findDecl g dsB)
  if statementHead gA /= statementHead gS then Left StatementChanged else Right ()
  if isOpen gA then Left TargetStillOpen else Right ()
  let shared = Set.fromList (map dName dsS) `Set.intersection` Set.fromList (map dName dsA)
      skS = skeleton shared lsS dsS
      skA = skeleton shared lsA dsA
      (missing, runs) = match skS skA
      lost = [l | Line l <- missing, not (T.null (T.strip l))] <> [m | Marker m <- missing]
  case lost of
    (l : _) -> Left (OutsideChanged (T.take 120 l))
    [] -> Right ()
  let added = [ls' | run <- runs, let ls' = [l | Line l <- run], any (not . T.null . T.strip) ls']
  let newDecl = take (dEnd gA - dStart gA) (drop (dStart gA) lsA)
  case filter denied (concat added <> newDecl) of
    (l : _) -> Left (Denied (T.take 120 l))
    [] -> Right ()
  -- a run the base already holds word for word (the same helper from an
  -- earlier merge) is not added again
  let fresh = [r | r <- added, not (T.intercalate "\n" (trimRun r) `T.isInfixOf` base)]
      helperText = T.intercalate "\n" (concatMap trimRun fresh)
      helperNames = map dName (declarations helperText)
      baseNames = Set.fromList (map dName dsB)
  case filter (`Set.member` baseNames) helperNames of
    (c : _) -> Left (NameClash c)
    [] -> Right ()
  -- a helper left open would join the goals (and its sorry would not
  -- show in the goal's axioms if the proof does not use it)
  case [dName d | d <- declarations helperText, usesSorry (dProof d)] of
    (h : _) -> Left (Denied ("the helper " <> h <> " uses sorry"))
    [] -> Right ()
  let replaced = take (dStart gB) lsB <> newDecl <> drop (dEnd gB) lsB
      -- a blank line after each run, but not after `set_option ... in`,
      -- which belongs to the declaration below it
      insertion = concatMap (\r -> let t = trimRun r in t <> [ "" | not (" in" `T.isSuffixOf` T.stripEnd (last t)) ]) fresh
      out = take (dDocStart gB) replaced <> insertion <> drop (dDocStart gB) replaced
  Right (Spliced (T.intercalate "\n" out) helperNames)

-- | A run without its leading and trailing blank lines.
trimRun :: [Text] -> [Text]
trimRun = reverse . dropWhile blank . reverse . dropWhile blank
  where blank = T.null . T.strip

usesSorry :: Text -> Bool
usesSorry t = any (`elem` ["sorry", "admit"]) (identTokens t)

-- | The text cut at every character that cannot be part of an identifier.
identTokens :: Text -> [Text]
identTokens = T.split (not . isIdent)
  where isIdent c = c == '_' || c == '.' || c == '\'' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')

-- | Whether a proof the search kept uses the @smt@ tactic, given the base
-- at the start and the merged text: a kept proof is a declaration of the
-- merged text that the start does not have as it is (a goal's new proof,
-- a helper merged with it), and it uses @smt@ when the token stands in its
-- code (comments left out; @simp@ or @smt_show@ do not count).
smtKept :: Text -> Text -> Bool
smtKept start final = any usesSmt kept
  where
    key d = (dName d, dStatement d, dProof d)
    before = Set.fromList (map key (declarations start))
    kept = [d | d <- declarations final, not (Set.member (key d) before)]
    usesSmt d = any (elem "smt" . identTokens) (proofCode d)

-- | The merged text as @--apply@ writes it, given the base at the start
-- (which carries the @import Smt@ marker line the search put in): the
-- marker stays only when a kept proof uses @smt@ ('smtKept'), else it is
-- removed ('stripImportSmt'; a user's own @import Smt@ stays).
applySmtImport :: Text -> Text -> Text
applySmtImport start final
  | smtKept start final = final
  | otherwise = stripImportSmt final

-- | The rule as other modules apply it to a line.
deniedLine :: Text -> Bool
deniedLine = denied

-- | Added lines no proof may bring: new axioms, @#exit@ (which would hide
-- the rest of the file from the check), and options that skip the
-- kernel's checking.
denied :: Text -> Bool
denied l =
  let ws = T.words l
      mods = dropWhile (\w -> w `elem` ["private", "protected", "noncomputable", "unsafe"] || "@[" `T.isPrefixOf` w) ws
   in take 1 mods == ["axiom"]
        || take 1 ws == ["#exit"]
        || (take 1 ws == ["set_option"] && any (\w -> "debug." `T.isPrefixOf` w || w == "trust") (take 2 (drop 1 ws)))
        || "skipKernelTC" `T.isInfixOf` l
