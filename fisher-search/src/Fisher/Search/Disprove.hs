-- | Disproving a goal before a model is asked: run Mathlib's @plausible@
-- at the goal's hole through the 'Lsp', and when it finds a counterexample
-- try to certify the negation with a kernel-checked
-- @theorem _fisher_neg_\<k\> : not (type_of% \@NAME) := by \<tactic\>@ placed
-- after the goal's declaration, its axioms printed and held to the axiom
-- policy. Only a certified negation is 'DFalse'; a counterexample alone is
-- a 'DSuspect' (a hint, never a verdict).
module Fisher.Search.Disprove
  ( Disproof (..)
  , parsePlausible
  , negationTheorem
  , certifyTactics
  , disprove
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Fisher.Search.Auto
import Fisher.Search.Check (AxiomPolicy, LeanRun, Probe (..), axiomAllowed, probe)
import Fisher.Search.Lean (Decl (..), declarations, findDecl)

data Disproof
  = DFalse Text
  -- ^ the tactic that proved the negation
  | DSuspect Text
  -- ^ the counterexample text, not certified
  | DUnknown
  deriving (Eq, Show)

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

-- | The next negation number: unique in the process, so no two probes
-- name their negation alike.
negCounter :: IORef Int
negCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE negCounter #-}

nextNeg :: IO Int
nextNeg = atomicModifyIORef' negCounter (\k -> (k + 1, k + 1))

-- | @plausible@ at the goal's first hole, then (only for a counterexample)
-- each certify tactic through 'probe' on the base cut after the goal's
-- declaration, with the negation after it. @plausible@ gets the per-hole
-- wall cap, and so does each probe. A negation certifies when the probe
-- has no error, exits 0, no @sorry@ warning on its line, and its printed
-- axioms hold no @sorryAx@ and nothing the policy does not allow. A goal
-- that is missing or has no hole is 'Missed'.
disprove :: AutoConfig -> Lsp -> LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text -> IO (Either AutoFail Disproof)
disprove cfg lsp lr pol root rel base name =
  case findDecl name ds of
    Nothing -> pure (Left Missed)
    Just d -> case holesIn base d of
      [] -> pure (Left Missed)
      (h : _) -> do
        r <- guarded lsp (acSeconds cfg) (lspAttempts lsp (cutAfter base d) h [wrapSnippet (acHeartbeats cfg) "plausible"])
        case r of
          Left e -> pure (Left e)
          Right (Just (tr : _)) | DSuspect c <- parsePlausible tr -> Right <$> certify d c certifyTactics
          Right _ -> pure (Right DUnknown)
  where
    ds = declarations base
    certify _ c [] = pure (DSuspect c)
    certify d c (t : ts) = do
      k <- nextNeg
      let (text, line) = insertAfter d (negationTheorem k name t)
          neg = maybe ("_fisher_neg_" <> T.pack (show k)) dName (find (\x -> dStart x == line - 1) (declarations text))
      r <- try (timeout (max 1 (ceiling (acSeconds cfg * 1000000) :: Int)) (probe lr root rel text [neg]))
      case r of
        Left ex
          | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
          | otherwise -> const (certify d c ts) (ex :: SomeException)
        Right (Just pr) | passes line neg pr -> pure (DFalse t)
        Right _ -> certify d c ts
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
