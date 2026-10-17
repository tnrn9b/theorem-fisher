-- | Scripted stand-ins shared by the graph specs: a worker attempt that
-- writes a given proof, and the planner and verifier replies.
module Fisher.Search.Scripted
  ( withProof
  , plan
  , planWith
  , accept
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Search.Lean

-- | The file with the proof of the named declaration replaced by the given
-- tactic text (one line, set under a @:= by@ ending the statement's line).
withProof :: Text -> Text -> Text -> Text
withProof goal proof start =
  let ls = T.splitOn "\n" start
      d = fromMaybe (error ("no goal " <> T.unpack goal)) (findDecl goal (declarations start))
      stmtLine = ls !! dStmtEnd d
      cut = fst (T.breakOn ":=" stmtLine) <> ":= by"
   in T.intercalate "\n" (take (dStmtEnd d) ls <> [cut, "  " <> proof] <> drop (dEnd d) ls)

-- | A planner reply: lemmas (name, statement), the node's declaration head, its new proof.
plan :: [(Text, Text)] -> Text -> Text -> Text
plan = planWith "lemma"

-- | 'plan' with the keyword that opens each lemma (core Lean has no @lemma@).
planWith :: Text -> [(Text, Text)] -> Text -> Text -> Text
planWith kw lemmas node proof = T.unlines $
  ["```blueprint", "split it", "```", "```lean"]
    <> concat [[kw <> " " <> n <> " : " <> s <> " := by", "  sorry", ""] | (n, s) <- lemmas]
    <> [node <> " := by", "  " <> proof, "```"]

-- | A verifier reply that accepts the split.
accept :: Text
accept = "{\"accept\": true, \"reason\": \"fine\", \"suspect\": []}"
