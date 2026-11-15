{-# LANGUAGE DataKinds #-}

-- | The judge: one model call per failed attempt the rules did not explain
-- ("Fisher.Learn.Mistakes"), asking what went wrong, the lesson, how sure
-- it is, and two sentences to read before starting the goal again.
--
-- The prompt carries only the attempt's event data (already redacted in the
-- run log): its last 40 message and tool events, tool args cut to 200
-- characters and results to 500, messages to 500, and the goal and the
-- outcome text, which are not event text the log redacts (an outcome can
-- hold a provider's error text), so they pass 'redactSensitiveText' here.
module Fisher.Learn.Judge
  ( Verdict (..)
  , judgeMessages
  , judgeRetryMessages
  , parseVerdict
  , judgeAttempt
  , lessonKey
  , judgeKey
  , sameLesson
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intersperse, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Fisher.Learn.Mistakes (AttemptView (..), ToolCall (..), resultText, toolCalls)
import Fisher.Lessons.File (levenshtein, normaliseLesson)
import Fisher.Redact (redactSensitiveText)
import Fisher.Role (Prompt, PromptPart, Promptable (..), Role (..), Said, saidText, source, static, tool, worker)
import Fisher.Role.Agent (AgentM, askTwice)
import Fisher.Role.Internal (said')
import Fisher.Search.Ask (Asked)
import Fisher.Search.Review (jsonObject, textOr)

-- | The judge's reply: the mistake in one line, the lesson (Nothing when
-- the judge was not sure enough), its confidence from 0 to 1, and what to
-- tell yourself before the next try. Its texts are what the judge said:
-- they reach a prompt only as lesson candidates ('Fisher.Role.candidate').
data Verdict = Verdict { vMistake :: Said 'Judge, vLesson :: Maybe (Said 'Judge), vConfidence :: Double, vTellYourself :: Said 'Judge }
  deriving (Eq, Show)

judgeSystem :: Text
judgeSystem = T.unlines
  [ "You review a failed attempt by an agent to prove a Lean 4 goal."
  , "You are given the end of its transcript and the reason the proof was rejected."
  , "Find the one mistake that most explains the failure."
  , "Reply with the JSON object only, in this form:"
  , "{\"mistake\": \"...\", \"lesson\": \"...\", \"confidence\": <0 to 1>, \"tell_yourself\": \"...\"}"
  , "mistake is one line. lesson is one or two imperative sentences that would help any agent avoid the mistake on another goal."
  , "lesson may be null when there is no general lesson."
  , "confidence is a number from 0 to 1: how sure you are that the lesson is right and general."
  , "tell_yourself is two sentences for the agent to read before it starts this goal again."
  ]

-- | System and user messages for one attempt: the goal's name (source),
-- Fisher's classification of it and of the outcome, the outcome's text
-- (tool output: a prover's rejection or a provider's error) and the
-- attempt's transcript (the worker's run).
judgeMessages :: AttemptView -> Prompt
judgeMessages av =
  [ ("system", static judgeSystem)
  , ("user", judgeUser av)
  ]

judgeUser :: AttemptView -> PromptPart
judgeUser av =
  mconcat . intersperse (static "\n\n") $
    [ static "The goal: " <> source (redactSensitiveText (avGoal av)) <> static (if T.null (avShape av) then "" else " (shape " <> avShape av <> ")")
    , static ("The outcome: " <> avClass av) <> (if T.null (avOutcomeText av) then mempty else static ", " <> tool (T.take 500 (redactSensitiveText (avOutcomeText av))))
    , static "The last events of the attempt:\n" <> toPrompt (worker (T.unlines (transcript (avEvents av))))
    , static "Reply with the JSON object only."
    ]

-- | The messages of the second call, when the first reply could not be
-- read: the first call's, the user's turn followed by why. The judge's own
-- reply is not in them: a judge's text reaches a prompt only as a lesson
-- candidate, so it does not go back as an assistant turn the way the
-- planner's and the verifier's do.
judgeRetryMessages :: AttemptView -> Text -> Prompt
judgeRetryMessages av why =
  [ ("system", static judgeSystem)
  , ("user", judgeUser av <> static ("\n\nYour previous reply could not be used: " <> why <> "\nReply with the JSON object only."))
  ]

-- | The last 40 message and tool events as lines, in seq order. A tool
-- call is one line, @tool NAME(args) -> result@, when its start and end are
-- both in the window (paired as "Fisher.Learn.Mistakes" pairs them: by
-- @call_id@, else first in first out by name); an end alone is
-- @tool NAME -> result@.
transcript :: [(Int, Double, Text, Value)] -> [Text]
transcript es = map snd (sortOn fst (msgs <> map callLine (toolCalls window)))
  where
    window = lastN 40 [e | e@(_, _, k, _) <- es, k `elem` ["message", "tool_start", "tool_end"]]
    msgs = [(s, fromMaybe "?" (str "role" d) <> ": " <> T.take 500 (oneLine (fromMaybe "" (str "text" d)))) | (s, _, "message", d) <- window]
    callLine c =
      ( tcSeq c
      , "tool " <> (if T.null (tcName c) then "?" else tcName c)
          <> maybe "" (\a -> "(" <> T.take 200 (args a) <> ")") (tcArgs c)
          <> maybe "" (\(_, d) -> " -> " <> T.take 500 (oneLine (resultText d))) (tcEnd c)
      )
    args = \case
      Null -> ""
      String a -> oneLine a
      v -> jsonText v
    str k = \case
      Object o | Just (String x) <- KM.lookup (K.fromText k) o -> Just x
      _ -> Nothing
    jsonText = TE.decodeUtf8With TEE.lenientDecode . BL.toStrict . A.encode
    oneLine = T.replace "\n" "\\n"

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- | The judge's JSON: mistake, confidence (a number) and tell_yourself are
-- required; lesson may be missing. Texts are put on one line and cut to 300
-- characters; under confidence 0.6 the lesson is dropped. Never throws.
-- The texts are what the judge said, read out of its reply: here, and
-- only here, its pieces are made @Said 'Judge@ (by @said'@).
parseVerdict :: Said 'Judge -> Either Text Verdict
parseVerdict s = case jsonObject (saidText s) of
  Nothing -> Left "the reply is not the JSON object asked for"
  Just o -> do
    mistake <- required "mistake" o
    conf <- case KM.lookup "confidence" o of
      Just (Number n) -> Right (max 0 (min 1 (toRealFloat n)))
      _ -> Left "confidence must be a number from 0 to 1"
    tell <- required "tell_yourself" o
    lesson <- maybe (Left "lesson must be a string") Right (textOr "lesson" o <|> nullLesson o)
    if T.null tell
      then Left "tell_yourself is empty"
      else
        Right
          Verdict
            { vMistake = said' (cut mistake)
            , vLesson = if conf < 0.6 || T.null (cut lesson) then Nothing else Just (said' (cut lesson))
            , vConfidence = conf
            , vTellYourself = said' tell
            }
  where
    required k o = case KM.lookup (K.fromText k) o of
      Just (String s) -> Right (cut s)
      _ -> Left (k <> " must be a string")
    nullLesson o = case KM.lookup "lesson" o of
      Just Null -> Just ""
      _ -> Nothing
    a <|> b = maybe b Just a
    cut = T.take 300 . T.unwords . T.words

-- | One judge call (a second one when the reply cannot be read, see
-- 'judgeRetryMessages').
judgeAttempt :: AttemptView -> AgentM 'Judge (Asked Verdict)
judgeAttempt av = askTwice (maybe (judgeMessages av) (judgeRetryMessages av . snd)) id (pure . parseVerdict)

-- | A lesson's dedup key: the first Lean identifier it mentions,
-- normalised, else the whole text normalised. An identifier is a dotted name
-- (@Nat.succ_le@: parts of letters, digits and @_'!?@, starting with a
-- letter or @_@, one part of 2 characters or more, not a file name such as
-- @Foo.lean@ and not a version such as @4.12.0@), or a backticked name with
-- an underscore that is neither a tool name (@lean_loogle@, @write_file@)
-- nor a tactic (@simp_all@, @norm_num@, ...).
lessonKey :: Text -> Text
lessonKey t = fromMaybe (normaliseLesson t) (identifier t)

-- | The key of a @judged@ or @self@ lesson's text: its kind, a colon and
-- 'lessonKey', so a judged text that names a rule kind (@`budget_burn`@)
-- never shares a key with that rule's row.
judgeKey :: Text -> Text -> Text
judgeKey kind t = kind <> ":" <> lessonKey t

identifier :: Text -> Maybe Text
identifier t = listToMaybe (mapMaybe ident (T.words t))
  where
    ident w
      | T.any (== '`') w =
          let b = T.strip (backticked w)
           in if dotted b || underscored b then Just (normaliseLesson b) else Nothing
      | dotted (core w) = Just (normaliseLesson (core w))
      | otherwise = Nothing
    -- the part of a word between its first two backticks (or after the first)
    backticked w = T.takeWhile (/= '`') (T.drop 1 (T.dropWhile (/= '`') w))
    core = T.dropWhileEnd (not . partChar) . T.dropWhile (\c -> not (startChar c))
    dotted c =
      let ps = T.splitOn "." c
       in length ps > 1
            && all (\p -> not (T.null p) && T.all partChar p) ps
            && maybe False (startChar . fst) (T.uncons c)
            && any ((>= 2) . T.length) ps
            && not (any (`T.isSuffixOf` T.toLower c) [".lean", ".olean", ".md", ".json", ".yaml"])
    underscored c =
      T.any (== '_') c && T.all partChar c && maybe False (startChar . fst) (T.uncons c)
        && not ("lean_" `T.isPrefixOf` c || "mcp_" `T.isPrefixOf` c) && c `notElem` toolNames && c `notElem` tactics
    startChar c = isAsciiUpper c || isAsciiLower c || c == '_'
    partChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("_'!?" :: String)
    toolNames = ["write_file", "read_file", "search_files", "web_search"]
    tactics =
      [ "simp", "simp_all", "simp_arith", "omega", "decide", "norm_num", "linarith", "nlinarith", "positivity"
      , "ring", "ring_nf", "field_simp", "push_cast", "aesop", "grind", "rfl", "exact", "exact?", "apply", "apply?", "rw"
      , "intro", "intros", "cases", "rcases", "obtain", "induction", "refine", "constructor", "use", "calc"
      , "gcongr", "norm_cast", "exact_mod_cast", "trivial", "tauto" ]

-- | The same lesson: the same identifier when both mention one; else the
-- same normalised text, or both up to 120 characters and at most 8 edits
-- apart.
sameLesson :: Text -> Text -> Bool
sameLesson a b = case (identifier a, identifier b) of
  (Just x, Just y) -> x == y
  _ -> na == nb || (T.length na <= 120 && T.length nb <= 120 && levenshtein na nb <= 8)
  where
    na = normaliseLesson a
    nb = normaliseLesson b
