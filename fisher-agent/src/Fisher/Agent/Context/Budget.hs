-- | What CLM tells the model, and the small rules around the budget
-- (docs/clm.md sections 6, 8 and 10): the wording, the nudge thresholds,
-- shortening edit turns, cutting long results.
module Fisher.Agent.Context.Budget
  ( commas
  , percent
  , promptSection
  , prevPath
  , nudgeText
  , urgentText
  , rollbackNote
  , budgetError
  , receiptApplied
  , receiptRejectedFit
  , receiptRejectedShrink
  , receiptNotUtf8
  , receiptOnlyKept
  , readout
  , readStub
  , cutResult
  , isErrorResult
  , shortenWrites
  , pathArg
  , samePath
  , mentionsContext
  , cmdArg
  , pathAliases
  , thresholds
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Agent.Context.Mirror (tokens)
import Fisher.Py.Value

-- | @f"{n:,}"@
commas :: Integer -> Text
commas n
  | n < 0 = "-" <> commas (negate n)
  | otherwise =
      let s = T.pack (show n)
          groups = reverse (map T.reverse (T.chunksOf 3 (T.reverse s)))
       in T.intercalate "," groups

percent :: Integer -> Integer -> Integer
percent t limit = t * 100 `div` limit

-- | The backup beside the mirror (section 3).
prevPath :: Text -> Text
prevPath p = maybe (p <> ".prev") (<> ".prev.txt") (T.stripSuffix ".txt" p)

promptSection :: Text -> Integer -> Text
promptSection path limit =
  "Your conversation is kept in a context file at " <> path <> ". It holds every turn after the task, one block per \
  \turn, each starting with a header line such as `<<< turn 7 assistant >>>`. The system prompt and the task \
  \are pinned and are not in the file. You can edit the file with patch or write_file to drop turns you no \
  \longer need, or to replace long tool output with a short note of what mattered. After each round of tool \
  \calls your edits become the conversation. Keep headers as they are on turns you leave untouched: an \
  \untouched turn keeps its tool calls, and a changed one becomes plain text. Read the file before you rewrite \
  \it with write_file: turns you have not read in it are kept. Before an edit is applied, the file as it was \
  \is saved at " <> prevPath path <> ", so you can copy back turns you dropped by mistake. Once a round is over, your \
  \edits to the file are kept only as their lengths, and a read of the file only as its length. \
  \The context has a budget of " <> commas limit <> " tokens. The budget limits how much of the conversation is \
  \kept, not how much work you can do: when it fills, compact the file and carry on with the task. Each tool \
  \result ends with a context line showing how \
  \much is used. If the budget is exceeded, your newest turns are dropped and you are asked to compact the \
  \file. Turns that only edit the context file do not count towards your step limit. Read and edit the context \
  \file with read_file, patch and write_file, not through the terminal."

-- | The three plain thresholds (section 8).
thresholds :: Integer -> [(Int, Integer)]
thresholds limit = [(p, limit * fromIntegral p `div` 100) | p <- [25, 50, 75]]

nudgeText :: Int -> Text
nudgeText p = "at " <> T.pack (show p) <> "% of the budget, consider dropping turns you no longer need"

urgentText :: Text
urgentText = "nearly full: compact the context file, then carry on with the task (otherwise the newest turns will be dropped)"

-- | The rollback note: the dropped turns, oldest first (accounting form).
rollbackNote :: [[PyVal]] -> Text -> Maybe Text -> Text
rollbackNote dropped path todo =
  let label t = case t of
        (m : _)
          | get "role" m == Just (PStr "assistant")
          , Just (PList cs@(_ : _)) <- get "tool_calls" m ->
              T.intercalate ", " [maybe "" pyStrOf (get "function" c >>= get "name") | c <- cs]
          | otherwise -> maybe "" pyStrOf (get "role" m)
        [] -> ""
      labels = T.intercalate "; " (map label dropped)
      size = commas (tokens (concat dropped))
      (what, it) =
        if length dropped == 1
          then ("the last turn (" <> labels <> ", " <> size <> " tokens) was", "it")
          else ("the last " <> T.pack (show (length dropped)) <> " turns (" <> labels <> "; " <> size <> " tokens) were", "them")
      note =
        "[Context over budget: " <> what <> " dropped from the context. Tool calls in " <> it
          <> " already ran. Compact the context file at " <> path <> ", then carry on with the task.]"
   in note <> maybe "" ("\n\n" <>) todo

budgetError :: Int -> Integer -> Text
budgetError n limit =
  "Context budget exceeded: " <> T.pack (show n) <> " rollbacks in a row did not bring the context under " <> commas limit <> " tokens."

receiptApplied :: Int -> Int -> Integer -> Integer -> Int -> Text
receiptApplied n0 n1 t0 t1 kept =
  "context edit applied: " <> T.pack (show n0) <> " turns -> " <> T.pack (show n1) <> ", " <> commas t0 <> " -> " <> commas t1 <> " tokens"
    <> (if kept > 0 then ", " <> T.pack (show kept) <> " turn" <> (if kept /= 1 then "s" else "") <> " you had not read kept" else "")

receiptRejectedFit :: Integer -> Integer -> Text
receiptRejectedFit t limit = "context edit rejected: " <> commas t <> " tokens would exceed the limit of " <> commas limit <> "; the file was restored"

receiptRejectedShrink :: Integer -> Integer -> Text
receiptRejectedShrink t0 t1 = "context edit rejected: it did not reduce the context (" <> commas t0 <> " -> " <> commas t1 <> " tokens); the file was restored"

receiptNotUtf8 :: Text
receiptNotUtf8 = "context edit rejected: the file is not valid UTF-8; the file was restored"

receiptOnlyKept :: Int -> Text
receiptOnlyKept k = "context edit not applied: it only left out " <> T.pack (show k) <> " turn" <> (if k /= 1 then "s" else "") <> " you had not read, which were kept"

readout :: Integer -> Integer -> Text
readout t limit = "context " <> commas t <> " of " <> commas limit <> " tokens (" <> T.pack (show (percent t limit)) <> "%)"

readStub :: Text -> Text
readStub content = "[context file, " <> T.pack (show (T.length content)) <> " characters, shown once; read it again to see it now]"

-- | A result over L characters keeps its first and last floor(L / 2).
cutResult :: Integer -> Text -> Text
cutResult limit content
  | fromIntegral (T.length content) <= limit = content
  | otherwise =
      let h = fromIntegral (limit `div` 2)
       in T.take h content <> "\n[... " <> T.pack (show (T.length content - 2 * h)) <> " characters cut ...]\n" <> T.takeEnd h content

-- | A result that is a JSON object with an @error@ key.
isErrorResult :: Text -> Bool
isErrorResult r = case loads (TE.encodeUtf8 r) of
  Right d@(PDict _) -> maybe False (const True) (get "error" d)
  _ -> False

-- | Two spellings of one file (docs/clm.md section 3): repeated and @.@
-- segments collapsed, and macOS's @/private@ prefix on @/var@, @/tmp@ and
-- @/etc@ (where those are symlinks) dropped.
samePath :: Text -> Text -> Bool
samePath a b = normPath a == normPath b

normPath :: Text -> Text
normPath p0 =
  let collapse t = let t' = T.replace "/./" "/" (T.replace "//" "/" t) in if t' == t then t else collapse t'
      p = collapse p0
   in case T.stripPrefix "/private/" p of
        Just rest | any (`T.isPrefixOf` rest) ["var/", "tmp/", "etc/"] -> "/" <> rest
        _ -> p

-- | The spellings the file tools should treat as the same file.
pathAliases :: Text -> [Text]
pathAliases p =
  let n = normPath p
   in if any (`T.isPrefixOf` n) ["/var/", "/tmp/", "/etc/"] then [p, n, "/private" <> n] else [p, n]

-- | Whether a shell command or a piece of code names the context file's
-- directory (so the context file, its backup or state.json), in any
-- spelling (docs/clm.md section 3).
mentionsContext :: Text -> Text -> Bool
mentionsContext path t =
  let dir = T.dropEnd 1 (T.dropWhileEnd (/= '/') path)
   in not (T.null dir) && any (`T.isInfixOf` t) (pathAliases dir)

-- | The @command@ (terminal) or @code@ (execute_code) argument of a call.
cmdArg :: PyVal -> Maybe Text
cmdArg c = case get "function" c >>= get "arguments" of
  Just (PStr a) | Right d@(PDict _) <- loads (TE.encodeUtf8 a) -> case (get "command" d, get "code" d) of
    (Just (PStr t), _) -> Just t
    (_, Just (PStr t)) -> Just t
    _ -> Nothing
  _ -> Nothing

-- | The @path@ argument of a call's JSON arguments.
pathArg :: PyVal -> Maybe Text
pathArg c = case get "function" c >>= get "arguments" of
  Just (PStr a) | Right d@(PDict _) <- loads (TE.encodeUtf8 a), Just (PStr p) <- get "path" d -> Just p
  _ -> Nothing

-- | Section 6: a live turn's writes of the mirror keep only the lengths
-- of their texts.
shortenWrites :: Text -> PyVal -> PyVal
shortenWrites path m = case get "tool_calls" m of
  Just (PList cs) -> set "tool_calls" (PList (map one cs)) m
  _ -> m
  where
    one c =
      let name = maybe "" pyStrOf (get "function" c >>= get "name")
          ks = case name of
            "write_file" -> ["content"]
            "patch" -> ["old_string", "new_string", "patch"]
            _ -> []
       in case (ks, get "function" c, get "function" c >>= get "arguments") of
            (_ : _, Just fn, Just (PStr a))
              | Right d@(PDict kvs) <- loads (TE.encodeUtf8 a :: BS.ByteString)
              , get "path" d == Just (PStr path) ->
                  let kvs' = [(k, if k `elem` ks then shorten v else v) | (k, v) <- kvs]
                      shorten = \case
                        PStr t -> PStr ("[" <> T.pack (show (T.length t)) <> " characters]")
                        v -> v
                   in set "function" (set "arguments" (PStr (dumpsAscii (PDict kvs'))) fn) c
            _ -> c
