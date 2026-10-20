-- | A planned workflow child, run headless: the launch argv `fisher plan`
-- would hand to a terminal, turned into one that runs to the end on its
-- own, prints its events as JSON lines, and stops at a turn budget.
module Fisher.Search.Headless
  ( nativeBackend
  , attemptPrompt
  , headlessArgv
  , workflowCommand
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The native backend's name: Fisher's own agent (@fisher chat@).
nativeBackend :: Text
nativeBackend = "fisher"

-- | The workflow each attempt runs, on the whole file (the prompt narrows
-- it to one theorem): no planning, review, checkpoints, commits or golf.
workflowCommand :: Text -> FilePath -> Text
workflowCommand workflow file =
  "/" <> workflow <> " " <> T.pack file
    <> " --planning=off --review-source=none --checkpoint=false --commit=never --golf=never"

-- | What every attempt is told beyond the workflow's own prompt.
attemptPrompt :: Text -> FilePath -> Text
attemptPrompt goal file = T.unwords
  [ "This is an unattended proof-search attempt: nobody will answer questions or approve anything, so never stop to ask."
  , "Work only on the theorem " <> goal <> " in " <> T.pack file <> ": replace its sorry with a proof."
  , "Leave every other sorry and every other declaration as it is, and do not change the statement;"
  , "you may add helper lemmas above the theorem."
  , "Other attempts are working on the other sorries at the same time; theorems proved already can be used."
  , "Stop when it is proved or when you cannot make progress." ]

-- | The headless form of a planned child's argv, whose last element is
-- its prompt. claude-code: @-p@ with stream-json and @--max-turns@; fisher
-- (the native backend, @fisher chat ... -q PROMPT@): the same flags; codex: @codex exec --json@ (no turn
-- limit; the attempt's time limit bounds it).
headlessArgv :: Text -> [Text] -> Text -> Int -> Either Text [Text]
headlessArgv backend argv extra maxTurns = case unsnoc argv of
  Nothing -> Left "empty launch argv"
  Just (front, prompt0) ->
    let prompt = prompt0 <> " " <> extra
        turns = T.pack (show maxTurns)
     in case backend of
          "claude-code" -> Right (front <> ["-p", prompt, "--output-format", "stream-json", "--verbose", "--max-turns", turns])
          b | b == nativeBackend -> Right (dropQ front <> ["-p", prompt, "--output-format", "stream-json", "--max-turns", turns])
          "codex" -> case front of
            exe : rest -> Right ([exe, "exec", "--json"] <> rest <> [prompt])
            [] -> Left "empty launch argv"
          other -> Left ("no headless form for backend " <> other)
  where
    dropQ xs = case unsnoc xs of
      Just (ys, "-q") -> ys
      _ -> xs
    unsnoc xs = if null xs then Nothing else Just (init xs, last xs)
