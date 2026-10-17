-- | What the agent shows while it works (@agent/display.py@): the one-line
-- preview of a tool call's main argument, the tool's emoji, the
-- completion line printed when a tool returns (with its failure suffix),
-- the kawaii faces and thinking verbs, and the spinner.
module Fisher.Agent.Display
  ( Skin (..)
  , defaultSkin
  , buildToolPreview
  , buildToolPreviewV
  , toolEmoji
  , detectToolFailure
  , cuteToolMessage
  , kawaiiWaiting
  , kawaiiThinking
  , thinkingVerbs
  , spinnerFrames
    -- * Spinner
  , Spinner
  , SpinnerOut (..)
  , startSpinner
  , stopSpinner
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (unless, when)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTime)

import Fisher.Py.Json (pyFormatFixed)
import Fisher.Py.Value
import Fisher.Tools.Py (pySplitWs, pyStrip)

-- | The skin settings the display reads.
data Skin = Skin
  { skToolPrefix :: Text
  , skToolEmojis :: [(Text, Text)]
  , skWings :: [(Text, Text)]
  }

defaultSkin :: Skin
defaultSkin = Skin "┊" [] []

kawaiiWaiting, kawaiiThinking, thinkingVerbs :: [Text]
kawaiiWaiting =
  ["(｡◕‿◕｡)", "(◕‿◕✿)", "٩(◕‿◕｡)۶", "(✿◠‿◠)", "( ˘▽˘)っ", "♪(´ε` )", "(◕ᴗ◕✿)", "ヾ(＾∇＾)", "(≧◡≦)", "(★ω★)"]
kawaiiThinking =
  [ "(｡•́︿•̀｡)", "(◔_◔)", "(¬‿¬)", "( •_•)>⌐■-■", "(⌐■_■)", "(´･_･`)", "◉_◉", "(°ロ°)", "( ˘⌣˘)♡", "ヽ(>∀<☆)☆"
  , "٩(๑❛ᴗ❛๑)۶", "(⊙_⊙)", "(¬_¬)", "( ͡° ͜ʖ ͡°)", "ಠ_ಠ" ]
thinkingVerbs =
  [ "pondering", "contemplating", "musing", "cogitating", "ruminating", "deliberating", "mulling", "reflecting"
  , "processing", "reasoning", "analyzing", "computing", "synthesizing", "formulating", "brainstorming" ]

spinnerFrames :: Text -> [Text]
spinnerFrames = \case
  "bounce" -> ["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]
  "grow" -> ["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█", "▇", "▆", "▅", "▄", "▃", "▂"]
  "arrows" -> ["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]
  "star" -> ["✶", "✷", "✸", "✹", "✺", "✹", "✸", "✷"]
  "moon" -> ["🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘"]
  "pulse" -> ["◜", "◠", "◝", "◞", "◡", "◟"]
  "brain" -> ["🧠", "💭", "💡", "✨", "💫", "🌟", "💡", "💭"]
  "sparkle" -> ["⁺", "˚", "*", "✧", "✦", "✧", "*", "˚"]
  _ -> ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

-- ------------------------------------------------------------- helpers

-- | @args.get(k, default)@
arg :: Text -> PyVal -> PyVal -> PyVal
arg k def args = maybe def id (get k args)

-- | @str(x)@
s :: PyVal -> Text
s = pyStrOf

argS :: Text -> Text -> PyVal -> Text
argS k def args = s (arg k (PStr def) args)

oneline :: Text -> Text
oneline = T.unwords . pySplitWs

pyLen :: PyVal -> Int
pyLen = \case
  PList xs -> length xs
  PDict kvs -> length kvs
  PStr t -> T.length t
  _ -> 0

-- | Python @str[:n]@ for the text form.
cut :: Int -> Text -> Text
cut = T.take

-- | @f"{x:9}"@ for a string: left-justified to 9.
pad9 :: Text -> Text
pad9 = T.justifyLeft 9 ' '

-- ------------------------------------------------------------- preview

-- | @build_tool_preview(name, args, max_len=40)@, as text.
buildToolPreview :: Text -> PyVal -> Maybe Text
buildToolPreview name args = pyStrOf <$> buildToolPreviewV name args

-- | The same, as the value Python returns: always a string, except for
-- @rl_select_environment@, which hands back its @name@ argument as given.
buildToolPreviewV :: Text -> PyVal -> Maybe PyVal
buildToolPreviewV name args
  | name == "rl_select_environment" && truthy args = Just (arg "name" (PStr "") args)
  | otherwise = PStr <$> preview name args

preview :: Text -> PyVal -> Maybe Text
preview name args
  | not (truthy args) = Nothing
  | name == "process" =
      let action = argS "action" "" args
          sid = argS "session_id" "" args
          dat = argS "data" "" args
          tout = arg "timeout" PNull args
          parts =
            [action]
              <> [cut 16 sid | not (T.null sid)]
              <> ["\"" <> oneline (cut 20 dat) <> "\"" | not (T.null dat)]
              <> [s tout <> "s" | truthy tout, action == "wait"]
       in Just (T.unwords parts)
  | name == "todo" = case arg "todos" PNull args of
      PNull -> Just "reading task list"
      t
        | truthy (arg "merge" (PBool False) args) -> Just ("updating " <> tshow (pyLen t) <> " task(s)")
        | otherwise -> Just ("planning " <> tshow (pyLen t) <> " task(s)")
  | name == "session_search" =
      let q = oneline (argS "query" "" args)
       in Just ("recall: \"" <> cut 25 q <> (if T.length q > 25 then "..." else "") <> "\"")
  | name == "memory" =
      let action = argS "action" "" args
          target = argS "target" "" args
       in Just $ case action of
            "add" ->
              let c = oneline (argS "content" "" args)
               in "+" <> target <> ": \"" <> cut 25 c <> (if T.length c > 25 then "..." else "") <> "\""
            "replace" -> "~" <> target <> ": \"" <> oneline (cut 20 (argS "old_text" "" args)) <> "\""
            "remove" -> "-" <> target <> ": \"" <> oneline (cut 20 (argS "old_text" "" args)) <> "\""
            _ -> action
  | name == "send_message" =
      let target = argS "target" "?" args
          m0 = oneline (argS "message" "" args)
          m = if T.length m0 > 20 then cut 17 m0 <> "..." else m0
       in Just ("to " <> target <> ": \"" <> m <> "\"")
  | "rl_" `T.isPrefixOf` name = case name of
      "rl_list_environments" -> Just "listing envs"
      "rl_select_environment" -> Just (argS "name" "" args)
      "rl_get_current_config" -> Just "reading config"
      "rl_edit_config" -> Just (argS "field" "" args <> "=" <> argS "value" "" args)
      "rl_start_training" -> Just "starting"
      "rl_check_status" -> Just (cut 16 (argS "run_id" "" args))
      "rl_stop_training" -> Just ("stopping " <> cut 16 (argS "run_id" "" args))
      "rl_get_results" -> Just (cut 16 (argS "run_id" "" args))
      "rl_list_runs" -> Just "listing runs"
      "rl_test_inference" -> Just (s (arg "num_steps" (PInt 3) args) <> " steps")
      _ -> Nothing
  | otherwise =
      let key = case lookup name primaryArgs of
            Just k -> Just k
            Nothing -> case [k | k <- ["query", "text", "command", "path", "name", "prompt", "code", "goal"], has k args] of
              (k : _) -> Just k
              [] -> Nothing
       in case key >>= \k -> get k args of
            Nothing -> Nothing
            Just v0 ->
              let v = case v0 of
                    PList (x : _) -> x
                    PList [] -> PStr ""
                    x -> x
                  p = oneline (s v)
               in if T.null p then Nothing else Just (if T.length p > 40 then cut 37 p <> "..." else p)
  where
    primaryArgs =
      [ ("terminal", "command"), ("web_search", "query"), ("web_extract", "urls"), ("read_file", "path")
      , ("write_file", "path"), ("patch", "path"), ("search_files", "pattern"), ("browser_navigate", "url")
      , ("browser_click", "ref"), ("browser_type", "text"), ("image_generate", "prompt"), ("text_to_speech", "text")
      , ("vision_analyze", "question"), ("mixture_of_agents", "user_prompt"), ("skill_view", "name")
      , ("skills_list", "category"), ("cronjob", "action"), ("execute_code", "code"), ("delegate_task", "goal")
      , ("clarify", "question"), ("skill_manage", "name") ]

-- | @get_tool_emoji@: the skin's override, the registry's emoji, else ⚡.
toolEmoji :: Skin -> (Text -> Maybe Text) -> Text -> Text
toolEmoji skin registry name = case lookup name (skToolEmojis skin) of
  Just e | not (T.null e) -> e
  _ -> case registry name of
    Just e | not (T.null e) -> e
    _ -> "⚡"

-- ------------------------------------------------------------- failure

-- | @_detect_tool_failure@: whether the result looks like a failure, and
-- the suffix the completion line gets.
detectToolFailure :: Text -> Maybe Text -> (Bool, Text)
detectToolFailure _ Nothing = (False, "")
detectToolFailure name (Just result)
  | name == "terminal" = case loads' result of
      Just d@(PDict _) -> case get "exit_code" d of
        Just PNull -> (False, "")
        Just (PInt 0) -> (False, "")
        Just (PFloat 0) -> (False, "")
        Just (PBool False) -> (False, "")
        Just v -> (True, " [exit " <> s v <> "]")
        Nothing -> (False, "")
      _ -> (False, "")
  | name == "memory"
  , Just d@(PDict _) <- loads' result
  , get "success" d == Just (PBool False)
  , Just (PStr e) <- get "error" d
  , "exceed the limit" `T.isInfixOf` e =
      (True, " [full]")
  | otherwise =
      let lower = T.toLower (T.take 500 result)
       in if "\"error\"" `T.isInfixOf` lower || "\"failed\"" `T.isInfixOf` lower || "Error" `T.isPrefixOf` result
            then (True, " [error]")
            else (False, "")
  where
    loads' t = either (const Nothing) Just (loadsText t)

loadsText :: Text -> Either String PyVal
loadsText = loads . TE.encodeUtf8

-- ------------------------------------------------------------- completion line

-- | @get_cute_tool_message(name, args, duration, result)@
cuteToolMessage :: Skin -> Text -> PyVal -> Double -> Maybe Text -> Text
cuteToolMessage skin name args duration result = wrap line
  where
    dur = pyFormatFixed 1 duration <> "s"
    (isFailure, suffix) = detectToolFailure name result
    wrap l =
      let l' = if skToolPrefix skin /= "┊" then replaceFirst "┊" (skToolPrefix skin) l else l
       in if isFailure then l' <> suffix else l'
    trunc n x = if T.length x > n then T.take (n - 3) x <> "..." else x
    path n x = if T.length x > n then "..." <> T.takeEnd (n - 3) x else x
    a k def = s (arg k (PStr def) args)
    domain u = head (T.splitOn "/" (T.replace "http://" "" (T.replace "https://" "" u)) <> [""])
    line = case name of
      "web_search" -> "┊ 🔍 search    " <> trunc 42 (a "query" "") <> "  " <> dur
      "web_extract" -> case arg "urls" (PList []) args of
        u | truthy u ->
          let url = case u of
                PList (x : _) -> s x
                x -> s x
              extra = if pyLen u > 1 then " +" <> tshow (pyLen u - 1) else ""
           in "┊ 📄 fetch     " <> trunc 35 (domain url) <> extra <> "  " <> dur
        _ -> "┊ 📄 fetch     pages  " <> dur
      "web_crawl" -> "┊ 🕸️  crawl     " <> trunc 35 (domain (a "url" "")) <> "  " <> dur
      "terminal" -> "┊ 💻 $         " <> trunc 42 (a "command" "") <> "  " <> dur
      "process" ->
        let action = a "action" "?"
            sid = T.take 12 (a "session_id" "")
            label = case action of
              "list" -> "ls processes"
              _ -> action <> " " <> sid
         in "┊ ⚙️  proc      " <> label <> "  " <> dur
      "read_file" -> "┊ 📖 read      " <> path 35 (a "path" "") <> "  " <> dur
      "write_file" -> "┊ ✍️  write     " <> path 35 (a "path" "") <> "  " <> dur
      "patch" -> "┊ 🔧 patch     " <> path 35 (a "path" "") <> "  " <> dur
      "search_files" ->
        let verb = if arg "target" (PStr "content") args == PStr "files" then "find" else "grep"
         in "┊ 🔎 " <> pad9 verb <> " " <> trunc 35 (a "pattern" "") <> "  " <> dur
      "browser_navigate" -> "┊ 🌐 navigate  " <> trunc 35 (domain (a "url" "")) <> "  " <> dur
      "browser_snapshot" -> "┊ 📸 snapshot  " <> (if truthy (arg "full" PNull args) then "full" else "compact") <> "  " <> dur
      "browser_click" -> "┊ 👆 click     " <> a "ref" "?" <> "  " <> dur
      "browser_type" -> "┊ ⌨️  type      \"" <> trunc 30 (a "text" "") <> "\"  " <> dur
      "browser_scroll" ->
        let d = a "direction" "down"
            arrow = case d of
              "up" -> "↑"
              "right" -> "→"
              "left" -> "←"
              _ -> "↓"
         in "┊ " <> arrow <> "  scroll    " <> d <> "  " <> dur
      "browser_back" -> "┊ ◀️  back      " <> dur
      "browser_press" -> "┊ ⌨️  press     " <> a "key" "?" <> "  " <> dur
      "browser_close" -> "┊ 🚪 close     browser  " <> dur
      "browser_get_images" -> "┊ 🖼️  images    extracting  " <> dur
      "browser_vision" -> "┊ 👁️  vision    analyzing page  " <> dur
      "todo" -> case arg "todos" PNull args of
        PNull -> "┊ 📋 plan      reading tasks  " <> dur
        t
          | truthy (arg "merge" (PBool False) args) -> "┊ 📋 plan      update " <> tshow (pyLen t) <> " task(s)  " <> dur
          | otherwise -> "┊ 📋 plan      " <> tshow (pyLen t) <> " task(s)  " <> dur
      "session_search" -> "┊ 🔍 recall    \"" <> trunc 35 (a "query" "") <> "\"  " <> dur
      "memory" ->
        let target = a "target" ""
         in case a "action" "?" of
              "add" -> "┊ 🧠 memory    +" <> target <> ": \"" <> trunc 30 (a "content" "") <> "\"  " <> dur
              "replace" -> "┊ 🧠 memory    ~" <> target <> ": \"" <> trunc 20 (a "old_text" "") <> "\"  " <> dur
              "remove" -> "┊ 🧠 memory    -" <> target <> ": \"" <> trunc 20 (a "old_text" "") <> "\"  " <> dur
              action -> "┊ 🧠 memory    " <> action <> "  " <> dur
      "skills_list" -> "┊ 📚 skills    list " <> a "category" "all" <> "  " <> dur
      "skill_view" -> "┊ 📚 skill     " <> trunc 30 (a "name" "") <> "  " <> dur
      "image_generate" -> "┊ 🎨 create    " <> trunc 35 (a "prompt" "") <> "  " <> dur
      "text_to_speech" -> "┊ 🔊 speak     " <> trunc 30 (a "text" "") <> "  " <> dur
      "vision_analyze" -> "┊ 👁️  vision    " <> trunc 30 (a "question" "") <> "  " <> dur
      "mixture_of_agents" -> "┊ 🧠 reason    " <> trunc 30 (a "user_prompt" "") <> "  " <> dur
      "send_message" -> "┊ 📨 send      " <> a "target" "?" <> ": \"" <> trunc 25 (a "message" "") <> "\"  " <> dur
      "cronjob" -> case a "action" "?" of
        "create" ->
          let skills = case arg "skills" PNull args of
                v | truthy v -> v
                _ -> if truthy (arg "skill" PNull args) then PList [arg "skill" PNull args] else PList []
              firstSkill = case skills of
                PList (x : _) -> Just x
                _ -> Nothing
              label = case arg "name" PNull args of
                v | truthy v -> s v
                _ -> case firstSkill of
                  Just v | truthy v -> s v
                  _ -> a "prompt" "task"
           in "┊ ⏰ cron      create " <> trunc 24 label <> "  " <> dur
        "list" -> "┊ ⏰ cron      listing  " <> dur
        action -> "┊ ⏰ cron      " <> action <> " " <> a "job_id" "" <> "  " <> dur
      _
        | "rl_" `T.isPrefixOf` name ->
            let label = case name of
                  "rl_list_environments" -> "list envs"
                  "rl_select_environment" -> "select " <> a "name" ""
                  "rl_get_current_config" -> "get config"
                  "rl_edit_config" -> "set " <> a "field" "?"
                  "rl_start_training" -> "start training"
                  "rl_check_status" -> "status " <> T.take 12 (a "run_id" "?")
                  "rl_stop_training" -> "stop " <> T.take 12 (a "run_id" "?")
                  "rl_get_results" -> "results " <> T.take 12 (a "run_id" "?")
                  "rl_list_runs" -> "list runs"
                  "rl_test_inference" -> "test inference"
                  _ -> T.replace "rl_" "" name
             in "┊ 🧪 rl        " <> label <> "  " <> dur
      "execute_code" ->
        let code = a "code" ""
            firstLine = if T.null (pyStrip code) then "" else head (T.splitOn "\n" (pyStrip code))
         in "┊ 🐍 exec      " <> trunc 35 firstLine <> "  " <> dur
      "delegate_task" -> case arg "tasks" PNull args of
        t@(PList (_ : _)) -> "┊ 🔀 delegate  " <> tshow (pyLen t) <> " parallel tasks  " <> dur
        _ -> "┊ 🔀 delegate  " <> trunc 35 (a "goal" "") <> "  " <> dur
      _ ->
        let pv = maybe "" id (buildToolPreview name args)
         in "┊ ⚡ " <> pad9 (T.take 9 name) <> " " <> trunc 35 pv <> "  " <> dur

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle rep hay = case T.breakOn needle hay of
  (a, b) | T.null b -> a
  (a, b) -> a <> rep <> T.drop (T.length needle) b

tshow :: Int -> Text
tshow = T.pack . show

-- ------------------------------------------------------------- spinner

-- | Where a spinner writes, and whether it animates (the agent's tests
-- turn the animation off: only what is left on the line counts).
data SpinnerOut = SpinnerOut
  { soWrite :: Text -> IO ()
  , soAnimate :: Bool
  }

data Spinner = Spinner
  { spOut :: SpinnerOut
  , spRunning :: IORef Bool
  , spDone :: MVar ()
  , spLastLen :: IORef Int
  }

-- | @KawaiiSpinner(message, spinner_type).start()@
startSpinner :: SpinnerOut -> Skin -> Text -> Text -> IO Spinner
startSpinner out skin message kind = do
  running <- newIORef True
  done <- newEmptyMVar
  lastLen <- newIORef 0
  let sp = Spinner out running done lastLen
      frames = spinnerFrames kind
  if not (soAnimate out)
    then putMVar done ()
    else do
      t0 <- getMonotonicTime
      _ <- forkIO $ do
        let loop i = do
              r <- readIORef running
              when r $ do
                now <- getMonotonicTime
                let frame = frames !! (i `mod` length frames)
                    el = pyFormatFixed 1 (now - t0)
                    line = case skWings skin of
                      [] -> "  " <> frame <> " " <> message <> " (" <> el <> "s)"
                      ws -> let (l, r') = ws !! (i `mod` length ws) in "  " <> l <> " " <> frame <> " " <> message <> " " <> r' <> " (" <> el <> "s)"
                prev <- readIORef lastLen
                soWrite out ("\r" <> line <> T.replicate (max (prev - T.length line) 0) " ")
                writeIORef lastLen (T.length line)
                threadDelay 120000
                loop (i + 1)
        loop (0 :: Int)
        putMVar done ()
      pure ()
  pure sp

-- | @spinner.stop(final)@
stopSpinner :: Spinner -> Maybe Text -> IO ()
stopSpinner sp final = do
  wasRunning <- atomicModifyIORef' (spRunning sp) (\r -> (False, r))
  unless (not wasRunning) $ readMVar (spDone sp)
  n <- readIORef (spLastLen sp)
  soWrite (spOut sp) ("\r" <> T.replicate (max (n + 5) 40) " " <> "\r")
  case final of
    Just f | not (T.null f) -> soWrite (spOut sp) ("  " <> f <> "\n")
    _ -> pure ()
