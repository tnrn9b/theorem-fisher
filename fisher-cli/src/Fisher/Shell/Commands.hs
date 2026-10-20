-- | The fisher shell's slash commands: upstream's CLI class's @process_command@
-- (@cli.py@) for the commands Fisher has, over a 'ShellDeps' record so the
-- same code runs against the real swarm and against the tests' fakes.
module Fisher.Shell.Commands
  ( ShellDeps (..)
  , ShellState (..)
  , ProjectView (..)
  , ProjectInfo (..)
  , TaskRow (..)
  , Resolved (..)
  , newShellState
  , submit
  , processCommand
  , showHelp
  , showBanner
  , welcome
  , goodbye
  , swarmTable
  , swarmDetail
  , statusMarkup
  , onTaskDone
  , Out (..)
  , cprint
  , consolePrint
  , plainPrint
  ) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Rich.Render
import Fisher.Rich.Style (parseStyle)
import Fisher.Rich.Text (Justify (..), escapeMarkup)
import Fisher.Shell.Banner
import Fisher.Shell.Data
import Fisher.Shell.Rewrite
import Fisher.Skin

-- ------------------------------------------------------------------ deps

data ProjectView = ProjectView
  { pvLabel :: Text
  , pvRoot :: Text
  , pvLeanRoot :: Text
  , pvSummary :: Text -- ^ @format_project_summary@ for the active cwd
  }

-- | @_project_state@: the project, where it came from, why there is none.
data ProjectInfo = ProjectInfo
  { piProject :: Maybe ProjectView
  , piSource :: Maybe Text -- ^ "override" or "ambient"
  , piError :: Maybe Text
  }

-- | One swarm task as the shell shows it.
data TaskRow = TaskRow
  { trId :: Text
  , trDescription :: Text
  , trTheorem :: Text
  , trKind :: Text
  , trCommand :: Text
  , trProjectName :: Text
  , trProjectRoot :: Text
  , trWorkingDir :: Text
  , trBackend :: Text
  , trStatus :: Text -- ^ running, complete, queued, failed, cancelled
  , trStart :: Maybe Double
  , trEnd :: Maybe Double
  , trProgress :: Text
  , trLean :: Maybe Text
  , trSessionId :: Maybe Text
  , trResult :: Maybe Text
  , trError :: Maybe Text
  , trHasPty :: Bool
  }

-- | A resolved launch: what to call it, and the action that spawns it
-- (returning the task id).
data Resolved = Resolved
  { rKind :: Text
  , rWorkflowCommand :: Text
  , rInstruction :: Text
  , rCanonical :: Text
  , rProjectLabel :: Text
  , rBackend :: Text
  , rSpawn :: Text -> Text -> IO Text -- ^ theorem, description -> task id
  }

-- | Where text goes. 'outLine' writes one finished line.
newtype Out = Out {outLine :: Text -> IO ()}

data ShellDeps = ShellDeps
  { dOut :: Out
  , dWidth :: IO Int
  , dAnsi :: Bool
  , dUnicode :: Bool
  , dRenderTerminal :: Text -> Text -- ^ @render_terminal_text@ for _cprint and print
  , dFisherHome :: FilePath
  , dConfigPath :: FilePath
  , dProject :: IO ProjectInfo
  , dProjectCommand :: [Text] -> IO [Text]
  , dHandoffCwd :: IO Text -- ^ TERMINAL_CWD, else the process cwd
  , dEnv :: Text -> IO (Maybe Text)
  , dSetEnv :: Text -> Text -> IO ()
  , dSaveConfigValue :: [Text] -> Value -> IO Bool
  , dResolveWorkflow :: Text -> Text -> IO (Either Text Resolved)
  , dResolveChat :: Text -> Text -> IO (Either Text Resolved)
  , dNormalizeBackend :: Text -> Either Text Text
  , dBackends :: [Text]
  , dTasks :: IO [TaskRow]
  , dTask :: Text -> IO (Maybe TaskRow)
  , dCancel :: Text -> IO Bool
  , dAttach :: Text -> IO (Either Text ())
  , dWaitAttachable :: Text -> IO Bool
  , dNow :: IO Double
  , dClear :: IO ()
  , dNewSessionId :: IO Text
  , dStartedAt :: Text -- ^ the session start, @%Y-%m-%d %H:%M:%S@
  , dRunQuick :: Text -> IO (Either Text Text)
  , dForgiving :: Text -> Maybe Text -- ^ @rewrite_forgiving_managed_command@
  , dChat :: Text -> Text -> IO () -- ^ upstream's CLI class's @chat@: session id, message
  , dChatExit :: IO Bool -- ^ @_print_exit_summary@ when there was a conversation
  }

data ShellState = ShellState
  { ssChatMode :: IORef Bool
  , ssSkinName :: IORef Text
  , ssSkin :: IORef Skin
  , ssConfig :: IORef Value -- ^ @load_cli_config@'s result
  , ssSessionId :: IORef Text
  , ssCompact :: IORef Bool -- ^ @--compact@: the small banner
  }

newShellState :: ShellDeps -> Value -> Text -> IO ShellState
newShellState deps cfg sessionId = do
  let skinName = case at ["display", "skin"] cfg of
        Just (String s) | not (T.null (T.strip s)) -> T.strip s
        _ -> "default"
  skin <- loadSkin (dFisherHome deps) skinName
  ShellState <$> newIORef False <*> newIORef skinName <*> newIORef skin <*> newIORef cfg <*> newIORef sessionId <*> newIORef False

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup (K.fromText k) o >>= at ks
at _ _ = Nothing

atText :: [Text] -> Value -> Maybe Text
atText p v = case at p v of
  Just (String s) -> Just s
  _ -> Nothing

-- ---------------------------------------------------------------- output

-- | @_cprint@: raw text (with ANSI) through the terminal renderer, unwrapped.
cprint :: ShellDeps -> Text -> IO ()
cprint d t = mapM_ (outLine (dOut d)) (T.splitOn "\n" (dRenderTerminal d t))

-- | @print@.
plainPrint :: ShellDeps -> Text -> IO ()
plainPrint = cprint

-- | @ChatConsole().print(markup)@: Rich at the terminal width, trailing
-- blank lines dropped, each line through _cprint.
consolePrint :: ShellDeps -> Text -> IO ()
consolePrint d m = do
  w <- dWidth d
  let ls = chatLines (printLines w Nothing (markup m))
  mapM_ (cprint d . (if dAnsi d then lineAnsi else linePlain)) ls

printRendered :: ShellDeps -> [Line] -> IO ()
printRendered d = mapM_ (cprint d . (if dAnsi d then lineAnsi else linePlain))

dimA, boldA, rst :: ShellDeps -> Text
dimA d = if dAnsi d then "\ESC[2m" else ""
boldA d = if dAnsi d then "\ESC[1m" else ""
rst d = if dAnsi d then "\ESC[0m" else ""

notice :: ShellDeps -> Text -> IO ()
notice = consolePrint

-- ----------------------------------------------------------------- input

-- | @process_loop@'s handling of one submitted line. False means exit.
submit :: ShellDeps -> ShellState -> Text -> IO Bool
submit d s input
  | T.null input = pure True
  | otherwise = do
      let dispatched
            | "/" `T.isPrefixOf` input = Just input
            | otherwise = case rewriteFriendlyEntry input of
                Just c -> Just c
                Nothing -> dForgiving d input
      case dispatched of
        Just c -> do
          cprint d ("\n⚙️  " <> c)
          processCommand d s c
        Nothing -> do
          chat <- readIORef (ssChatMode s)
          info <- dProject d
          if not chat && isJustNothing (piProject info)
            then lockNotice d "/chat" >> pure True
            else do
              echoPlain d s input
              sid <- readIORef (ssSessionId s)
              dChat d sid input
              pure True
  where
    isJustNothing = maybe True (const False)

echoPlain :: ShellDeps -> ShellState -> Text -> IO ()
echoPlain d s input = do
  skin <- readIORef (ssSkin s)
  let accent = skinColor skin "ui_accent" "#FFBF00"
  plainPrint d ""
  if "\n" `T.isInfixOf` input
    then
      let ls = T.splitOn "\n" input
       in consolePrint d ("[bold " <> accent <> "]●[/] [bold]" <> escapeMarkup (head ls) <> "[/] [dim](+" <> T.pack (show (length ls - 1)) <> " lines)[/]")
    else consolePrint d ("[bold " <> accent <> "]●[/] [bold]" <> escapeMarkup input <> "[/]")

-- --------------------------------------------------------------- dispatch

lockAllowed :: [Text]
lockAllowed =
  [ "/chat", "/managed-chat", "/start", "/project", "/help", "/quit", "/exit", "/q", "/autoformalize-backend"
  , "/model", "/provider", "/config", "/clear", "/new", "/reset", "/swarm" ]

matchesCmd :: Text -> Text -> Bool
matchesCmd lower c = lower == c || (c <> " ") `T.isPrefixOf` lower

interactiveWorkflows, managedWorkflows :: [Text]
interactiveWorkflows = ["/prove", "/review", "/checkpoint", "/refactor", "/golf", "/formalize", "/draft"]
managedWorkflows = ["/autoformalize", "/auto-formalize", "/auto_formalize", "/autoprove", "/auto-proof", "/auto_proof"]

-- | Upstream commands that drive its own agent loop (plus those its
-- table does not list), answered with a pointer to the native agent.
agentNotices :: [Text]
agentNotices =
  agentOnlyCommands <> ["/prompt", "/personality", "/insights", "/browser", "/voice", "/plugins", "/tools", "/toolsets"]

processCommand :: ShellDeps -> ShellState -> Text -> IO Bool
processCommand d s command = do
  let original0 = pyStrip command
  original <- case rewriteFriendlySlash fisherCommands original0 of
    Just c | c /= original0 -> do
      cprint d ("  " <> dimA d <> "Assuming " <> head (T.words c) <> " from your input." <> rst d)
      pure c
    _ -> pure original0
  let lower = T.strip (T.toLower original)
  locked <- enforceLock d original
  if locked
    then pure True
    else dispatch d s original lower

dispatch :: ShellDeps -> ShellState -> Text -> Text -> IO Bool
dispatch d s original lower
  | lower `elem` ["/quit", "/exit", "/q"] = pure False
  | matchesCmd lower "/chat" = chatCommand d s original >> pure True
  | matchesCmd lower "/managed-chat" = managedChatCommand d s original >> pure True
  | matchesCmd lower "/start" = chatCommand d s ("/chat" <> T.drop 6 (T.strip original)) >> pure True
  | lower == "/help" = showHelp d s >> pure True
  | matchesCmd lower "/project" = do
      busy d (slowStatus original)
      out <- dProjectCommand d (drop 1 (T.words original))
      mapM_ (plainPrint d) out
      pure True
  | lower == "/config" = showCliConfig d s >> pure True
  | lower == "/clear" = clearCommand d s >> pure True
  | matchesCmd lower "/autoformalize-backend" = backendCommand d s original >> pure True
  | any (matchesCmd lower) interactiveWorkflows = workflow d s original True >> pure True
  | any (matchesCmd lower) managedWorkflows = workflow d s original False >> pure True
  | matchesCmd lower "/swarm" = swarmCommand d s original >> pure True
  | matchesCmd lower "/plan" = do
      notice d "[bold yellow]`/plan` is no longer part of the default Fisher workflow.[/] [dim]Use `/prove`, `/review`, `/checkpoint`, `/refactor`, `/golf`, `/autoprove`, `/formalize`, or `/autoformalize` for Lean work.[/]"
      pure True
  | "/cron" `T.isPrefixOf` lower = do
      notice d "[bold yellow]Cron automation is not part of the default Fisher surface.[/] [dim]Use the top-level `fisher cron` command only if you intentionally need the legacy workflow.[/]"
      pure True
  | "/skills" `T.isPrefixOf` lower = do
      notice d "[bold yellow]Bundled skills are not part of the default Fisher distribution.[/] [dim]Fisher stages `/prove`, `/review`, `/checkpoint`, `/refactor`, `/golf`, `/draft`, `/autoprove`, `/formalize`, and `/autoformalize` automatically.[/]"
      pure True
  | lower == "/reload-mcp" = do
      notice d "[bold yellow]User-managed MCP reload is not part of the default Fisher surface.[/] [dim]Managed Lean workflows use an isolated Lean MCP config automatically.[/]"
      pure True
  | matchesCmd lower "/handoff" = do
      notice d "[dim]`/handoff` now forwards to `/autoformalize`.[/]"
      workflow d s ("/autoformalize" <> T.drop 8 original) False
      pure True
  | "/background" `T.isPrefixOf` lower = do
      notice d "[bold yellow]`/background` is not part of the default Fisher workflow surface.[/] [dim]Stay in the current session or use `/prove`, `/review`, `/checkpoint`, `/refactor`, `/golf`, `/draft`, `/autoprove`, `/formalize`, or `/autoformalize` for Lean work.[/]"
      pure True
  | "/skin" `T.isPrefixOf` lower = skinCommand d s original >> pure True
  | Just c <- agentCommand lower = do
      notice d ("[bold yellow]`" <> c <> "` drives the native agent, which Fisher does not have yet.[/] [dim]Use `/managed-chat` for a managed Claude Code or Codex session, or the Lean workflow commands.[/]")
      pure True
  | otherwise = fallback d s original lower
  where
    agentCommand l = case T.words l of
      (w : _) | w `elem` agentNotices -> Just w
      _ -> Nothing

-- | Quick commands from config, then prefix matching, then unknown.
fallback :: ShellDeps -> ShellState -> Text -> Text -> IO Bool
fallback d s original lower = do
  cfg <- readIORef (ssConfig s)
  let base = head (T.words lower)
      name = T.dropWhile (== '/') base
  case at ["quick_commands", name] cfg of
    Just (Object q) -> do
      case KM.lookup "type" q of
        Just (String "exec") -> case KM.lookup "command" q of
          Just (String c) | not (T.null c) ->
            dRunQuick d c >>= \case
              Right out | not (T.null out) -> consolePrint d (escapeMarkup out)
              Right _ -> consolePrint d "[dim]Command returned no output[/]"
              Left "timeout" -> consolePrint d "[bold red]Quick command timed out (30s)[/]"
              Left e -> consolePrint d ("[bold red]Quick command error: " <> escapeMarkup e <> "[/]")
          _ -> consolePrint d ("[bold red]Quick command '" <> base <> "' has no command defined[/]")
        _ -> consolePrint d ("[bold red]Quick command '" <> base <> "' has unsupported type (only 'exec' is supported)[/]")
      pure True
    _ -> do
      let matches = filter (base `T.isPrefixOf`) fisherCommands
      case matches of
        [full]
          | full == base -> unknown >> pure True
          | otherwise -> processCommand d s (full <> T.drop (T.length base) (T.strip original))
        (_ : _ : _) -> do
          consolePrint d ("[bold yellow]Ambiguous command: " <> lower <> "[/]")
          consolePrint d ("[dim]Did you mean: " <> T.intercalate ", " (sort matches) <> "?[/]")
          pure True
        [] -> unknown >> pure True
  where
    unknown = do
      consolePrint d ("[bold red]Unknown command: " <> lower <> "[/]")
      consolePrint d "[dim #B8860B]Type /help for available commands[/]"

-- ------------------------------------------------------------- the lock

lockNotice :: ShellDeps -> Text -> IO ()
lockNotice d label = do
  info <- dProject d
  let detail0 = fromMaybe "No active Fisher project found." (piError info)
      detail = if piSource info == Just "override" then "Session override is invalid: " <> detail0 else detail0
  notice d ("[bold yellow]Fisher needs an active project before `" <> label <> "`.[/] [dim]Use `/project init`, `/project convert`, `/project create <path>`, or `/project use <path>` first.[/]")
  notice d "[dim]If you only want orientation first, run `/chat` for inline onboarding chat.[/]"
  notice d ("[dim]" <> detail <> "[/]")

enforceLock :: ShellDeps -> Text -> IO Bool
enforceLock d command
  | any (matchesCmd (T.strip (T.toLower command))) lockAllowed = pure False
  | otherwise = do
      info <- dProject d
      case piProject info of
        Just _ -> pure False
        Nothing -> do
          let label = case T.words command of
                (w : _) -> w
                [] -> "/chat"
          lockNotice d label
          pure True

-- ------------------------------------------------------------ busy line

slowStatus :: Text -> Text
slowStatus command
  | any (`T.isPrefixOf` c)
      [ "/autoformalize", "/auto_formalize", "/auto-formalize", "/chat", "/draft", "/formalize", "/prove", "/review"
      , "/checkpoint", "/refactor", "/golf", "/autoprove", "/auto-proof", "/auto_proof", "/handoff" ] =
      "Preparing managed Lean workflow session..."
  | "/project" `T.isPrefixOf` c = "Updating Fisher project state..."
  | "/skills" `T.isPrefixOf` c = "Processing skills command..."
  | c == "/reload-mcp" = "Reloading MCP servers..."
  | "/browser" `T.isPrefixOf` c = "Configuring browser..."
  | otherwise = "Processing command..."
  where
    c = T.strip (T.toLower command)

busy :: ShellDeps -> Text -> IO ()
busy d status = cprint d (dimA d <> (if dUnicode d then "⏳" else "...") <> " " <> status <> rst d)

-- ------------------------------------------------------------------ /chat

chatCommand :: ShellDeps -> ShellState -> Text -> IO ()
chatCommand d s cmd = do
  let payload0 = case T.words (T.strip cmd) of
        (_ : _) -> T.strip (T.drop 1 (T.dropWhile (/= ' ') (T.strip cmd)))
        [] -> ""
      lowered = T.toLower payload0
  case () of
    _
      | lowered == "status" -> do
          on <- readIORef (ssChatMode s)
          let st = if on then "on" else "off"
          notice d ("[dim]`/chat` is " <> st <> ". When it is on, plain text goes to the main interactive provider in the current Fisher session. Use `/chat off` to return to top-level command mode, or `/managed-chat` if you want the configured managed backend child session.[/]")
      | lowered `elem` ["off", "disable", "stop"] -> do
          writeIORef (ssChatMode s) False
          notice d "[dim]`/chat` is off. Plain text stays at the top-level Theorem Fisher prompt until you run `/chat` again.[/]"
      | otherwise -> do
          let payload = if lowered `elem` ["on", "enable", "start"] then "" else payload0
          writeIORef (ssChatMode s) True
          notice d "[bold green]`/chat` is on.[/] [dim]Plain text now goes to the main interactive provider even without an active Fisher project.[/]"
          notice d "[dim]Next: use `/project use <path>` for an existing Lean repo, `/project init` in the current repo, `/project create <path> --template-source <template-or-git-url>` for a new one, or `/managed-chat` if you want the configured managed backend child session instead.[/]"
          notice d "[dim]When the project is ready, run `/prove`, `/review`, `/draft`, `/autoprove`, or `/swarm`.[/]"
          unless (T.null payload) $ do
            notice d "[dim]`/chat` is sending your message to the main interactive provider.[/]"
            -- queued as the next plain input
            echoPlain d s payload
            sid <- readIORef (ssSessionId s)
            dChat d sid payload

activeBackend :: ShellDeps -> ShellState -> IO Text
activeBackend d s = do
  env <- dEnv d "FISHER_AUTOFORMALIZE_BACKEND"
  cfg <- readIORef (ssConfig s)
  let raw = case env of
        Just e | not (T.null e) -> e
        _ -> case atText ["fisher", "autoformalize", "backend"] cfg of
          Just b | not (T.null b) -> b
          _ -> head (dBackends d)
  pure (either (const raw) id (dNormalizeBackend d raw))

managedChatCommand :: ShellDeps -> ShellState -> Text -> IO ()
managedChatCommand d s cmd = do
  let payload0 = T.strip (T.drop 1 (T.dropWhile (/= ' ') (T.strip cmd)))
      lowered = T.toLower payload0
  case () of
    _
      | lowered == "status" -> do
          b <- activeBackend d s
          notice d ("[dim]`/managed-chat` opens the configured managed backend child session (`" <> b <> "`). Use `/autoformalize-backend` to switch backends, or `/chat` for inline onboarding in the current Fisher session.[/]")
      | lowered `elem` ["off", "disable", "stop"] ->
          notice d "[dim]`/managed-chat` is not a persistent mode. Run `/chat` if you want inline onboarding in the current Fisher session.[/]"
      | otherwise -> managedChat d s "/managed-chat" (if lowered `elem` ["on", "enable", "start"] then "" else payload0)

-- | Spawn a managed chat child and attach, or say how to.
managedChat :: ShellDeps -> ShellState -> Text -> Text -> IO ()
managedChat d _s statusFor payload = do
  busy d (slowStatus statusFor)
  cwd <- dHandoffCwd d
  dResolveChat d payload cwd >>= \case
    Left e -> plainPrint d ("(>_<) " <> e)
    Right r -> do
      let desc = if T.null (T.strip payload) then "managed chat" else T.strip payload
      tid <- rSpawn r desc desc
      attachOrSay d tid
        ("[dim]Spawned managed chat session " <> tid <> " (" <> rBackend r <> "): " <> desc <> "[/]")

attachOrSay :: ShellDeps -> Text -> Text -> IO ()
attachOrSay d tid spawnedMsg = do
  ready <- dWaitAttachable d tid
  if ready
    then attach d (Just tid)
    else do
      consolePrint d spawnedMsg
      consolePrint d ("[dim]Use `/swarm attach " <> tid <> "` to connect. `/swarm` for status. Ctrl-] to detach.[/]")

-- ------------------------------------------------------------- workflows

workflow :: ShellDeps -> ShellState -> Text -> Bool -> IO ()
workflow d _s cmd interactive = do
  busy d (slowStatus cmd)
  info <- dProject d
  cwd <- maybe (dHandoffCwd d) (pure . pvRoot) (piProject info)
  dResolveWorkflow d cmd cwd >>= \case
    Left e -> plainPrint d ("(>_<) " <> e)
    Right r -> do
      let headline = T.strip (T.take 60 (rInstruction r))
          desc = if T.null headline then T.dropWhile (== '/') (rCanonical r) else headline
          theorem = if T.null (rInstruction r) then rWorkflowCommand r else rInstruction r
      tid <- rSpawn r theorem desc
      if interactive
        then attachOrSay d tid ("[dim]Spawned " <> rKind r <> " agent " <> tid <> ": " <> desc <> "[/]")
        else do
          consolePrint d ("[dim]Spawned " <> rKind r <> " agent " <> tid <> " for " <> rProjectLabel r <> ": " <> desc <> "[/]")
          consolePrint d ("[dim]Use `/swarm attach " <> tid <> "` to connect. `/swarm` for status. Ctrl-] to detach.[/]")

-- | The completion notice (@_setup_swarm_completion_callback@).
onTaskDone :: ShellDeps -> ShellState -> TaskRow -> IO ()
onTaskDone d s t = do
  skin <- readIORef (ssSkin s)
  let ok = skinColor skin "ui_ok" "#8B9B7A"
      err = skinColor skin "ui_error" "#B07070"
      dimC = skinColor skin "banner_dim" "#6B6B60"
  case trStatus t of
    "complete" -> do
      consolePrint d ("[" <> ok <> "]● Swarm task " <> trId t <> " complete[/] [" <> dimC <> "](" <> trDescription t <> ")[/]")
      forM_ (trResult t) $ \r -> unless (T.null r) (consolePrint d ("[" <> dimC <> "]" <> T.take 200 r <> "[/]"))
    "failed" -> do
      consolePrint d ("[" <> err <> "]● Swarm task " <> trId t <> " failed[/] [" <> dimC <> "](" <> trDescription t <> ")[/]")
      forM_ (trError t) $ \r -> unless (T.null r) (consolePrint d ("[" <> err <> "]" <> T.take 200 r <> "[/]"))
    _ -> pure ()

-- ----------------------------------------------------------------- /swarm

statusMarkup :: Skin -> Text -> Text
statusMarkup skin = \case
  "complete" -> "[" <> c "ui_ok" "#8B9B7A" <> "]● done[/]"
  "running" -> "[" <> c "ui_accent" "#C8C0B0" <> "]● active[/]"
  "queued" -> "[" <> c "banner_dim" "#6B6B60" <> "]○ queued[/]"
  "failed" -> "[" <> c "ui_error" "#B07070" <> "]● failed[/]"
  "cancelled" -> "[" <> c "banner_dim" "#6B6B60" <> "]● cancel[/]"
  other -> other
  where
    c = skinColor skin

formatElapsed :: Double -> Text
formatElapsed secs
  | secs < 60 = T.pack (show (truncate secs :: Int)) <> "s"
  | secs < 3600 = T.pack (show (truncate (secs / 60) :: Int)) <> "m"
  | otherwise =
      let h = truncate (secs / 3600) :: Int
          m = truncate ((secs - fromIntegral h * 3600) / 60) :: Int
       in T.pack (show h) <> "h" <> T.pack (show m) <> "m"

elapsedOf :: Double -> TaskRow -> Text
elapsedOf now t = case trStart t of
  Nothing -> "—"
  Just st -> formatElapsed (fromMaybe now (trEnd t) - st)

projectOf :: TaskRow -> Text
projectOf t
  | not (T.null (trProjectName t)) = trProjectName t
  | not (T.null (trProjectRoot t)) = last (T.splitOn "/" (trProjectRoot t))
  | otherwise = "—"

-- | @SwarmManager.render_table@, titled for Fisher.
swarmTable :: Skin -> Double -> [TaskRow] -> Table
swarmTable skin now rows =
  let border = skinColor skin "banner_border" "#5B6B4F"
      titleC = skinColor skin "banner_title" "#C8C0B0"
      textC = parseStyle (skinColor skin "banner_text" "#C8C0B0")
      dimS = parseStyle (skinColor skin "banner_dim" "#6B6B60")
      dimC = skinColor skin "banner_dim" "#6B6B60"
      cols =
        [ (column "ID") {cStyle = textC, cNoWrap = True}
        , (column "Workflow") {cStyle = textC, cMaxWidth = Just 26}
        , (column "Project") {cStyle = textC, cMaxWidth = Just 18}
        , (column "Status") {cNoWrap = True}
        , (column "Time") {cStyle = dimS, cNoWrap = True, cJustify = JRight}
        , (column "Lean") {cStyle = textC, cNoWrap = True}
        , (column "Progress") {cStyle = dimS} ]
      row t =
        let wf0 = if T.null (trKind t) then "workflow" else trKind t
            wf = if T.null (trDescription t) then wf0 else wf0 <> ": " <> trDescription t
            prog = trProgress t <> (if trHasPty t && trStatus t == "running" then " [" <> dimC <> "]› attach[/]" else "")
         in map markup [trId t, wf, projectOf t, statusMarkup skin (trStatus t), elapsedOf now t, fromMaybe "—" (trLean t), prog]
   in (table cols (map row rows))
        { tbTitle = Just ("[bold " <> titleC <> "]Fisher Workflow Swarm[/]")
        , tbBorderStyle = parseStyle border
        , tbPadEdge = True }

-- | @SwarmManager.render_detail@.
swarmDetail :: Skin -> Double -> TaskRow -> Table
swarmDetail skin now t =
  let border = skinColor skin "banner_border" "#5B6B4F"
      titleC = skinColor skin "banner_title" "#C8C0B0"
      textC = parseStyle (skinColor skin "banner_text" "#C8C0B0")
      dimC = skinColor skin "banner_dim" "#6B6B60"
      ok = skinColor skin "ui_ok" "#8B9B7A"
      errC = skinColor skin "ui_error" "#B07070"
      opt k v = [(k, v) | not (T.null v)]
      rows =
        [("ID", trId t), ("Description", trDescription t), ("Theorem", trTheorem t)]
          <> opt "Workflow" (trKind t)
          <> opt "Command" (trCommand t)
          <> opt "Backend" (trBackend t)
          <> ( if not (T.null (trProjectName t))
                 then [("Project", trProjectName t)]
                 else [("Project", last (T.splitOn "/" (trProjectRoot t))) | not (T.null (trProjectRoot t))] )
          <> opt "Project Root" (trProjectRoot t)
          <> opt "Working Dir" (trWorkingDir t)
          <> [ ("Status", statusMarkup skin (trStatus t)), ("Elapsed", elapsedOf now t)
             , ("Lean", fromMaybe "—" (trLean t)), ("Progress", trProgress t) ]
          <> maybe [] (\x -> [("Session", x) | not (T.null x)]) (trSessionId t)
          <> [("Attach", "[" <> ok <> "]/swarm attach " <> trId t <> "[/]  (Ctrl-] to detach)") | trHasPty t && trStatus t == "running"]
          <> maybe [] (\x -> [("Result", x) | not (T.null x)]) (trResult t)
          <> maybe [] (\x -> [("Error", "[" <> errC <> "]" <> x <> "[/]") | not (T.null x)]) (trError t)
      cols = [(column "Key") {cStyle = parseStyle ("bold " <> dimC), cNoWrap = True}, (column "Value") {cStyle = textC}]
   in (table cols [[markup k, markup v] | (k, v) <- rows])
        { tbTitle = Just ("[bold " <> titleC <> "]Task " <> trId t <> "[/]")
        , tbBorderStyle = parseStyle border
        , tbShowHeader = False
        , tbPadEdge = True }

swarmCommand :: ShellDeps -> ShellState -> Text -> IO ()
swarmCommand d s cmd = do
  let parts = T.words (T.strip cmd)
  skin <- readIORef (ssSkin s)
  now <- dNow d
  w <- dWidth d
  let printTable tb = printRendered d (chatLines (printLines w Nothing (RTable tb)))
  case parts of
    [_] -> do
      ts <- dTasks d
      if null ts
        then consolePrint d "[dim]No managed workflow tasks. Use `/prove`, `/review`, `/checkpoint`, `/refactor`, `/golf`, `/draft`, `/autoprove`, `/formalize`, or `/autoformalize` to spawn one.[/]"
        else printTable (swarmTable skin now ts)
    (_ : sub : rest) -> do
      let subL = T.toLower sub
      case (subL, rest) of
        ("cancel", tid : _) -> do
          ok <- dCancel d tid
          consolePrint d (if ok then "[dim]Cancelled task " <> tid <> ".[/]" else "[dim]Cannot cancel " <> tid <> " (not found or already finished).[/]")
        ("attach", _) -> attach d (case rest of (x : _) -> Just x; [] -> Nothing)
        _ -> do
          t1 <- dTask d subL
          t2 <- maybe (dTask d sub) (pure . Just) t1
          case t2 of
            Just t -> printTable (swarmDetail skin now t)
            Nothing -> consolePrint d "[dim]Usage: /swarm [task-id] | /swarm attach [task-id] | /swarm cancel <task-id>[/]"
    [] -> pure ()

-- | @_attach_to_swarm_task@.
attach :: ShellDeps -> Maybe Text -> IO ()
attach d given = do
  target <- case given of
    Just tid -> pure (Just tid)
    Nothing -> do
      ts <- dTasks d
      pure (case reverse [t | t <- ts, trStatus t == "running", trHasPty t] of
        (t : _) -> Just (trId t)
        [] -> Nothing)
  case target of
    Nothing -> consolePrint d "[dim]No running interactive swarm task to attach to.[/]"
    Just tid ->
      dTask d tid >>= \case
        Nothing -> consolePrint d ("[dim]Task " <> tid <> " not found.[/]")
        Just t
          | trStatus t /= "running" -> consolePrint d ("[dim]Task " <> tid <> " is not running (status: " <> trStatus t <> ").[/]")
          | not (trHasPty t) -> consolePrint d ("[dim]Task " <> tid <> " has no interactive session to attach to.[/]")
          | otherwise -> do
              plainPrint d ("[swarm] Attaching to " <> tid <> " — press Ctrl-] to detach.")
              r <- dAttach d tid
              case r of
                Left e -> plainPrint d ("[swarm] Attach error: " <> e)
                Right () -> pure ()
              plainPrint d ("\n[swarm] Detached from " <> tid <> ".")

-- --------------------------------------------------- /autoformalize-backend

backendCommand :: ShellDeps -> ShellState -> Text -> IO ()
backendCommand d s cmd = do
  let arg = T.strip (T.drop 1 (T.dropWhile (/= ' ') (T.strip cmd)))
      avail = T.intercalate ", " (dBackends d)
  active <- activeBackend d s
  cfg <- readIORef (ssConfig s)
  if T.null arg
    then do
      let configured0 = fromMaybe (head (dBackends d)) (atText ["fisher", "autoformalize", "backend"] cfg)
          configured = either (const configured0) id (dNormalizeBackend d configured0)
      env <- dEnv d "FISHER_AUTOFORMALIZE_BACKEND"
      plainPrint d ("  Autoformalize backend: " <> active)
      plainPrint d ("  Config backend:        " <> configured)
      when (maybe False (not . T.null . T.strip) env) $
        plainPrint d "  Session override:     FISHER_AUTOFORMALIZE_BACKEND is set for this running CLI"
      plainPrint d ("  Available:            " <> avail)
      plainPrint d "  Usage: /autoformalize-backend <backend>"
    else case dNormalizeBackend d arg of
      Left e -> plainPrint d ("(>_<) " <> e) >> plainPrint d ("  Available: " <> avail)
      Right b -> do
        modifyIORef' (ssConfig s) (setPath ["fisher", "autoformalize", "backend"] (String b))
        dSetEnv d "FISHER_AUTOFORMALIZE_BACKEND" b
        saved <- dSaveConfigValue d ["fisher", "autoformalize", "backend"] (String b)
        plainPrint d $
          if saved
            then "(^_^)b Autoformalize backend set to: " <> b <> " (saved to config; active now)"
            else "(^_^) Autoformalize backend set to: " <> b <> " (this session only)"
        plainPrint d "  The next managed Lean workflow run will use this backend."

setPath :: [Text] -> Value -> Value -> Value
setPath [] v _ = v
setPath (k : ks) v (Object o) =
  let inner = case KM.lookup (K.fromText k) o of
        Just x@(Object _) -> x
        _ -> Object KM.empty
   in Object (KM.insert (K.fromText k) (if null ks then v else setPath ks v inner) o)
setPath ks v _ = setPath ks v (Object KM.empty)

-- ------------------------------------------------------------------ /skin

skinCommand :: ShellDeps -> ShellState -> Text -> IO ()
skinCommand d s cmd = do
  let arg = T.strip (T.drop 1 (T.dropWhile (/= ' ') (T.strip cmd)))
  entries <- listSkins (dFisherHome d)
  if T.null arg
    then do
      current <- readIORef (ssSkinName s)
      plainPrint d ("\n  Current skin: " <> current)
      plainPrint d "  Available skins:"
      forM_ entries $ \e -> do
        let marker = if seName e == current then " ●" else "  "
            source = if seUser e then " (user)" else ""
        plainPrint d ("   " <> marker <> " " <> seName e <> source <> " — " <> seDescription e)
      plainPrint d "\n  Usage: /skin <name>"
      plainPrint d ("  Custom skins: drop a YAML file in " <> T.pack (dFisherHome d) <> "/skins/\n")
    else do
      let new = T.toLower arg
          names = map seName entries
      if new `notElem` names
        then do
          plainPrint d ("  Unknown skin: " <> new)
          plainPrint d ("  Available: " <> T.intercalate ", " (sort (dedupe names)))
        else do
          skin <- loadSkin (dFisherHome d) new
          writeIORef (ssSkinName s) new
          writeIORef (ssSkin s) skin
          modifyIORef' (ssConfig s) (setPath ["display", "skin"] (String new))
          saved <- dSaveConfigValue d ["display", "skin"] (String new)
          plainPrint d (if saved then "  Skin set to: " <> new <> " (saved)" else "  Skin set to: " <> new)
          plainPrint d "  Note: banner colors will update on next session start."
  where
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ------------------------------------------------------------------ /help

showHelp :: ShellDeps -> ShellState -> IO ()
showHelp d s = do
  skin <- readIORef (ssSkin s)
  let header0 = T.strip (skinBranding skin "help_header" "(^_^)? Available Commands")
      header1 = if T.null header0 then "(^_^)? Available Commands" else header0
      header = T.take 55 header1
      left = (55 - T.length header) `div` 2
      centred = T.replicate left " " <> header <> T.replicate (55 - T.length header - left) " "
      accent = skinColor skin "ui_accent" "#FFBF00"
  cprint d ("\n" <> boldA d <> "+" <> T.replicate 55 "-" <> "+" <> rst d)
  cprint d (boldA d <> "|" <> centred <> "|" <> rst d)
  cprint d (boldA d <> "+" <> T.replicate 55 "-" <> "+" <> rst d)
  info <- dProject d
  case piProject info of
    Just p -> cprint d ("\n  " <> dimA d <> "Active project: " <> summaryOf info p <> rst d)
    Nothing -> cprint d ("\n  " <> dimA d <> "No active project — use /chat for inline onboarding, or /project init, /project convert, /project create <path>, or /project use <path>." <> rst d)
  forM_ fisherCommandTable $ \(cat, cmds) -> do
    cprint d ("\n  " <> boldA d <> "── " <> cat <> " ──" <> rst d)
    forM_ cmds $ \(c, desc) ->
      consolePrint d ("    [bold " <> accent <> "]" <> T.justifyLeft 15 ' ' c <> "[/] [dim]-[/] " <> escapeMarkup desc)
  cprint d ("\n  " <> dimA d <> "Tip: Start with /chat for inline onboarding if you want orientation first. Use /project when you're ready to work in a Lean repo, then launch /prove, /review, /checkpoint, /refactor, /golf, /draft, /autoprove, /formalize, or /autoformalize." <> rst d)
  cprint d ("  " <> dimA d <> "Multi-line: Alt+Enter for a new line" <> rst d)
  cprint d ("  " <> dimA d <> "Paste image: Alt+V (or /paste)" <> rst d <> "\n")

summaryOf :: ProjectInfo -> ProjectView -> Text
summaryOf info p = pvSummary p <> (if piSource info == Just "override" then " [override]" else "")

-- ---------------------------------------------------------------- /config

showCliConfig :: ShellDeps -> ShellState -> IO ()
showCliConfig d s = do
  cfg <- readIORef (ssConfig s)
  termEnv <- fromMaybe "local" <$> dEnv d "TERMINAL_ENV"
  termCwd <- dHandoffCwd d
  timeout <- fromMaybe "60" <$> dEnv d "TERMINAL_TIMEOUT"
  baseUrl <- baseUrlOf d cfg
  apiKey <- apiKeyOf d baseUrl
  maxTurns <- maxTurnsOf d cfg
  let model = modelOf cfg
      keyShown = case apiKey of
        Just k | T.length k > 4 -> "********" <> T.takeEnd 4 k
        _ -> "Not set!"
      title = "(^_^) Configuration"
      pad = 50 - T.length title
      handoff = case at ["cli", "handoff"] cfg of
        Just (Object o) -> case KM.lookup "mode" o of
          Just (String m) -> m
          Just other -> T.pack (show other)
          Nothing -> "auto"
        _ -> "auto"
      p = plainPrint d
  p ""
  p ("+" <> T.replicate 50 "-" <> "+")
  p ("|" <> T.replicate (pad `div` 2) " " <> title <> T.replicate (pad - pad `div` 2) " " <> "|")
  p ("+" <> T.replicate 50 "-" <> "+")
  p ""
  p "  -- Model --"
  p ("  Model:     " <> model)
  p ("  Base URL:  " <> baseUrl)
  p ("  API Key:   " <> keyShown)
  p ""
  p "  -- Terminal --"
  p ("  Environment:  " <> termEnv)
  when (termEnv == "ssh") $ do
    h <- fromMaybe "not set" <$> dEnv d "TERMINAL_SSH_HOST"
    u <- fromMaybe "not set" <$> dEnv d "TERMINAL_SSH_USER"
    port <- fromMaybe "22" <$> dEnv d "TERMINAL_SSH_PORT"
    p ("  SSH Target:   " <> u <> "@" <> h <> ":" <> port)
  p ("  Working Dir:  " <> termCwd)
  p ("  Timeout:      " <> timeout <> "s")
  p ("  Handoff:      " <> handoff)
  p ""
  p "  -- Agent --"
  p ("  Max Turns:  " <> maxTurns)
  p "  Toolsets:   all"
  p "  Verbose:    False"
  p ""
  p "  -- Session --"
  p ("  Started:     " <> dStartedAt d)
  p ("  Config File: " <> T.pack (dConfigPath d) <> " (loaded)")
  info <- dProject d
  p ""
  p "  -- Project --"
  case piProject info of
    Nothing -> do
      p "  Active:     (none)"
      p ("  Detail:     " <> fromMaybe "No active Fisher project found" (piError info))
    Just pv -> do
      p ("  Active:     " <> pvLabel pv <> " [" <> fromMaybe "" (piSource info) <> "]")
      p ("  Root:       " <> pvRoot pv)
      p ("  Lean Root:  " <> pvLeanRoot pv)
  p ""

modelOf :: Value -> Text
modelOf cfg = case at ["model"] cfg of
  Just (Object o) | Just (String m) <- KM.lookup "default" o, not (T.null m) -> m
  Just (String m) | not (T.null m) -> m
  _ -> "anthropic/claude-opus-4.6"

baseUrlOf :: ShellDeps -> Value -> IO Text
baseUrlOf d cfg = do
  a <- dEnv d "OPENAI_BASE_URL"
  b <- dEnv d "OPENROUTER_BASE_URL"
  let fromCfg = fromMaybe "https://openrouter.ai/api/v1" (atText ["model", "base_url"] cfg)
  pure $ case (a, b) of
    (Just x, _) | not (T.null x) -> x
    (_, Just y) -> y
    _ -> fromCfg

apiKeyOf :: ShellDeps -> Text -> IO (Maybe Text)
apiKeyOf d url = do
  orKey <- dEnv d "OPENROUTER_API_KEY"
  oaKey <- dEnv d "OPENAI_API_KEY"
  let pick xs = case [x | Just x <- xs, not (T.null x)] of
        (x : _) -> Just x
        [] -> Nothing
  pure (if "openrouter.ai" `T.isInfixOf` url then pick [orKey, oaKey] else pick [oaKey, orKey])

maxTurnsOf :: ShellDeps -> Value -> IO Text
maxTurnsOf d cfg = do
  env <- dEnv d "FISHER_MAX_ITERATIONS"
  pure $ case (at ["agent", "max_turns"] cfg, at ["max_turns"] cfg, env) of
    (Just v, _, _) | truthy v -> showV v
    (_, Just v, _) | truthy v -> showV v
    (_, _, Just e) | not (T.null e) -> e
    _ -> "90"
  where
    truthy = \case
      Null -> False
      Bool b -> b
      Number n -> n /= 0
      String t -> not (T.null t)
      _ -> True
    showV = \case
      Number n -> let t = T.pack (show n) in fromMaybe t (T.stripSuffix ".0" t)
      String t -> t
      other -> T.pack (show other)

-- ----------------------------------------------------------------- /clear

clearCommand :: ShellDeps -> ShellState -> IO ()
clearCommand d s = do
  sid <- dNewSessionId d
  writeIORef (ssSessionId s) sid
  dClear d
  showBanner d s
  cprint d "  ✨ (◕‿◕)✨ Fresh start! Screen cleared and conversation reset.\n"

-- | The welcome banner (compact on very narrow terminals).
showBanner :: ShellDeps -> ShellState -> IO ()
showBanner d s = do
  w <- dWidth d
  skin <- readIORef (ssSkin s)
  compact <- readIORef (ssCompact s)
  if compact || w < 34
    then printRendered d (compactBanner w (dUnicode d) skin)
    else do
      cfg <- readIORef (ssConfig s)
      cwd <- dHandoffCwd d
      info <- dProject d
      sid <- readIORef (ssSessionId s)
      ts <- dTasks d
      let summary = summaryLineFor skin ts
      printRendered d $
        welcomeBanner
          BannerInput
            { biWidth = w, biUnicode = dUnicode d, biSkin = skin, biModel = modelOf cfg, biCwd = cwd
            , biProject = fmap (summaryOf info) (piProject info), biSession = Just sid
            , biSwarmLine = summary, biBehind = Nothing }

-- | @SwarmManager.summary_line@.
summaryLineFor :: Skin -> [TaskRow] -> Maybe Text
summaryLineFor skin ts
  | null parts = Nothing
  | otherwise = Just ("[" <> dimC <> "]Swarm:[/] " <> T.intercalate (" [" <> dimC <> "]·[/] ") parts)
  where
    count st = length [() | t <- ts, trStatus t == st]
    ok = skinColor skin "ui_ok" "#8B9B7A"
    accent = skinColor skin "ui_accent" "#C8C0B0"
    dimC = skinColor skin "banner_dim" "#6B6B60"
    err = skinColor skin "ui_error" "#B07070"
    running = count "running"
    parts =
      [ "[" <> accent <> "]" <> T.pack (show running) <> " " <> (if running == 1 then "agent" else "agents") <> " running[/]" | running > 0 ]
        <> [ "[" <> ok <> "]" <> T.pack (show (count "complete")) <> " complete[/]" | count "complete" > 0 ]
        <> [ "[" <> dimC <> "]" <> T.pack (show (count "queued")) <> " queued[/]" | count "queued" > 0 ]
        <> [ "[" <> err <> "]" <> T.pack (show (count "failed")) <> " failed[/]" | count "failed" > 0 ]

-- | The line after the banner.
welcome :: ShellDeps -> ShellState -> IO ()
welcome d s = do
  skin <- readIORef (ssSkin s)
  let text = skinBranding skin "welcome" "Welcome to Fisher! Type /chat for inline onboarding, /project for project setup, or /help for commands."
      colour = skinColor skin "banner_text" "#FFF8DC"
  consolePrint d ("[" <> colour <> "]" <> text <> "[/]")
  consolePrint d ""

-- | @_print_exit_summary@: the native session's summary, or with no
-- conversation a blank line and the skin's goodbye.
goodbye :: ShellDeps -> ShellState -> IO ()
goodbye d s = do
  summarised <- dChatExit d
  unless summarised $ do
    skin <- readIORef (ssSkin s)
    plainPrint d ""
    plainPrint d (skinBranding skin "goodbye" "Goodbye! @")
