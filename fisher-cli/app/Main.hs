-- | The fisher executable.
module Main (main) where


import Control.Exception (IOException, catch, try)
import Data.IORef (readIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, hGetEcho, hSetEcho, stderr, stdin, stdout)
import System.IO.Error (isEOFError)

import Fisher.Cli.ConfigCmd
import Fisher.Cli.Check (checkMain)
import Fisher.Cli.Search (searchMain)
import Fisher.Cli.Sessions (SessionsCmd (..), sessionsParser)
import Fisher.Cli.Subcommands
import Fisher.Shell (ShellOptions (..), runShell)
import Fisher.Cli.Launch
import Fisher.Cli.Session
import Fisher.Config (loadConfigAt)
import Fisher.Config.Migrate (Prompter (..))
import Fisher.Config.Set (setConfigValueAt)
import Fisher.Doctor
import Fisher.Lean.Mcp (serveMcp)
import Fisher.Lean.Session (defaultLeanEnv)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Paths (configPathIn, envPathIn, ensureGaussHome)
import Fisher.ProjectCommand
import Fisher.Term (detectStyle)
import Fisher.Workflow.LocalAuth (readKeychainClaudeCredentials)
import Fisher.Cli.LlmCheck (llmCheckMain)
import Fisher.Cli.Rlm (rlmMain)
import Fisher.Cli.ToolsCheck (toolsCheckMain)
import Fisher.Cli.AgentChat (chatMain, defaultChatDeps, replayMain)
import Fisher.Cli.Clm (readContextFlag)
import Fisher.Cli.Acp (acpMain)
import Fisher.Cli.Cron (cronMain, defaultCronDeps)
import Fisher.Cli.Login (loginMain)
import Fisher.Cli.Logout (logoutMain)
import Fisher.Cli.Model (modelMain)
import Fisher.Cli.Setup (SetupInfo (..), setupMainWith)
import Fisher.Cli.Status (StatusInfo (..), statusMain)
import Fisher.LLM.Auth.Error (SystemExit (..))
import Fisher.LLM.SysIO (newSysIO)
import Fisher.UI.Server (uiMain)
import Fisher.Paths (getGaussHome)
import System.Directory (findExecutable, getHomeDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (takeDirectory)

data Command
  = Project [String]
  | Doctor Bool Bool
  | ConfigPath
  | ConfigEnvPath
  | ConfigCheck
  | ConfigMigrate
  | ConfigSet String String
  | Plan [String]
  | Launch [String]
  | Chat [String]
  | Prewarm
  | LeanMcp
  | Search [String]
  | Check [String]
  | Shell Bool (Maybe T.Text)
  | Sessions SessionsCmd
  | ConfigShow
  | ConfigEdit
  | Version
  | ViaSys SysCommand [String]

-- | Commands ported over 'Fisher.LLM.Sys' (B1): they parse their own
-- arguments the way upstream's argparse does.
data SysCommand = SysSetup | SysModel | SysLogin | SysLogout | SysStatus | SysLlmCheck | SysRlm | SysToolsCheck | SysChat | SysReplay | SysUi | SysAcp | SysCron

sysCommand :: String -> SysCommand -> String -> Mod CommandFields Command
sysCommand name c desc =
  command name (info (ViaSys c <$> many (strArgument (metavar "ARGS..."))) (progDesc desc <> forwardOptions <> noIntersperse))

parser :: Parser Command
parser =
  subcommands
    <|> ( Shell
            <$> switch (long "compact" <> help "Use the small banner")
            <*> optional
              ( option
                  (eitherReader readContextFlag)
                  (long "context" <> metavar "STRATEGY" <> help "Fisher only: compress the context (compress), keep it as a file the model edits (clm), or move old turns out to a store the model recalls from (rlm)") ) )

subcommands :: Parser Command
subcommands = hsubparser $
  command "sessions"
    (info (Sessions <$> sessionsParser) (progDesc "Manage session history (list, rename, export, prune, delete)"))
  <> command "version" (info (pure Version) (progDesc "Show version information"))
  <> sysCommand "setup" SysSetup "Interactive setup wizard"
  <> sysCommand "model" SysModel "Select default model and provider"
  <> sysCommand "login" SysLogin "Authenticate with an inference provider"
  <> sysCommand "logout" SysLogout "Clear authentication for an inference provider"
  <> sysCommand "status" SysStatus "Show status of all components"
  <> sysCommand "llm-check" SysLlmCheck "Send one small request to the configured model (Fisher only)"
  <> sysCommand "rlm" SysRlm "Answer a query over a long context with a recursive language model (Fisher only)"
  <> sysCommand "tools-check" SysToolsCheck "List the agent's tools, call one, or show MCP status (Fisher only)"
  <> sysCommand "replay" SysReplay "Run a logged agent session again and check it is bit-exact (Fisher only)"
  <> sysCommand "ui" SysUi "Serve the Fisher web app on 127.0.0.1 and open it (Fisher only)"
  <> sysCommand "acp" SysAcp "Serve the native agent to editors over the Agent Client Protocol (stdio)"
  <> sysCommand "cron" SysCron "Manage scheduled jobs (fisher cron serve runs them)"
  <> command "project"
    (info (Project <$> many (strArgument (metavar "ARGS...")))
          (progDesc "status | init [PATH] [--name N] | convert [PATH] | use [PATH] | clear | create PATH [--template-source S] [--name N]"
             <> forwardOptions <> noIntersperse))
  <> command "plan"
    (info (Plan <$> some (strArgument (metavar "COMMAND... [--model M] [--prover P] [--print] [-- TEXT]")))
          (progDesc "Stage a workflow (/prove, /draft, /review, ...) and print its launch plan (--print: the headless one); --model M chooses the session's model" <> forwardOptions <> noIntersperse))
  <> command "launch"
    (info (Launch <$> some (strArgument (metavar "COMMAND... [--model M] [--prover P] [--print] [-- TEXT]")))
          (progDesc "Stage a workflow and run it here, attached to this terminal (--print: headless, stdin closed, the session's exit status); --model M chooses the session's model; put -- before a description that contains such words, or quote it" <> forwardOptions <> noIntersperse))
  <> command "chat"
    (info (Chat <$> many (strArgument (metavar "TEXT...")))
          (progDesc "Open a managed backend chat session (with -q/-Q: one query to the native agent)" <> forwardOptions <> noIntersperse))
  <> command "prewarm"
    (info (pure Prewarm) (progDesc "Fetch the Lean skill assets and warm the Lean MCP runtime"))
  <> command "lean-mcp"
    (info (pure LeanMcp) (progDesc "Serve the Lean tools over MCP on stdio, in place of lean-lsp-mcp"))
  <> command "search"
    (info (Search <$> many (strArgument (metavar "ARGS...")))
          (progDesc "The proof-search swarm: parallel attempts on a file's open goals (Fisher only)" <> forwardOptions <> noIntersperse))
  <> command "check"
    (info (Check <$> many (strArgument (metavar "ARGS...")))
          (progDesc "Decide whether Rocq or Isabelle targets are proved: compile, then audit axioms or oracles" <> forwardOptions <> noIntersperse))
  <> command "doctor"
    (info (Doctor <$> switch (long "fix" <> help "Create what can be created")
                  <*> switch (long "all" <> help "Check every prover, not only the one the project in reach uses"))
          (progDesc "Check the setup for managed Lean, Rocq and Isabelle workflows"))
  <> command "config"
    (info (hsubparser
             ( command "show" (info (pure ConfigShow) (progDesc "Show the current configuration"))
            <> command "edit" (info (pure ConfigEdit) (progDesc "Open config.yaml in your editor"))
            <> command "path" (info (pure ConfigPath) (progDesc "Print the config.yaml path"))
            <> command "env-path" (info (pure ConfigEnvPath) (progDesc "Print the .env path"))
            <> command "check" (info (pure ConfigCheck) (progDesc "Show the config version and keys"))
            <> command "migrate" (info (pure ConfigMigrate) (progDesc "Bring config.yaml up to the current version"))
            <> command "set" (info (ConfigSet <$> strArgument (metavar "KEY") <*> strArgument (metavar "VALUE"))
                                   (progDesc "Set a value: credentials go to .env, the rest to config.yaml")) ))
          (progDesc "Configuration commands"))

-- | Prompts on the terminal. End of input reads as an empty answer, as
-- upstream's @except (EOFError, KeyboardInterrupt)@ does.
prompter :: Prompter
prompter = Prompter ask askSecret
  where
    ask p = TIO.putStr p >> hFlush stdout >> readAnswer
    askSecret p = do
      TIO.putStr p
      hFlush stdout
      echo <- hGetEcho stdin `catch` \(_ :: IOException) -> pure True
      _ <- try (hSetEcho stdin False) :: IO (Either IOException ())
      v <- readAnswer
      _ <- try (hSetEcho stdin echo) :: IO (Either IOException ())
      TIO.putStrLn ""
      pure v
    readAnswer = TIO.getLine `catch` \e -> if isEOFError e then TIO.putStrLn "" >> pure "" else ioError e

run :: Command -> IO ()
run = \case
  Project args -> do
    (home, session) <- cliSession
    out <- handleProjectTokens session (map T.pack args)
    mapM_ TIO.putStrLn out
    readIORef (sessionOverride session) >>= saveOverride home
  Doctor fix allProvers -> do
    home <- ensureGaussHome
    (cfg, _) <- loadConfigAt home
    env <- getEnvironment
    st <- detectStyle
    let de = DoctorEnv st cfg env Nothing (getEnvValueAt home) readKeychainClaudeCredentials "fisher"
    runDoctor fix allProvers de home >>= mapM_ TIO.putStrLn
  ConfigPath -> ensureGaussHome >>= putStrLn . configPathIn
  ConfigEnvPath -> ensureGaussHome >>= putStrLn . envPathIn
  ConfigCheck -> do
    home <- ensureGaussHome
    st <- detectStyle
    configCheck st home >>= mapM_ TIO.putStrLn
  ConfigSet key val -> do
    home <- ensureGaussHome
    setConfigValueAt home (T.pack key) (T.pack val) >>= \case
      Right line -> TIO.putStrLn line
      Left e -> TIO.hPutStrLn stderr e >> exitWith (ExitFailure 1)
  Plan args -> planCommand args
  Launch args -> launchCommand args
  Chat args -> chatCommand args
  Prewarm -> prewarmCommand
  LeanMcp -> serveMcp defaultLeanEnv stdin stdout
  Search args -> searchMain args
  Check args -> checkMain args >>= \code -> if code == ExitSuccess then pure () else exitWith code
  Shell compact ctx -> runShell (ShellOptions compact ctx)
  Sessions c -> sessionsCli c
  ConfigShow -> configShowCli
  ConfigEdit -> configEditCli
  Version -> versionCli
  ViaSys c args -> do
    home <- getHomeDirectory
    gh <- getGaussHome
    sys <- newSysIO home gh
    exe <- getExecutablePath
    py <- maybe "python3" id <$> findExecutable "python3"
    let argv = map T.pack args
        info' = StatusInfo (T.pack (takeDirectory exe)) "n/a (Fisher)"
    code <- (case c of
      SysSetup -> setupMainWith (SetupInfo (T.pack py) (T.pack (takeDirectory exe))) sys argv
      SysModel -> modelMain sys argv
      SysLogin -> loginMain sys argv
      SysLogout -> logoutMain sys argv
      SysStatus -> statusMain info' sys argv
      SysLlmCheck -> llmCheckMain sys argv
      SysRlm -> rlmMain sys argv
      SysToolsCheck -> toolsCheckMain sys argv
      SysChat -> chatMain defaultChatDeps sys argv
      SysReplay -> replayMain sys argv
      SysUi -> uiMain gh args
      SysAcp -> acpMain sys argv
      SysCron -> cronMain defaultCronDeps sys argv)
      `catch` \(SystemExit n) -> pure n
    if code == 0 then pure () else exitWith (ExitFailure code)
  ConfigMigrate -> do
    home <- ensureGaussHome
    st <- detectStyle
    configMigrate st home prompter TIO.putStrLn

main :: IO ()
main = do
  -- The B1 commands parse their own arguments (upstream's argparse,
  -- help text and errors included), so they bypass optparse.
  args <- getArgs
  case args of
    (name : rest) | Just c <- lookup name sysCommands -> run (ViaSys c rest)
    -- `fisher chat -q QUERY` / `-Q`: upstream's `gauss chat`, the native agent
    ("chat" : rest) | any (`elem` ["-q", "--query", "-Q", "--quiet", "-p", "--print"]) rest -> run (ViaSys SysChat rest)
    -- check, plan and launch take their flags anywhere ('ownArgsCommand')
    _ | Just own <- ownArgsCommand args -> run $ case own of
          OwnCheck rest -> Check rest
          OwnPlan rest -> Plan rest
          OwnLaunch rest -> Launch rest
    _ -> do
      cmd <- execParser (info (parser <**> helper) (fullDesc <> progDesc "Fisher: managed Lean workflows"))
      run cmd

sysCommands :: [(String, SysCommand)]
sysCommands =
  [ ("setup", SysSetup), ("model", SysModel), ("login", SysLogin), ("logout", SysLogout)
  , ("status", SysStatus), ("llm-check", SysLlmCheck), ("rlm", SysRlm), ("tools-check", SysToolsCheck), ("replay", SysReplay), ("ui", SysUi), ("acp", SysAcp), ("cron", SysCron) ]

