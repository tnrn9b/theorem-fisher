-- | The argparse surface of @fisher login@, @logout@, @status@, @setup@ and
-- @model@ (main.py): the options each takes, their help (as argparse lays
-- it out at 100 columns) and argparse's error text. Unique prefixes of
-- long options are accepted, as argparse accepts them. A @--provider@ that
-- names a provider Fisher removed gets the removal message, not argparse's
-- invalid choice.
module Fisher.Cli.AuthArgs
  ( ArgParser (..)
  , OptSpec (..)
  , OptKind (..)
  , Parsed
  , parseArgs
  , flagSet
  , optValue
  , loginParser
  , logoutParser
  , statusParser
  , setupParser
  , modelParser
  ) where

import Data.List (find, isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.LLM.Auth.Registry (isRemovedProvider, removedProviderMessage)
import Fisher.LLM.Auth.Util (pyFloatText)

data OptKind
  = Flag
  | Value (Maybe [Text]) Bool
  -- ^ choices, and whether the value must be a float
  deriving (Eq, Show)

data OptSpec = OptSpec
  { osName :: Text
  -- ^ the long option, @--provider@
  , osMetavar :: Text
  , osKind :: OptKind
  }
  deriving (Eq, Show)

data ArgParser = ArgParser
  { apProg :: Text
  , apUsage :: Text
  -- ^ the usage block, ending in a newline
  , apHelp :: Text
  -- ^ the whole @--help@ output
  , apOpts :: [OptSpec]
  , apPositional :: Maybe (Text, [Text])
  -- ^ an optional positional with its choices
  }

-- | Options given, by option name without dashes (@provider@), and the
-- positional under @"_"@.
type Parsed = [(Text, Text)]

flagSet :: Text -> Parsed -> Bool
flagSet k = isJust . lookup k

optValue :: Text -> Parsed -> Maybe Text
optValue = lookup

-- | Parse like argparse: Left is the text to print and the exit code
-- (help, 0; an error, 2).
parseArgs :: ArgParser -> [Text] -> Either (Text, Int) Parsed
parseArgs p args0 = do
  (acc, pos, extra) <- scan args0 [] [] []
  case (apPositional p, pos) of
    _ | not (null extra) -> err ("unrecognized arguments: " <> T.unwords extra)
    (_, []) -> Right acc
    (Just (name, choices), v : more)
      | v `notElem` choices -> err ("argument " <> name <> ": invalid choice: '" <> v <> "' (choose from " <> choiceList choices <> ")")
      | not (null more) -> err ("unrecognized arguments: " <> T.unwords more)
      | otherwise -> Right (acc <> [("_", v)])
    (Nothing, more) -> err ("unrecognized arguments: " <> T.unwords more)
  where
    err msg = Left (apUsage p <> apProg p <> ": error: " <> msg <> "\n", 2)
    choiceList cs = T.intercalate ", " ["'" <> c <> "'" | c <- cs]
    dest o = T.drop 2 (osName o)
    scan args acc pos extra = case args of
      [] -> Right (acc, pos, extra)
      (a : rest)
        | a == "-h" || a == "--help" -> Left (apHelp p, 0)
        | a == "--" -> Right (acc, pos <> rest, extra)
        | "--" `T.isPrefixOf` a -> do
            let (name, inline) = case T.breakOn "=" a of
                  (n, "") -> (n, Nothing)
                  (n, v) -> (n, Just (T.drop 1 v))
                exact = find ((== name) . osName) (apOpts p)
                prefixed = [o | o <- apOpts p, T.unpack name `isPrefixOf` T.unpack (osName o)]
                helpMatch = name `T.isPrefixOf` "--help" && T.length name > 2
            case (exact, prefixed) of
              (Just o, _) -> takeOpt o inline rest
              (Nothing, [o]) | not helpMatch -> takeOpt o inline rest
              (Nothing, []) | helpMatch -> Left (apHelp p, 0)
              (Nothing, []) -> scan rest acc pos (extra <> [a])
              (Nothing, os) ->
                err ("ambiguous option: " <> name <> " could match " <> T.intercalate ", " (map osName os <> ["--help" | helpMatch]))
        | "-" `T.isPrefixOf` a && T.length a > 1 -> scan rest acc pos (extra <> [a])
        | otherwise -> scan rest acc (pos <> [a]) extra
      where
        takeOpt o inline rest = case osKind o of
          Flag -> case inline of
            Just v -> err ("argument " <> osName o <> ": ignored explicit argument '" <> v <> "'")
            Nothing -> scan rest (upsert (dest o) "true" acc) pos extra
          Value choices isFloat -> do
            (v, rest') <- case inline of
              Just v -> Right (v, rest)
              Nothing -> case rest of
                (v : r) | not ("-" `T.isPrefixOf` v) || v == "-" -> Right (v, r)
                _ -> err ("argument " <> osName o <> ": expected one argument")
            case choices of
              Just cs | v `notElem` cs, osName o == "--provider", isRemovedProvider v ->
                err ("argument --provider: " <> removedProviderMessage)
              Just cs | v `notElem` cs ->
                err ("argument " <> osName o <> ": invalid choice: '" <> v <> "' (choose from " <> choiceList cs <> ")")
              _ | isFloat, Nothing <- pyFloatText v -> err ("argument " <> osName o <> ": invalid float value: '" <> v <> "'")
              _ -> scan rest' (upsert (dest o) v acc) pos extra
    upsert k v acc = filter ((/= k) . fst) acc <> [(k, v)]

providerChoices :: [Text]
providerChoices = ["openai-codex"]

loginParser :: ArgParser
loginParser =
  ArgParser
    { apProg = "fisher login"
    , apUsage = loginUsage
    , apHelp =
        loginUsage
          <> "\nRun OAuth device authorization flow for the Fisher CLI\n\noptions:\n\
             \  -h, --help            show this help message and exit\n\
             \  --provider {openai-codex}\n\
             \                        Provider to authenticate with\n\
             \  --no-browser          Do not attempt to open the browser automatically\n\
             \  --timeout TIMEOUT     HTTP request timeout in seconds (default: 15)\n\
             \  --ca-bundle CA_BUNDLE\n\
             \                        Path to CA bundle PEM file for TLS verification\n\
             \  --insecure            Disable TLS verification (testing only)\n"
    , apOpts =
        [ OptSpec "--provider" "PROVIDER" (Value (Just providerChoices) False)
        , OptSpec "--no-browser" "" Flag
        , OptSpec "--timeout" "TIMEOUT" (Value Nothing True)
        , OptSpec "--ca-bundle" "CA_BUNDLE" (Value Nothing False)
        , OptSpec "--insecure" "" Flag ]
    , apPositional = Nothing
    }
  where
    loginUsage =
      "usage: fisher login [-h] [--provider {openai-codex}] [--no-browser] [--timeout TIMEOUT]\n\
      \                    [--ca-bundle CA_BUNDLE] [--insecure]\n"

logoutParser :: ArgParser
logoutParser =
  ArgParser
    { apProg = "fisher logout"
    , apUsage = usage
    , apHelp =
        usage
          <> "\nRemove stored credentials and reset provider config\n\noptions:\n\
             \  -h, --help            show this help message and exit\n\
             \  --provider {openai-codex}\n\
             \                        Provider to log out from (default: active provider)\n"
    , apOpts = [OptSpec "--provider" "PROVIDER" (Value (Just providerChoices) False)]
    , apPositional = Nothing
    }
  where
    usage = "usage: fisher logout [-h] [--provider {openai-codex}]\n"

statusParser :: ArgParser
statusParser =
  ArgParser
    { apProg = "fisher status"
    , apUsage = usage
    , apHelp =
        usage
          <> "\nDisplay status of Fisher components\n\noptions:\n\
             \  -h, --help  show this help message and exit\n\
             \  --all       Show all details (redacted for sharing)\n\
             \  --deep      Run deep checks (may take longer)\n"
    , apOpts = [OptSpec "--all" "" Flag, OptSpec "--deep" "" Flag]
    , apPositional = Nothing
    }
  where
    usage = "usage: fisher status [-h] [--all] [--deep]\n"

setupParser :: ArgParser
setupParser =
  ArgParser
    { apProg = "fisher setup"
    , apUsage = usage
    , apHelp =
        usage
          <> "\nConfigure Fisher with an interactive wizard. Run a specific section: fisher setup\n\
             \model|terminal|gateway|tools|agent\n\n\
             \positional arguments:\n\
             \  {model,terminal,gateway,tools,agent}\n\
             \                        Run a specific setup section instead of the full wizard\n\n\
             \options:\n\
             \  -h, --help            show this help message and exit\n\
             \  --non-interactive     Non-interactive mode (use defaults/env vars)\n\
             \  --reset               Reset configuration to defaults\n"
    , apOpts = [OptSpec "--non-interactive" "" Flag, OptSpec "--reset" "" Flag]
    , apPositional = Just ("section", ["model", "terminal", "gateway", "tools", "agent"])
    }
  where
    usage = "usage: fisher setup [-h] [--non-interactive] [--reset] [{model,terminal,gateway,tools,agent}]\n"

modelParser :: ArgParser
modelParser =
  ArgParser
    { apProg = "fisher model"
    , apUsage = usage
    , apHelp =
        usage
          <> "\nInteractively select your inference provider and default model\n\noptions:\n\
             \  -h, --help  show this help message and exit\n"
    , apOpts = []
    , apPositional = Nothing
    }
  where
    usage = "usage: fisher model [-h]\n"
