-- | The parts of upstream's backends that are plain functions: the
-- subprocess environment (secrets filtered, PATH made sane), the sudo
-- rewrite, the output fence and shell-noise cleaning, and Python's text-mode
-- decoding of a child's output.
module Fisher.Tools.Sandbox.Common
  ( providerEnvBlocklist
  , makeRunEnv
  , sanePath
  , transformSudo
  , outputFence
  , fencedCommand
  , extractFencedOutput
  , cleanShellNoise
  , decodeOutput
  , mergeStdin
  ) where

import qualified Data.ByteString as BS
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Fisher.Py.Re (compileOrDie, search, subText)

-- | @_GAUSS_PROVIDER_ENV_BLOCKLIST@: Gauss-managed secrets and settings that
-- never reach a subprocess. Upstream derives part of it from its provider
-- registry and optional-env table; this is the resulting set at the
-- pinned commit (the sandbox golden checks it).
providerEnvBlocklist :: S.Set Text
providerEnvBlocklist =
  S.fromList
    [ "ANTHROPIC_API_KEY", "ANTHROPIC_BASE_URL", "ANTHROPIC_TOKEN", "BROWSERBASE_API_KEY", "BROWSERBASE_PROJECT_ID"
    , "CLAUDE_CODE_OAUTH_TOKEN", "COHERE_API_KEY", "DEEPSEEK_API_KEY", "DEEPSEEK_BASE_URL", "DISCORD_ALLOWED_USERS"
    , "DISCORD_AUTO_THREAD", "DISCORD_BOT_TOKEN", "DISCORD_FREE_RESPONSE_CHANNELS", "DISCORD_HOME_CHANNEL"
    , "DISCORD_HOME_CHANNEL_NAME", "DISCORD_REQUIRE_MENTION", "ELEVENLABS_API_KEY", "EMAIL_ADDRESS", "EMAIL_HOME_ADDRESS"
    , "EMAIL_HOME_ADDRESS_NAME", "EMAIL_IMAP_HOST", "EMAIL_PASSWORD", "EMAIL_SMTP_HOST", "FAL_KEY", "FIRECRAWL_API_KEY"
    , "FIRECRAWL_API_URL", "FIREWORKS_API_KEY", "GATEWAY_ALLOWED_USERS", "GATEWAY_ALLOW_ALL_USERS", "GH_TOKEN", "GITHUB_APP_ID"
    , "GITHUB_APP_INSTALLATION_ID", "GITHUB_APP_PRIVATE_KEY_PATH", "GITHUB_TOKEN", "GLM_API_KEY", "GLM_BASE_URL"
    , "GOOGLE_API_KEY", "GROQ_API_KEY", "HASS_TOKEN", "HASS_URL", "HELICONE_API_KEY", "KIMI_API_KEY", "KIMI_BASE_URL", "LLM_MODEL"
    , "MINIMAX_API_KEY", "MINIMAX_BASE_URL", "MINIMAX_CN_API_KEY", "MINIMAX_CN_BASE_URL", "MISTRAL_API_KEY", "OPENAI_API_BASE"
    , "OPENAI_API_KEY", "OPENAI_BASE_URL", "OPENAI_ORGANIZATION", "OPENAI_ORG_ID", "OPENROUTER_API_KEY", "PERPLEXITY_API_KEY"
    , "SIGNAL_ACCOUNT", "SIGNAL_ALLOWED_USERS", "SIGNAL_GROUP_ALLOWED_USERS", "SIGNAL_HOME_CHANNEL"
    , "SIGNAL_HOME_CHANNEL_NAME", "SIGNAL_HTTP_URL", "SIGNAL_IGNORE_STORIES", "SLACK_ALLOWED_USERS", "SLACK_APP_TOKEN"
    , "SLACK_BOT_TOKEN", "SLACK_HOME_CHANNEL", "SLACK_HOME_CHANNEL_NAME", "SUDO_PASSWORD", "TELEGRAM_ALLOWED_USERS"
    , "TELEGRAM_BOT_TOKEN", "TELEGRAM_HOME_CHANNEL", "TELEGRAM_HOME_CHANNEL_NAME", "TINKER_API_KEY", "TOGETHER_API_KEY"
    , "VOICE_TOOLS_OPENAI_KEY", "WANDB_API_KEY", "WHATSAPP_ALLOWED_USERS", "WHATSAPP_ENABLED", "WHATSAPP_MODE", "XAI_API_KEY"
    , "ZAI_API_KEY", "Z_AI_API_KEY"
    ]

sanePath :: Text
sanePath = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

forcePrefix :: Text
forcePrefix = "_GAUSS_FORCE_"

-- | @_make_run_env(env)@ over this process's environment (in its order):
-- blocked names dropped, @_GAUSS_FORCE_X@ re-admitted as @X@, and the sane
-- PATH appended when @/usr/bin@ is missing from it.
makeRunEnv :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
makeRunEnv environ extra =
  let merged = [(k, maybe v id (lookup k extra)) | (k, v) <- environ] <> [(k, v) | (k, v) <- extra, k `notElem` map fst environ]
      put acc (k, v) = if any ((== k) . fst) acc then [(k', if k' == k then v else v') | (k', v') <- acc] else acc <> [(k, v)]
      step acc (k, v)
        | forcePrefix `T.isPrefixOf` k = put acc (T.drop (T.length forcePrefix) k, v)
        | k `S.member` providerEnvBlocklist = acc
        | otherwise = put acc (k, v)
      runEnv = foldl step [] merged
      existing = maybe "" id (lookup "PATH" runEnv)
   in if "/usr/bin" `elem` T.splitOn ":" existing
        then runEnv
        else put runEnv ("PATH", if T.null existing then sanePath else existing <> ":" <> sanePath)

-- | @_transform_sudo_command@ once the password is known (empty = none):
-- every standalone word @sudo@ gets @-S -p ''@ and the password goes on
-- stdin.
transformSudo :: Text -> Text -> (Text, Maybe Text)
transformSudo pw cmd
  | Nothing <- search sudoRe cmd = (cmd, Nothing)
  | T.null pw = (cmd, Nothing)
  | otherwise = (subText sudoRe "sudo -S -p ''" cmd, Just (pw <> "\n"))
  where
    sudoRe = compileOrDie [] "\\bsudo\\b"

-- | Sudo stdin first, then the caller's.
mergeStdin :: Maybe Text -> Maybe Text -> Maybe Text
mergeStdin sudo stdin = case (sudo, stdin) of
  (Just s, Just d) -> Just (s <> d)
  (Just s, Nothing) -> Just s
  (Nothing, d) -> d

outputFence :: Text
outputFence = "__GAUSS_FENCE_a9f7b3__"

fencedCommand :: Text -> Text
fencedCommand cmd = "printf '" <> outputFence <> "'; " <> cmd <> "; __gauss_rc=$?; printf '" <> outputFence <> "'; exit $__gauss_rc"

-- | @_extract_fenced_output@.
extractFencedOutput :: Text -> Text
extractFencedOutput raw =
  case T.breakOn outputFence raw of
    (_, "") -> cleanShellNoise raw
    (before, _) ->
      let first = T.length before
          start = first + T.length outputFence
          lastIdx = T.length (fst (T.breakOnEnd outputFence raw)) - T.length outputFence
       in if lastIdx <= first
            then cleanShellNoise (T.drop start raw)
            else T.take (lastIdx - start) (T.drop start raw)

noise :: [Text]
noise =
  [ "bash: cannot set terminal process group", "bash: no job control in this shell", "no job control in this shell"
  , "cannot set terminal process group", "tcsetattr: Inappropriate ioctl for device", "Restored session:"
  , "Saving session...", "Last login:", "command not found:", "Oh My Zsh", "compinit:"
  ]

-- | @_clean_shell_noise@: noise lines off the front, noise and empty lines
-- off the back, a trailing newline kept.
cleanShellNoise :: Text -> Text
cleanShellNoise output =
  let isNoise l = any (`T.isInfixOf` l) noise
      ls = dropWhile isNoise (T.splitOn "\n" output)
      kept = reverse (dropWhile (\l -> T.null l || isNoise l) (reverse ls))
   in if null kept
        then ""
        else
          let r = T.intercalate "\n" kept
           in if "\n" `T.isSuffixOf` output && not (T.null r) && not ("\n" `T.isSuffixOf` r) then r <> "\n" else r

-- | A child's output as Python's @text=True, errors="replace"@ reads it:
-- UTF-8 with replacement characters, and universal newlines (@\\r\\n@ and
-- lone @\\r@ become @\\n@).
decodeOutput :: BS.ByteString -> Text
decodeOutput bs =
  let t = TE.decodeUtf8With TEE.lenientDecode bs
   in T.replace "\r" "\n" (T.replace "\r\n" "\n" t)
