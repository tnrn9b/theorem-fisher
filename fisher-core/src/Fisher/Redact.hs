-- | Secret masking, as upstream's @agent.redact.redact_sensitive_text@
-- applies it to file contents and search hits before the model sees them,
-- and as @tools.mcp_tool._sanitize_error@ applies it to MCP error text.
-- Same patterns, same order, same masks.
module Fisher.Redact
  ( redactSensitiveText
  , redactEnabled
  , sanitizeError
  , maskToken
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Re

-- | Redaction is on unless @FISHER_REDACT_SECRETS@ is 0, false, no or off.
redactEnabled :: Maybe Text -> Bool
redactEnabled v = maybe True (\x -> T.toLower x `notElem` ["0", "false", "no", "off"]) v

-- | Short tokens (under 18 characters) become @***@; longer ones keep their
-- first 6 and last 4 characters.
maskToken :: Text -> Text
maskToken t
  | T.length t < 18 = "***"
  | otherwise = T.take 6 t <> "..." <> T.takeEnd 4 t

prefixPatterns :: [Text]
prefixPatterns =
  [ "sk-[A-Za-z0-9_-]{10,}"
  , "ghp_[A-Za-z0-9]{10,}"
  , "github_pat_[A-Za-z0-9_]{10,}"
  , "xox[baprs]-[A-Za-z0-9-]{10,}"
  , "AIza[A-Za-z0-9_-]{30,}"
  , "pplx-[A-Za-z0-9]{10,}"
  , "fal_[A-Za-z0-9_-]{10,}"
  , "fc-[A-Za-z0-9]{10,}"
  , "bb_live_[A-Za-z0-9_-]{10,}"
  , "gAAAA[A-Za-z0-9_=-]{20,}"
  , "AKIA[A-Z0-9]{16}"
  , "sk_live_[A-Za-z0-9]{10,}"
  , "sk_test_[A-Za-z0-9]{10,}"
  , "rk_live_[A-Za-z0-9]{10,}"
  , "SG\\.[A-Za-z0-9_-]{10,}"
  , "hf_[A-Za-z0-9]{10,}"
  , "r8_[A-Za-z0-9]{10,}"
  , "npm_[A-Za-z0-9]{10,}"
  , "pypi-[A-Za-z0-9_-]{10,}"
  , "dop_v1_[A-Za-z0-9]{10,}"
  , "doo_v1_[A-Za-z0-9]{10,}"
  , "am_[A-Za-z0-9_-]{10,}"
  ]

prefixRe, envAssignRe, jsonFieldRe, authHeaderRe, telegramRe, privateKeyRe, dbConnRe, phoneRe :: Regex
prefixRe = compileOrDie [] ("(?<![A-Za-z0-9_-])(" <> T.intercalate "|" prefixPatterns <> ")(?![A-Za-z0-9_-])")
envAssignRe = compileOrDie [IgnoreCase] "(?<![A-Za-z_])(?=[A-Za-z_]*\\s*=)([A-Z_]*(?:API_?KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL|AUTH)[A-Z_]*)\\s*=\\s*(['\"]?)(\\S+)\\2"
jsonFieldRe = compileOrDie [IgnoreCase] "(\"(?:api_?[Kk]ey|token|secret|password|access_token|refresh_token|auth_token|bearer|secret_value|raw_secret|secret_input|key_material)\")\\s*:\\s*\"([^\"]+)\""
authHeaderRe = compileOrDie [IgnoreCase] "(Authorization:\\s*Bearer\\s+)(\\S+)"
telegramRe = compileOrDie [] "(bot)?(?<!\\d)(\\d{8,}):([-A-Za-z0-9_]{30,})"
privateKeyRe = compileOrDie [] "-----BEGIN[A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END[A-Z ]*PRIVATE KEY-----"
dbConnRe = compileOrDie [IgnoreCase] "((?:postgres(?:ql)?|mysql|mongodb(?:\\+srv)?|redis|amqp)://[^:]+:)([^@]+)(@)"
phoneRe = compileOrDie [] "(\\+[1-9]\\d{6,14})(?![A-Za-z0-9])"

-- | @redact_sensitive_text(text)@ with redaction enabled; the caller checks
-- 'redactEnabled' first.
redactSensitiveText :: Text -> Text
redactSensitiveText text
  | T.null text = text
  | otherwise =
      let g n m = groupOr "" n m
          t1 = sub prefixRe (maskToken . g 1) text
          t2 = sub envAssignRe (\m -> g 1 m <> "=" <> g 2 m <> maskToken (g 3 m) <> g 2 m) t1
          t3 = sub jsonFieldRe (\m -> g 1 m <> ": \"" <> maskToken (g 2 m) <> "\"") t2
          t4 = sub authHeaderRe (\m -> g 1 m <> maskToken (g 2 m)) t3
          t5 = sub telegramRe (\m -> g 1 m <> g 2 m <> ":***") t4
          t6 = sub privateKeyRe (const "[REDACTED PRIVATE KEY]") t5
          t7 = sub dbConnRe (\m -> g 1 m <> "***" <> g 3 m) t6
          t8 = sub phoneRe (\m -> let ph = g 1 m in if T.length ph <= 8 then T.take 2 ph <> "****" <> T.takeEnd 2 ph else T.take 4 ph <> "****" <> T.takeEnd 4 ph) t7
       in t8

credentialRe :: Regex
credentialRe =
  compileOrDie
    [IgnoreCase]
    ( T.intercalate
        "|"
        [ "ghp_[A-Za-z0-9_]{1,255}"
        , "sk-[A-Za-z0-9_]{1,255}"
        , "Bearer\\s+\\S+"
        , "token=[^\\s&,;\"']{1,255}"
        , "key=[^\\s&,;\"']{1,255}"
        , "API_KEY=[^\\s&,;\"']{1,255}"
        , "password=[^\\s&,;\"']{1,255}"
        , "secret=[^\\s&,;\"']{1,255}"
        ]
    )

-- | @_sanitize_error@: every credential-looking run becomes @[REDACTED]@.
sanitizeError :: Text -> Text
sanitizeError = subText credentialRe "[REDACTED]"
