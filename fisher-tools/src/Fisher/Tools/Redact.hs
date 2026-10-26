-- | The secret masking now lives in fisher-core ("Fisher.Redact"), so the
-- run log can use it; this module re-exports it for the existing callers.
module Fisher.Tools.Redact (module Fisher.Redact) where

import Fisher.Redact
