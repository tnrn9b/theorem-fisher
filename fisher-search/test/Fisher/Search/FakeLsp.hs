-- | A stand-in for Lean's language server, shared by the automation specs.
module Fisher.Search.FakeLsp
  ( Fake (..)
  , fake0
  , fakeLsp
  , noCertify
  , noCertifyOn
  ) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))

import Fisher.Search.Auto
import Fisher.Search.Check (LeanRun (..))
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.Lean

-- | How the fake language server behaves.
data Fake = Fake
  { fClose :: Text -> Text -> Maybe (Maybe Text)
  -- ^ goal, its proof: Nothing leaves the hole open, Just Nothing closes
  -- it, Just (Just s) closes it with a @Try this: s@ suggestion
  , fSuspects :: [Text]
  -- ^ goals plausible finds a counterexample for
  , fDown :: Text -> Bool
  -- ^ which method fails: attempts, goal, diagnostics
  , fDelay :: Text -> Int
  -- ^ ms each call of a method takes
  }

fake0 :: Fake
fake0 = Fake (\_ _ -> Nothing) [] (const False) (const 0)

declAt :: Text -> Hole -> Maybe Decl
declAt src h = find (\d -> dStart d <= hLine h - 1 && hLine h - 1 < dEnd d) (declarations src)

-- | The fake Lsp, logging each call: @attempts g@, @plausible g@,
-- @diagnostics@, @goal@, @close@.
fakeLsp :: IORef [Text] -> Fake -> Lsp
fakeLsp calls f = Lsp
  { lspAttempts = \src h snips -> do
      let d = declAt src h
          g = maybe "?" dName d
          isPl = any ("plausible" `T.isInfixOf`) snips
      logCall ((if isPl then "plausible " else "attempts ") <> g)
      threadDelay (fDelay f "attempts" * 1000)
      if fDown f "attempts" then pure (Left "server died") else
        pure . Right $
          [ if isPl
              then Trial s [] [Diag (hLine h) 2 SevError (if g `elem` fSuspects f then "Found a counter-example!\nx := 0" else "Unable to find a counter-example")]
              else case fClose f g (maybe "" dProof d) of
                Nothing -> Trial s ["P"] []
                Just Nothing -> Trial s [] []
                Just (Just sug) -> Trial s [] [Diag (hLine h) 2 SevInfo ("Try this: " <> sug)]
          | s <- snips ]
  , lspGoal = \_ h -> do
      logCall "goal"
      if fDown f "goal" then pure (Left "server died") else pure (Right (Just ("goal at line " <> T.pack (show (hLine h)))))
  , lspDiagnostics = \src -> do
      logCall "diagnostics"
      threadDelay (fDelay f "diagnostics" * 1000)
      if fDown f "diagnostics" then pure (Left "server died") else
        pure (Right [Diag i 2 SevError "type mismatch" | (i, l) <- zip [1 ..] (T.splitOn "\n" src), "BAD" `T.isInfixOf` l, not ("maxHeartbeats" `T.isInfixOf` l)])
  , lspClose = logCall "close"
  , lspTimedOut = pure () }
  where logCall c = atomicModifyIORef' calls (\cs -> (cs <> [c], ()))

-- | Compiles as 'fakeLean' does, but a file holding a negation to
-- certify fails.
noCertify :: LeanRun
noCertify = noCertifyOn fakeLean

-- | 'noCertify' over a given Lean stand-in.
noCertifyOn :: LeanRun -> LeanRun
noCertifyOn lean = LeanRun $ \root rel -> do
  t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
  if "theorem _fisher_neg_" `T.isInfixOf` t
    then pure (1, T.pack rel <> ":1:1: error: nope")
    else runLean lean root rel
