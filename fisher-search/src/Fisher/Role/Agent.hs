{-# LANGUAGE DataKinds #-}

-- | The role monad: a model-call job for role @r@ runs in @'AgentM' r@,
-- whose only effects are the operations here ('ask', 'askTwice', 'check',
-- 'withChecker', 'event'). Its constructor is not exported and it has no
-- @MonadIO@ instance, so a job that needs another effect has to add it to
-- 'RoleCtx', where a reviewer sees it; 'runRole' is the only way to run
-- one.
module Fisher.Role.Agent
  ( AgentM
  , RoleCtx (..)
  , ask
  , askTwice
  , check
  , withChecker
  , event
  , excerpt
  , oneLineOf
  , runRole
  , roleCtx
  , CanCheck
  ) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.Aeson (Value)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Role (KnownRole (..), Prompt, Role (..), Said, partText, saidText, static, tool)
import Fisher.Role.Internal (PromptPart (..), said')
import Fisher.Search.Ask (Ask, AskError (..), Asked (..), Checker (..), locked)
import Fisher.Search.Check (Probe, probe)
import Fisher.Search.Split (Message)

-- | What a job in role @r@ may do: call the model as @r@, check a sketch
-- (when @r@ may), charge its dollars to @r@ and log an event tagged @r@.
data RoleCtx (r :: Role) = RoleCtx
  { rcAsk :: [Message] -> IO (Either AskError (Text, Double))
  -- ^ the run's 'Ask', applied to this role
  , rcCheck :: Maybe Checker
  -- ^ 'Nothing' for roles that may not check
  , rcSpend :: Double -> IO ()
  -- ^ adds to this role's dollars in the run's cost record
  , rcEvent :: Text -> Value -> IO ()
  -- ^ a run-log event, tagged with the role
  }

-- | A job in role @r@. Only 'Functor', 'Applicative' and 'Monad', written
-- by hand (a reader over the context); no @MonadIO@, so the IO it does is
-- the operations of this module.
newtype AgentM (r :: Role) a = AgentM (RoleCtx r -> IO a)

instance Functor (AgentM r) where
  fmap f (AgentM m) = AgentM (fmap f . m)

instance Applicative (AgentM r) where
  pure a = AgentM (const (pure a))
  AgentM f <*> AgentM a = AgentM (\c -> f c <*> a c)

instance Monad (AgentM r) where
  AgentM m >>= k = AgentM (\c -> m c >>= \a -> runRole c (k a))

-- | Run a job with its role's context.
runRole :: RoleCtx r -> AgentM r a -> IO a
runRole c (AgentM m) = m c

-- Only here: an action that uses the context, as a job step.
withCtx :: (RoleCtx r -> IO a) -> AgentM r a
withCtx = AgentM

-- | The roles that may check a sketch with the kernel.
class CanCheck (r :: Role)

instance CanCheck 'Planner
instance CanCheck 'Verifier

-- | A role's context from the run's 'Ask', its checker ('Nothing' for a
-- role that does not check, the judge's always) and the run's spend and
-- event writers, each given this role.
roleCtx :: forall r. KnownRole r => Ask -> Maybe Checker -> (Role -> Double -> IO ()) -> (Role -> Text -> Value -> IO ()) -> RoleCtx r
roleCtx ask' ck spend ev = RoleCtx (ask' role) ck (spend role) (ev role)
  where
    role = roleVal (Proxy @r)

-- | One model call as @r@: the prompt becomes text at the hand-off to the
-- model, the reply is what @r@ said, and its dollars are charged to @r@.
ask :: Prompt -> AgentM r (Either AskError (Said r, Double))
ask prompt = withCtx $ \c -> do
  r <- rcAsk c [(who, partText p) | (who, p) <- prompt]
  case r of
    Left e -> pure (Left e)
    Right (t, d) -> do
      rcSpend c d
      pure (Right (said' t, d))

-- | Two calls at most: a failed call, or a reply the reader refuses, gets
-- one more. An asynchronous exception in a call or in the reader is
-- rethrown. The reader gets what @r@ said and runs in the role monad, so it
-- does no IO but the role's operations. A refused reply goes to the second
-- call's prompt, with what was wrong with it, as a part made here: a role
-- sees its own reply again in its own call, whatever the role (the judge
-- too, which has no 'Fisher.Role.toPrompt'), and the builder never holds
-- the 'Said'. The last problem, as a part (@errPart@ gives a refused
-- reply's problem the provenance of its pieces), is the job's error; a
-- failed call or an exception is Fisher's sentence ('static') with the
-- provider's or the exception's text ('tool').
askTwice :: (Maybe (PromptPart, e) -> Prompt) -> (e -> PromptPart) -> (Said r -> AgentM r (Either e a)) -> AgentM r (Asked a)
askTwice promptFor errPart readReply = withCtx $ \c -> go c (0 :: Int) Nothing 0
  where
    go c n prev cost = do
      r <- try (runRole c (ask (promptFor prev)))
      case r of
        Left e -> broke cost e
        Right (Left AskNoCredit) -> pure (NoCredit cost)
        Right (Left (AskFailed e)) -> again c n cost (static "the model call failed: " <> tool e) prev
        Right (Right (s, d)) -> do
          x <- try (runRole c (readReply s))
          case x of
            Left e -> broke (cost + d) e
            Right (Right a) -> pure (Asked (cost + d) (Right a))
            Right (Left e) -> again c n (cost + d) (errPart e) (Just (PromptPart (saidText s), e))
    again c n cost e next
      | n == 0 = go c 1 next cost
      | otherwise = pure (Asked cost (Left e))
    -- an exception ends the job as a failure, keeping the dollars already
    -- spent; an asynchronous one (an interrupt, a cancel) goes on up
    broke cost e
      | Just (_ :: SomeAsyncException) <- fromException e = throwIO e
      | otherwise = pure (Asked cost (Left (static "the call broke: " <> tool (T.take 300 (T.pack (displayException (e :: SomeException)))))))

-- | The kernel probe of @text@ with each of @names@ printed, under the
-- checker's lock. A role with 'CanCheck' always has a checker in its
-- context; reaching the error is a programming mistake the type is meant
-- to prevent.
check :: CanCheck r => Text -> [Text] -> AgentM r Probe
check text names = withCtx $ \c -> case rcCheck c of
  Nothing -> error "check without a checker"
  Just ck -> locked ck (probe (ckLean ck) (ckRoot ck) (ckRel ck) text names)

-- | An action with the role's checker, under the checker's lock: the
-- kernel checks a job runs other than 'check' (the planner's
-- @checkSketchVerdict@). The same guarantee as 'check': a role with
-- 'CanCheck' always has a checker in its context.
withChecker :: CanCheck r => (Checker -> IO a) -> AgentM r a
withChecker act = withCtx $ \c -> case rcCheck c of
  Nothing -> error "withChecker without a checker"
  Just ck -> locked ck (act ck)

-- | A piece a reader takes out of what @r@ said (the planner's blueprint
-- block or a name in its sketch, the verifier's reason, JSON-decoded),
-- with the rest of what it read: the piece is still @r@'s words. The
-- reader must be a pure function of the reply alone, reading the piece out
-- of it (parsing, decoding, finding); a reader whose closure supplies
-- other text as the piece is a review error. A reader that refuses the
-- reply gives its problem.
excerpt :: (Text -> Either e (Text, a)) -> Said r -> Either e (Said r, a)
excerpt readText s = (\(t, a) -> (said' t, a)) <$> readText (saidText s)

-- | What @r@ said put on one line (its words joined by single spaces) and
-- cut at @n@ characters, as a failure note puts it: still @r@'s words.
oneLineOf :: Int -> Said r -> Said r
oneLineOf n = said' . T.take n . T.unwords . T.words . saidText

-- | A run-log event, tagged with this role.
event :: Text -> Value -> AgentM r ()
event name v = withCtx $ \c -> rcEvent c name v
