{-# LANGUAGE DataKinds #-}

module Fisher.Role.AgentSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar, tryReadMVar)
import Control.Exception (evaluate)
import Data.Aeson (Value (..))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Role (Prompt, Role (..), partText, roleName, saidText, static, toPrompt)
import Fisher.Role.Agent
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), Probe (..), standardAxioms)
import Fisher.Search.GraphJobs (Ask, AskError (..), Asked (..), Checker (..))

-- "runRole is the only way in": AgentM's constructor is not exported and
-- it has no MonadIO instance, so a job in AgentM r runs only through
-- runRole with a RoleCtx r, and does IO only through ask, askTwice, check
-- and event. That is a compile-level fact; there is no test body for it.

-- | An Ask that replies with the given texts in turn, 0.25 dollars each,
-- recording the role and the messages of every call.
scripted :: [Either AskError Text] -> IO (Ask, IORef [(Role, [(Text, Text)])])
scripted replies = do
  left <- newIORef replies
  calls <- newIORef []
  let ask' role msgs = do
        modifyIORef' calls (++ [(role, msgs)])
        next <- atomicModifyIORef' left (\case [] -> ([], Left (AskFailed "no more replies")); (x : xs) -> (xs, x))
        pure (fmap (\t -> (t, 0.25)) next)
  pure (ask', calls)

-- | Spend and event writers that record the role's name with each entry,
-- and readers of what they recorded.
recorders :: IO (Role -> Double -> IO (), Role -> Text -> Value -> IO (), IO [(Text, Double)], IO [(Text, Text)])
recorders = do
  spent <- newIORef []
  events <- newIORef []
  pure
    ( \r d -> modifyIORef' spent (++ [(roleName r, d)])
    , \r n _ -> modifyIORef' events (++ [(roleName r, n)])
    , readIORef spent
    , readIORef events
    )

prompt :: Text -> Prompt
prompt t = [("user", static t)]

spec :: Spec
spec = describe "the role monad" $ do
  it "ask charges the role's dollars and names the role in events" $ do
    (ask', calls) <- scripted [Right "ok"]
    (spend, ev, spent, events) <- recorders
    let ctx = roleCtx @'Planner ask' Nothing spend ev
    r <- runRole ctx $ do
      a <- ask (prompt "hello")
      event "planned" (String "x")
      pure a
    fmap (\(s, d) -> (saidText s, d)) r `shouldBe` Right ("ok", 0.25)
    spent >>= (`shouldBe` [("planner", 0.25)])
    events >>= (`shouldBe` [("planner", "planned")])
    readIORef calls >>= (`shouldBe` [(Planner, [("user", "hello")])])

  it "askTwice retries once on a refused reply and feeds it back" $ do
    (ask', calls) <- scripted [Right "bad reply", Right "good"]
    (spend, ev, spent, _) <- recorders
    let msgsFor = \case
          Nothing -> prompt "go"
          Just (p, e) -> prompt "go" ++ [("assistant", p), ("user", static e)]
        readReply s = let t = saidText s in pure (if t == "good" then Right t else Left ("refused: " <> t))
    r <- runRole (roleCtx @'Verifier ask' Nothing spend ev) (askTwice msgsFor id readReply)
    case r of
      Asked d (Right t) -> (d, t) `shouldBe` (0.5, "good")
      _ -> expectationFailure "expected Asked with a reply"
    cs <- readIORef calls
    length cs `shouldBe` 2
    map fst cs `shouldBe` [Verifier, Verifier]
    let second = T.unlines (map snd (snd (cs !! 1)))
    second `shouldSatisfy` T.isInfixOf "bad reply"
    second `shouldSatisfy` T.isInfixOf "refused: bad reply"
    spent >>= (`shouldBe` [("verifier", 0.25), ("verifier", 0.25)])

  it "askTwice stops at NoCredit" $ do
    (ask', calls) <- scripted [Left AskNoCredit, Right "never"]
    (spend, ev, spent, _) <- recorders
    r <- runRole (roleCtx @'Planner ask' Nothing spend ev) (askTwice (const (prompt "go")) id (\s -> pure (Right (saidText s) :: Either Text Text)))
    case r of
      NoCredit d -> d `shouldBe` 0
      _ -> expectationFailure "expected NoCredit"
    length <$> readIORef calls >>= (`shouldBe` 1)
    spent >>= (`shouldBe` [])

  it "check runs under the lock" $ withSystemTempDirectory "fisher-agent" $ \root -> do
    active <- newIORef (0 :: Int)
    most <- newIORef (0 :: Int)
    let enter = do
          n <- atomicModifyIORef' active (\a -> (a + 1, a + 1))
          atomicModifyIORef' most (\m -> (max m n, ()))
        leave = atomicModifyIORef' active (\a -> (a - 1, ()))
        lean = LeanRun $ \_ _ -> do
          enter
          threadDelay 10000
          leave
          pure (0, "")
    lock <- newMVar ()
    let ck = Checker lean (AxiomPolicy standardAxioms False) root "Main.lean" lock
    (spend, ev, _, _) <- recorders
    (ask', _) <- scripted []
    let ctx = roleCtx @'Verifier ask' (Just ck) spend ev
    -- another thread's check holds the lock for 200 ms
    takeMVar lock
    enter
    done <- newEmptyMVar
    _ <- forkIO (runRole ctx (check "theorem t : True := trivial" ["t"]) >>= putMVar done)
    threadDelay 200000
    tryReadMVar done >>= (`shouldSatisfy` isNothing)
    leave
    putMVar lock ()
    p <- takeMVar done
    prExit p `shouldBe` 0
    readIORef most >>= (`shouldBe` 1)

  it "askTwice's reader runs in the role monad: its check is the role's, and its event is tagged" $ do
    (ask', calls) <- scripted [Right "first", Right "second"]
    (spend, ev, spent, events) <- recorders
    let readReply s = do
          event "read" (String (saidText s))
          pure (if saidText s == "second" then Right (saidText s) else Left "not yet")
    r <- runRole (roleCtx @'Planner ask' Nothing spend ev) (askTwice (const (prompt "go")) id readReply)
    case r of
      Asked d (Right t) -> (d, t) `shouldBe` (0.5, "second")
      _ -> expectationFailure "expected Asked with a reply"
    length <$> readIORef calls >>= (`shouldBe` 2)
    events >>= (`shouldBe` [("planner", "read"), ("planner", "read")])
    spent >>= (`shouldBe` [("planner", 0.25), ("planner", 0.25)])

  it "withChecker runs its action with the role's checker, under the lock" $ withSystemTempDirectory "fisher-agent" $ \root -> do
    lock <- newMVar ()
    let ck = Checker (LeanRun (\_ _ -> pure (0, ""))) (AxiomPolicy standardAxioms False) root "Main.lean" lock
    (spend, ev, _, _) <- recorders
    (ask', _) <- scripted []
    (rel, held) <- runRole (roleCtx @'Verifier ask' (Just ck) spend ev) (withChecker (\c -> (,) (ckRel c) . isNothing <$> tryReadMVar (ckLock c)))
    rel `shouldBe` "Main.lean"
    -- the lock is taken while the action runs, and given back after
    held `shouldBe` True
    tryReadMVar lock >>= (`shouldSatisfy` (not . isNothing))

  it "excerpt and oneLineOf keep the role of what was said" $ do
    (ask', _) <- scripted [Right "```blueprint\nsplit on n\n```"]
    (spend, ev, _, _) <- recorders
    r <- runRole (roleCtx @'Planner ask' Nothing spend ev) (ask (prompt "go"))
    case r of
      Right (s, _) -> do
        let piece = excerpt (\t -> if "split" `T.isInfixOf` t then Right (T.strip (T.drop 13 (T.dropEnd 4 t)), T.length t) else Left ("no split" :: Text)) s
        fmap (\(p, n) -> (partText (toPrompt p), n)) piece `shouldBe` Right ("split on n", 27)
        fmap fst (excerpt (const (Left ("refused" :: Text) :: Either Text (Text, ()))) s) `shouldBe` Left "refused"
        -- a piece that is not in the reply is a programming error
        evaluate (excerpt (const (Right ("split on m", ()) :: Either Text (Text, ()))) s) `shouldThrow` anyErrorCall
        partText (toPrompt (oneLineOf 300 s)) `shouldBe` "```blueprint split on n ```"
        partText (toPrompt (oneLineOf 15 s)) `shouldBe` "```blueprint sp"
      Left _ -> expectationFailure "expected a reply"

  it "a judge context has no checker" $ do
    (ask', _) <- scripted []
    (spend, ev, _, _) <- recorders
    let ctx = roleCtx @'Judge ask' Nothing spend ev
    isNothing (rcCheck ctx) `shouldBe` True
