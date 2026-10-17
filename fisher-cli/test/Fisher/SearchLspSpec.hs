-- | The search's 'Lsp' over fisher-lean, against a stand-in language
-- server: a 'Wire' that answers the messages fisher-lean's client sends
-- (open, change, close, the diagnostics barrier, the goal) from canned
-- rules on the document's text, and logs what it was sent.
module Fisher.SearchLspSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Cli.SearchLsp
import Fisher.Lean.Client (ClientEnv (..))
import Fisher.Lean.Session (LeanEnv (..), defaultLeanEnv)
import Fisher.Lean.Transport (Wire (..))
import Fisher.Py.Value
import Fisher.Search.Auto

data Srv = Srv
  { sDocs :: IORef (M.Map Text Text)
  , sSent :: IORef [PyVal]
  -- ^ what the client sent, oldest first
  , sOut :: Chan (Either Text PyVal)
  , sBuf :: IORef BS.ByteString
  }

-- | The server's rules: the diagnostics of a text (0-based lines; the
-- columns as the server counts them), and the goals at a position.
data Rules = Rules
  { rDiags :: Text -> [PyVal]
  , rGoals :: Text -> Int -> Int -> [Text]
  , rHang :: Text -> Bool
  -- ^ never answer the barrier for this text
  }

diag :: Int -> Int -> Int -> Text -> PyVal
diag l c sev m =
  let p = PDict [("line", PInt (fromIntegral l)), ("character", PInt (fromIntegral c))]
      rng = PDict [("start", p), ("end", p)]
   in PDict [("range", rng), ("fullRange", rng), ("severity", PInt (fromIntegral sev)), ("message", PStr m)]

-- | A sorry warning on the first line while the text has one; an error on
-- the line holding @bad@ (an unrelated declaration) and on any line holding
-- @omega@; goals left by @simp@.
rules0 :: Rules
rules0 = Rules
  { rDiags = \t -> let numbered = zip [0 ..] (T.splitOn "\n" t) in
      [diag 0 8 2 "declaration uses 'sorry'" | "sorry" `T.isInfixOf` t]
        <> [diag i 0 1 "unknown identifier 'bad'" | (i, l) <- numbered, "bad" `T.isInfixOf` l]
        <> [diag i 2 1 "omega could not prove the goal" | (i, l) <- numbered, "omega" `T.isInfixOf` l]
  , rGoals = \t _ _ -> ["⊢ P" | "simp" `T.isInfixOf` t]
  , rHang = T.isInfixOf "hang" }

newSrv :: IO Srv
newSrv = Srv <$> newIORef M.empty <*> newIORef [] <*> newChan <*> newIORef BS.empty

wire :: Srv -> Rules -> Wire
wire srv rules = Wire
  { wWrite = \b -> do
      modifyIORef' (sBuf srv) (<> b)
      let drain = do
            buf <- readIORef (sBuf srv)
            case frameOf buf of
              Just (v, rest) -> writeIORef (sBuf srv) rest >> handle v >> drain
              Nothing -> pure ()
      drain
  , wRead = readChan (sOut srv)
  , wClose = pure ()
  , wStderr = pure "" }
  where
    send = writeChan (sOut srv) . Right
    reply i r = send (PDict [("jsonrpc", PStr "2.0"), ("id", i), ("result", r)])
    uriOf p = maybe "" pyStrOf (get "textDocument" p >>= get "uri")
    handle m = do
      modifyIORef' (sSent srv) (<> [m])
      let p = maybe (PDict []) id (get "params" m)
      case (get "method" m, get "id" m) of
        (Just (PStr "initialize"), Just i) -> reply i (PDict [("capabilities", PDict [])])
        (Just (PStr "textDocument/didOpen"), _) ->
          modifyIORef' (sDocs srv) (M.insert (uriOf p) (maybe "" pyStrOf (get "textDocument" p >>= get "text")))
        (Just (PStr "textDocument/didChange"), _) -> case get "contentChanges" p of
          Just (PList (ch : _)) -> modifyIORef' (sDocs srv) (M.insert (uriOf p) (maybe "" pyStrOf (get "text" ch)))
          _ -> pure ()
        (Just (PStr "textDocument/didClose"), _) -> modifyIORef' (sDocs srv) (M.delete (uriOf p))
        (Just (PStr "textDocument/waitForDiagnostics"), Just i) -> do
          let uri = maybe "" pyStrOf (get "uri" p)
          t <- M.findWithDefault "" uri <$> readIORef (sDocs srv)
          when (not (rHang rules t)) $ do
            send (PDict [ ("jsonrpc", PStr "2.0"), ("method", PStr "textDocument/publishDiagnostics")
                        , ("params", PDict [("uri", PStr uri), ("version", maybe PNull id (get "version" p)), ("diagnostics", PList (rDiags rules t))]) ])
            reply i (PDict [])
        (Just (PStr "$/lean/plainGoal"), Just i) -> do
          t <- M.findWithDefault "" (uriOf p) <$> readIORef (sDocs srv)
          let pos = maybe (PDict []) id (get "position" p)
              n k = case get k pos of Just (PInt x) -> fromIntegral x; _ -> 0
          reply i (PDict [("goals", PList (map PStr (rGoals rules t (n "line") (n "character"))))])
        (Just _, Just i) -> reply i PNull
        _ -> pure ()

frameOf :: BS.ByteString -> Maybe (PyVal, BS.ByteString)
frameOf b = do
  let (hdr, rest) = BS.breakSubstring "\r\n\r\n" b
  if BS.null rest then Nothing else Just ()
  (n, _) <- BC.readInt (BC.dropWhile (== ' ') (BS.drop 15 hdr))
  let body = BS.take n (BS.drop 4 rest)
  if BS.length body < n then Nothing else either (const Nothing) (\v -> Just (v, BS.drop (4 + n) rest)) (loads body)

envFor :: Srv -> Rules -> LeanEnv
envFor srv rules = defaultLeanEnv
  { leGetEnv = \_ -> pure Nothing
  , leClientEnv = ClientEnv (\_ -> pure (wire srv rules)) (\_ _ -> pure (pure ())) (pure 1) getMonotonicTime }

-- | A project with F.lean holding the source, and the Lsp over it.
withLsp :: Rules -> (Srv -> Lsp -> IO ()) -> IO ()
withLsp rules k = withSystemTempDirectory "search-lsp" $ \root -> do
  writeFile (root </> "lean-toolchain") "leanprover/lean4:v4.24.0\n"
  writeFile (root </> "lakefile.toml") "name = \"p\"\n"
  BS.writeFile (root </> "F.lean") (BC.pack "")
  srv <- newSrv
  r <- openLspWith (envFor srv rules) 600 root "F.lean"
  case r of
    Left e -> expectationFailure ("did not start: " <> T.unpack e)
    Right lsp -> k srv lsp >> lspClose lsp

methodOf :: PyVal -> Text
methodOf m = maybe "" pyStrOf (get "method" m)

goalPositions :: Srv -> IO [(Int, Int)]
goalPositions srv = do
  ms <- readIORef (sSent srv)
  pure [ (n "line" pos, n "character" pos)
       | m <- ms, methodOf m == "$/lean/plainGoal"
       , Just pos <- [get "params" m >>= get "position"]
       , let n k v = case get k v of Just (PInt x) -> fromIntegral x; _ -> -1 ]

src3 :: Text
src3 = T.intercalate "\n" ["theorem t : P := by", "  intro x", "  sorry", "", "theorem other : Q :=", "bad", ""]

spec :: Spec
spec = describe "the search's Lsp over fisher-lean" $ do
  it "sends a hole after ∀ ℝ≥0∞ at its UTF-16 column (a code point past the BMP counts two)" $ do
    let line = "theorem u (𝕜 : Type) : ∀ ℝ≥0∞ : Nat, True := by sorry"
        col = T.length (fst (T.breakOn "sorry" line))
        h = Hole 1 col False
    withLsp rules0 $ \srv lsp -> do
      _ <- lspGoal lsp line h
      _ <- lspAttempts lsp line h ["trivial"]
      ps <- goalPositions srv
      -- the goal at the sorry, then where the snippet ends: one more unit for 𝕜
      ps `shouldBe` [(0, col + 1), (0, col + T.length "trivial" + 1)]
  it "maps the attempts to trials in snippet order, with only the snippet's and new diagnostics" $
    withLsp rules0 $ \_ lsp -> do
      r <- lspAttempts lsp src3 (Hole 3 2 False) ["rfl", "simp", "omega"]
      r `shouldBe` Right
        [ Trial "rfl" [] []
        , Trial "simp" ["⊢ P"] []
        , Trial "omega" [] [Diag 3 2 SevError "omega could not prove the goal"] ]
  it "keeps the file's own errors out of a trial when the snippet adds lines" $
    withLsp rules0 $ \_ lsp -> do
      r <- lspAttempts lsp src3 (Hole 3 2 False) ["constructor\nrfl"]
      r `shouldBe` Right [Trial "constructor\nrfl" [] []]
  it "places a term hole's snippet as Auto splices it, and asks the goal inside its parentheses" $
    withLsp rules0 $ \srv lsp -> do
      let s = "theorem t : P ∧ Q := ⟨sorry, sorry⟩"
      _ <- lspAttempts lsp s (Hole 1 29 True) ["rfl"]
      docs <- M.elems <$> readIORef (sDocs srv)
      docs `shouldContain` ["theorem t : P ∧ Q := ⟨sorry, (by rfl)⟩\n"]
      ps <- goalPositions srv
      ps `shouldBe` [(0, 36)]
  it "the goal at a term hole is asked at a tactic sorry in its place" $
    withLsp rules0 $ \srv lsp -> do
      _ <- lspGoal lsp "theorem b : P := sorry" (Hole 1 17 True)
      docs <- M.elems <$> readIORef (sDocs srv)
      docs `shouldContain` ["theorem b : P := by sorry\n"]
      goalPositions srv `shouldReturn` [(0, 20)]
  it "the diagnostics of a whole file, 1-based" $
    withLsp rules0 $ \_ lsp ->
      lspDiagnostics lsp src3 `shouldReturn` Right [Diag 1 8 SevWarning "declaration uses 'sorry'", Diag 6 0 SevError "unknown identifier 'bad'"]
  it "a call cut off by a timeout is cancelled, and the next one runs on a fresh document" $
    withLsp rules0 $ \srv lsp -> do
      r0 <- timeout 300000 (lspAttempts lsp src3 (Hole 3 2 False) ["hang"])
      fmap (const ()) r0 `shouldBe` Nothing
      r <- lspAttempts lsp src3 (Hole 3 2 False) ["rfl"]
      r `shouldBe` Right [Trial "rfl" [] []]
      ms <- readIORef (sSent srv)
      let names = [(methodOf m, T.takeWhileEnd (/= '/') (maybe "" pyStrOf (get "params" m >>= get "textDocument" >>= get "uri"))) | m <- ms]
      [() | (m, _) <- names, m == "$/cancelRequest"] `shouldBe` [()]
      [d | (m, d) <- names, m == "textDocument/didClose"] `shouldBe` ["_fisher_auto_1_0.lean"]
      [d | (m, d) <- names, m == "textDocument/didOpen"] `shouldBe` ["_fisher_auto_1_0.lean", "_fisher_auto_2_0.lean"]
  it "a dead server answers Left, never an exception" $
    withLsp rules0 $ \srv lsp -> do
      writeChan (sOut srv) (Left "server killed")
      threadDelay 100000
      a <- lspAttempts lsp src3 (Hole 3 2 False) ["rfl"]
      g <- lspGoal lsp src3 (Hole 3 2 False)
      d <- lspDiagnostics lsp src3
      forM_ [either (const True) (const False) a, either (const True) (const False) g, either (const True) (const False) d] (`shouldBe` True)
  it "after lspClose every call is Left, and closing again is harmless" $
    withLsp rules0 $ \_ lsp -> do
      lspClose lsp
      lspClose lsp
      lspAttempts lsp src3 (Hole 3 2 False) ["rfl"] `shouldReturn` Left "the Lean server was closed"
  it "a hole that holds no sorry is Left" $
    withLsp rules0 $ \_ lsp ->
      lspAttempts lsp src3 (Hole 2 2 False) ["rfl"] `shouldReturn` Left "no sorry at line 2, column 2"
  it "a directory that is not a Lake project does not start" $
    withSystemTempDirectory "search-lsp" $ \root -> do
      srv <- newSrv
      r <- openLspWith (envFor srv rules0) 600 root "F.lean"
      either (T.isInfixOf "must contain") (const False) r `shouldBe` True
  it "a server that dies at startup is Left" $
    withSystemTempDirectory "search-lsp" $ \root -> do
      writeFile (root </> "lean-toolchain") "leanprover/lean4:v4.24.0\n"
      writeFile (root </> "lakefile.toml") "name = \"p\"\n"
      BS.writeFile (root </> "F.lean") (BC.pack "")
      srv <- newSrv
      writeChan (sOut srv) (Left "died at once")
      r <- openLspWith (envFor srv rules0) 600 root "F.lean"
      either (T.isInfixOf "Failed to start Lean language server") (const False) r `shouldBe` True
  it "a warm-up past its bound is Left, the server stopped, and quickly" $
    withSystemTempDirectory "search-lsp" $ \root -> do
      writeFile (root </> "lean-toolchain") "leanprover/lean4:v4.24.0\n"
      writeFile (root </> "lakefile.toml") "name = \"p\"\n"
      -- the stand-in never answers the diagnostics barrier for this text
      BS.writeFile (root </> "F.lean") (BC.pack "theorem hang : True := trivial\n")
      srv <- newSrv
      t0 <- getMonotonicTime
      r <- openLspWith (envFor srv rules0) 0.5 root "F.lean"
      t1 <- getMonotonicTime
      either id (const "started") r `shouldBe` "the warm-up took longer than 0.5 s"
      (t1 - t0) `shouldSatisfy` (< 3)
      -- closed: the scratch document is closed
      ms <- readIORef (sSent srv)
      [() | m <- ms, methodOf m == "textDocument/didClose"] `shouldBe` [()]
  describe "diagOf" $
    it "a diagnostic without a range is dropped" $
      diagOf (PDict [("message", PStr "m")]) `shouldBe` Nothing
