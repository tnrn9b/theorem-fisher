-- | The @smt_check@ tool: its schema, the argument checks (the 64 KB cap,
-- the timeout clamp, the solver name), the result shape on a fake solver,
-- and the options 'prepareQuery' strips reported as notes.
module Fisher.Tools.SmtSpec (spec) where

import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Py.Value
import Fisher.Smt (Model (..), SmtConfig (..), SmtResult (..), Solver (..), Sort (..), defaultSmtConfig)
import qualified Fisher.Smt as Smt
import Fisher.Tools.Registry
import Fisher.Tools.Smt

cfg :: SmtConfig
cfg = defaultSmtConfig {smtcTimeoutSecs = 10}

-- | A tool whose runner records (solver, seconds, query) and answers @r@;
-- only z3 is on the fake PATH.
recording :: IORef [(Text, Int, Text)] -> SmtResult -> ToolDef
recording calls r = smtCheckToolDefWith deps cfg (Z3 "/fake/z3")
  where
    deps =
      SmtDeps
        { sdLookup = \n -> pure (if n == "z3" then Just (Z3 "/fake/z3") else Nothing)
        , sdRun = \s secs q -> modifyIORef calls (<> [(Smt.solverName s, secs, q)]) >> pure r }

call :: ToolDef -> [(Text, PyVal)] -> IO PyVal
call td args = do
  out <- tdHandler td defaultCtx (PDict args)
  either (\e -> fail ("not JSON: " <> e <> ": " <> T.unpack out)) pure (loads (TE.encodeUtf8 out))

dictKeys :: PyVal -> [Text]
dictKeys = \case
  PDict kvs -> map fst kvs
  _ -> []

spec :: Spec
spec = do
  describe "smt_check" $ do
    it "schema names the four parameters" $ do
      ref <- newIORef []
      let td = recording ref Unsat
          params = get "parameters" (tdSchema td)
          props = params >>= get "properties"
      (tdName td, tdToolset td) `shouldBe` ("smt_check", "smt")
      get "name" (tdSchema td) `shouldBe` Just (PStr "smt_check")
      fmap dictKeys props `shouldBe` Just ["query", "solver", "timeout_secs", "models"]
      (params >>= get "required") `shouldBe` Just (PList [PStr "query"])
      (props >>= get "solver" >>= get "enum") `shouldBe` Just (PList [PStr "z3", PStr "cvc5"])
      (props >>= get "timeout_secs" >>= get "type") `shouldBe` Just (PStr "integer")
      (props >>= get "models" >>= get "default") `shouldBe` Just (PBool True)
      fmap tsTools (lookup "smt" builtinToolsets) `shouldBe` Just ["smt_check"]
      fmap tsDescription (lookup "smt" builtinToolsets) `shouldBe` Just "Run an SMT solver on SMT-LIB text"

    it "rejects an oversized query" $ do
      ref <- newIORef []
      let td = recording ref Unsat
      out <- tdHandler td defaultCtx (PDict [("query", PStr (T.replicate 65537 "a"))])
      out `shouldBe` "{\"error\": \"query over 64 KB\"}"
      readIORef ref `shouldReturn` []
      r <- call td [("query", PStr (T.replicate 65536 " "))]
      get "result" r `shouldBe` Just (PStr "unsat")

    it "a missing or non-string query is an argument error" $ do
      ref <- newIORef []
      let td = recording ref Unsat
      tdHandler td defaultCtx (PDict []) `shouldReturn` "{\"error\": \"query is required\"}"
      tdHandler td defaultCtx (PDict [("query", PNull)]) `shouldReturn` "{\"error\": \"query is required\"}"
      tdHandler td defaultCtx (PDict [("query", PInt 3)]) `shouldReturn` "{\"error\": \"query must be a string\"}"
      tdHandler td defaultCtx (PDict [("query", PList [PStr "(assert true)"])]) `shouldReturn` "{\"error\": \"query must be a string\"}"
      readIORef ref `shouldReturn` []

    it "accepts true and false strings and integral floats, and rejects other types" $ do
      ref <- newIORef []
      let td = recording ref Unsat
          q = ("query", PStr "(assert true)")
      _ <- call td [q, ("models", PStr "false"), ("timeout_secs", PFloat 4.0)]
      _ <- call td [q, ("models", PStr "True"), ("timeout_secs", PNull)]
      map (\(_, s, b) -> (s, "(get-model)" `T.isInfixOf` b)) <$> readIORef ref `shouldReturn` [(4, False), (10, True)]
      tdHandler td defaultCtx (PDict [q, ("timeout_secs", PFloat 2.5)]) `shouldReturn` "{\"error\": \"timeout_secs must be an integer\"}"
      tdHandler td defaultCtx (PDict [q, ("timeout_secs", PStr "5")]) `shouldReturn` "{\"error\": \"timeout_secs must be an integer\"}"
      tdHandler td defaultCtx (PDict [q, ("models", PStr "maybe")]) `shouldReturn` "{\"error\": \"models must be a boolean\"}"
      tdHandler td defaultCtx (PDict [q, ("models", PInt 1)]) `shouldReturn` "{\"error\": \"models must be a boolean\"}"
      length <$> readIORef ref `shouldReturn` 2

    it "clamps the timeout" $ do
      ref <- newIORef []
      let td = recording ref Unsat
      mapM_ (\a -> call td (("query", PStr "(assert true)") : a)) [[("timeout_secs", PInt 300)], [("timeout_secs", PInt 3)], [], [("timeout_secs", PInt 0)]]
      map (\(_, s, _) -> s) <$> readIORef ref `shouldReturn` [10, 3, 10, 1]

    it "unknown solver name" $ do
      ref <- newIORef []
      let td = recording ref Unsat
      out <- tdHandler td defaultCtx (PDict [("query", PStr "(assert true)"), ("solver", PStr "cvc5")])
      out `shouldBe` "{\"error\": \"cvc5 is not on PATH\"}"
      bad <- call td [("query", PStr "(assert true)"), ("solver", PStr "yices")]
      get "error" bad `shouldBe` Just (PStr "unknown solver: yices (use z3 or cvc5)")
      readIORef ref `shouldReturn` []
      _ <- call td [("query", PStr "(assert true)"), ("solver", PStr "z3")]
      map (\(s, _, _) -> s) <$> readIORef ref `shouldReturn` ["z3"]

    it "sat result shape (fake solver)" $ withSystemTempDirectory "fisher-smt-tool" $ \tmp -> do
      let exe = tmp </> "z3"
      BS.writeFile exe "#!/bin/sh\nprintf 'sat\\n(model\\n  (define-fun x () Int (- 3))\\n  (define-fun b () Bool true)\\n  (define-fun f ((a Int)) Int a)\\n)\\n'\n"
      getPermissions exe >>= setPermissions exe . setOwnerExecutable True
      r <- call (smtCheckToolDef cfg (Z3 exe)) [("query", PStr "(declare-const x Int)\n(assert (< x 0))")]
      dictKeys r `shouldBe` ["result", "model", "functions", "notes", "stderr", "seconds"]
      get "result" r `shouldBe` Just (PStr "sat")
      get "model" r
        `shouldBe` Just
          ( PList
              [ PDict [("name", PStr "x"), ("sort", PStr "Int"), ("value", PInt (-3))]
              , PDict [("name", PStr "b"), ("sort", PStr "Bool"), ("value", PBool True)] ] )
      get "functions" r `shouldBe` Just (PList [PStr "f"])
      get "notes" r `shouldBe` Just (PList [])
      get "stderr" r `shouldBe` Just (PStr "")
      case get "seconds" r of
        Just (PFloat s) -> s `shouldSatisfy` (>= 0)
        Just (PInt s) -> s `shouldSatisfy` (>= 0)
        other -> expectationFailure ("seconds: " <> show other)
      -- an error keeps its message in stderr
      ref <- newIORef []
      e <- call (recording ref (SolverError "(error \"line 1: bad\")")) [("query", PStr "(assert")]
      (get "result" e, get "stderr" e, get "model" e) `shouldBe` (Just (PStr "error"), Just (PStr "(error \"line 1: bad\")"), Just (PList []))
      u <- call (recording ref (Unknown "killed at cap")) [("query", PStr "(assert true)")]
      (get "result" u, get "notes" u) `shouldBe` (Just (PStr "unknown"), Just (PList [PStr "unknown: killed at cap"]))
      s <- call (recording ref (Sat (Model [("v", SBitVec 8, Smt.VBitVec 8 255)] []))) [("query", PStr "(assert true)")]
      get "model" s `shouldBe` Just (PList [PDict [("name", PStr "v"), ("sort", PStr "(_ BitVec 8)"), ("value", PInt 255)]])

    it "stripped options appear in notes" $ do
      ref <- newIORef []
      let td = recording ref Unsat
      r <- call td [("query", PStr "(set-option :trace-file \"/tmp/t\")\n(declare-const x Int)\n"), ("models", PBool False)]
      get "notes" r `shouldBe` Just (PList [PStr "removed option: (set-option :trace-file \"/tmp/t\")"])
      [q] <- map (\(_, _, q) -> q) <$> readIORef ref
      q `shouldSatisfy` (not . T.isInfixOf "set-option")
      q `shouldSatisfy` T.isInfixOf "(check-sat)"
      q `shouldSatisfy` (not . T.isInfixOf "(get-model)")
