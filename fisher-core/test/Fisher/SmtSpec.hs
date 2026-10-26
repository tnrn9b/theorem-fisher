module Fisher.SmtSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.List (isPrefixOf)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, makeAbsolute, removePathForcibly)
import System.FilePath (normalise, takeFileName, (</>))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Process (callProcess)
import Test.Hspec

import Fisher.Smt

fake :: IO Solver
fake = Z3 <$> makeAbsolute "test/fixtures/smt/fake-solver.sh"

withMode :: String -> IO a -> IO a
withMode m act = do
  setEnv "FAKE_SMT_MODE" m
  r <- act
  unsetEnv "FAKE_SMT_MODE"
  pure r

z3Model :: T.Text
z3Model =
  T.unlines
    [ "(model"
    , "  (define-fun x () Int (- 3))"
    , "  (define-fun b () Bool true)"
    , "  (define-fun v () (_ BitVec 8) #x1f)"
    , "  (define-fun f ((a!1 Int)) Int 0)"
    , ")"
    ]

expected :: [(T.Text, Sort, Value)]
expected = [("x", SInt, VInt (-3)), ("b", SBool, VBool True), ("v", SBitVec 8, VBitVec 8 31)]

spec :: Spec
spec = do
  it "parses a z3 model" $ do
    let m = parseModel z3Model
    mAssign m `shouldBe` expected
    mFunctions m `shouldBe` ["f"]

  it "parses a cvc5 model" $ do
    let m = parseModel "(\n(define-fun x () Int (- 3))\n(define-fun b () Bool true)\n(define-fun v () (_ BitVec 8) #b00011111)\n(define-fun f ((a!1 Int)) Int 0)\n)\n"
    mAssign m `shouldBe` expected
    mFunctions m `shouldBe` ["f"]

  it "prepareQuery adds the commands" $ do
    let (q, notes) = prepareQuery True "(declare-const x Int)\n(assert (> x 0))"
    notes `shouldBe` []
    T.count "(check-sat)" q `shouldBe` 1
    T.count "(get-model)" q `shouldBe` 1
    let full = "(assert true)\n(check-sat)\n(get-model)\n"
    prepareQuery True full `shouldBe` (full, [])

  it "prepareQuery strips path options" $ do
    let (q, notes) = prepareQuery True "(set-option :regular-output-channel \"/tmp/x\")\n(assert true)\n"
    q `shouldSatisfy` (not . T.isInfixOf "set-option")
    length notes `shouldBe` 1

  it "prepareQuery strips a set-option split over lines" $ do
    let (q, notes) = prepareQuery False "(set-option\n  :regular-output-channel \"/x\")\n(assert true)\n"
    q `shouldSatisfy` (not . T.isInfixOf "set-option")
    q `shouldSatisfy` T.isInfixOf "(assert true)"
    length notes `shouldBe` 1

  it "prepareQuery strips a set-option sharing a line with another command" $ do
    let (q, notes) = prepareQuery False "(assert true) (set-option :trace_file_name \"t\") (assert false)\n(set-option :proof-log p)\n"
    q `shouldSatisfy` (not . T.isInfixOf "set-option")
    q `shouldSatisfy` T.isInfixOf "(assert false)"
    length notes `shouldBe` 2

  it "prepareQuery keeps the produce switches" $
    fst (prepareQuery False "(set-option :produce-proofs true)\n(check-sat)\n")
      `shouldBe` "(set-option :produce-proofs true)\n(check-sat)\n"

  it "prepareQuery decides from the commands, not the text" $ do
    let (q, _) = prepareQuery True "; (check-sat) (get-model)\n(assert (= s \"(check-sat)\"))\n"
    T.count "\n(check-sat)\n" q `shouldBe` 1
    T.count "\n(get-model)\n" q `shouldBe` 1
    let full = "(assert true)\n(check-sat-assuming (a))\n(get-model)\n"
    prepareQuery True full `shouldBe` (full, [])

  it "prepareQuery drops an include with a note" $ do
    let (q, notes) = prepareQuery False "(include \"/etc/passwd\")\n(assert true)\n(check-sat)\n"
    q `shouldBe` "\n(assert true)\n(check-sat)\n"
    notes `shouldBe` ["dropped: (include \"/etc/passwd\")"]

  it "prepareQuery cuts a dropped form's note at 60 characters" $ do
    let long = "(get-proof " <> T.replicate 100 "x" <> ")"
        (_, notes) = prepareQuery False (long <> "\n(check-sat)\n")
    notes `shouldBe` ["dropped: " <> T.take 60 long]

  it "prepareQuery drops echo, which could forge the result line" $
    prepareQuery False "(echo \"sat\")\n(check-sat)\n" `shouldBe` ("\n(check-sat)\n", ["dropped: (echo \"sat\")"])

  it "findSolverWith makes a relative path absolute" $ do
    here <- makeAbsolute "."
    r <- findSolverWith (\_ -> pure (Just "bin/z3")) defaultSmtConfig
    r `shouldBe` Just (Z3 (here </> "bin/z3"))

  it "prepareQuery drops a form with no command head" $
    snd (prepareQuery False "((x))\n(check-sat)\n") `shouldBe` ["dropped: ((x))"]

  it "prepareQuery keeps every allowlisted command" $ do
    let full =
          T.unlines
            [ "(set-logic QF_LIA)"
            , "(set-info :status sat)"
            , "(set-option :produce-models true)"
            , "(declare-sort U 0)"
            , "(declare-const x Int)"
            , "(declare-fun f (Int) Int)"
            , "(declare-datatype P ((mk (fst Int))))"
            , "(declare-datatypes ((Q 0)) (((q))))"
            , "(define-sort I () Int)"
            , "(define-fun y () Int 1)"
            , "(define-fun-rec g ((a Int)) Int a)"
            , "(define-funs-rec ((h ((a Int)) Int)) (a))"
            , "(assert (> x 0))"
            , "(push 1)"
            , "(pop 1)"
            , "(check-sat)"
            , "(check-sat-assuming (true))"
            , "(get-model)"
            , "(get-value (x))"
            , "(get-unsat-core)"
            , "(reset-assertions)"
            , "(reset)"
            , "(exit)"
            ]
    prepareQuery True full `shouldBe` (full, [])

  it "runQuery runs the solver in a temp directory it removes" $ do
    s <- fake
    here <- makeAbsolute "."
    r <- withMode "pwd" (runQuery s 5 "(assert true)")
    case r of
      SolverError e -> do
        let cwd = T.unpack (T.takeWhile (/= ' ') (T.drop 4 (snd (T.breakOn "cwd=" e))))
            file = T.unpack (T.strip (T.drop 5 (snd (T.breakOn "file=" e))))
        cwd `shouldNotBe` here
        tmp <- getTemporaryDirectory >>= canonicalizePath
        cwd `shouldSatisfy` (tmp `isPrefixOf`)
        takeFileName file `shouldBe` "query.smt2"
        doesDirectoryExist cwd `shouldReturn` False
      other -> expectationFailure (show other)

  it "runQuery unsat" $ do
    s <- fake
    withMode "unsat" (runQuery s 5 "(assert false)") `shouldReturn` Unsat

  it "runQuery sat model" $ do
    s <- fake
    r <- withMode "sat" (runQuery s 5 "(assert true)")
    case r of
      Sat m -> mAssign m `shouldBe` expected
      other -> expectationFailure (show other)

  it "runQuery unknown" $ do
    s <- fake
    r <- withMode "unknown" (runQuery s 5 "(assert true)")
    case r of
      Unknown _ -> pure ()
      other -> expectationFailure (show other)

  it "runQuery garbage is SolverError with stderr" $ do
    s <- fake
    r <- withMode "garbage" (runQuery s 5 "(assert true)")
    case r of
      SolverError e -> e `shouldSatisfy` T.isInfixOf "boom"
      other -> expectationFailure (show other)

  it "runQuery cuts at 64 KB" $ do
    s <- fake
    r <- withMode "big" (runQuery s 5 "(assert true)")
    case r of
      Sat m -> do
        -- 3000 constants of about 50 bytes were printed; 64 KB holds fewer
        length (mAssign m) `shouldSatisfy` (< 1400)
        length (mAssign m) `shouldSatisfy` (> 1000)
      other -> expectationFailure (show other)

  it "sat without model at cap" $ do
    s <- fake
    t0 <- getCurrentTime
    r <- withMode "hang" (runQuery s 1 "(assert true)")
    t1 <- getCurrentTime
    case r of
      Unknown _ -> pure ()
      other -> expectationFailure (show other)
    (realToFrac (diffUTCTime t1 t0) :: Double) `shouldSatisfy` (< 3)

  it "smtConfig clamps the timeout" $ do
    let cfg n = A.object ["fisher" A..= A.object ["smt" A..= A.object ["timeout_secs" A..= (n :: Int)]]]
    smtcTimeoutSecs (smtConfig (cfg 0)) `shouldBe` 1
    smtcTimeoutSecs (smtConfig (cfg 600)) `shouldBe` 60
    smtConfig (A.object []) `shouldBe` defaultSmtConfig

  it "findSolver honours a forced missing name" $ do
    tmp <- getTemporaryDirectory
    let dir = normalise (tmp </> "fisher-smt-path-test")
    removePathForcibly dir
    createDirectoryIfMissing True dir
    writeFile (dir </> "z3") "#!/bin/sh\n"
    callProcess "chmod" ["+x", dir </> "z3"]
    old <- lookupEnv "PATH"
    setEnv "PATH" dir
    r <- findSolver defaultSmtConfig {smtcSolver = "cvc5"}
    a <- findSolver defaultSmtConfig
    maybe (unsetEnv "PATH") (setEnv "PATH") old
    removePathForcibly dir
    r `shouldBe` Nothing
    a `shouldBe` Just (Z3 (dir </> "z3"))
