-- | The @smt_check@ tool (Fisher only, the native agent's): run z3 or cvc5
-- on SMT-LIB text through 'prepareQuery' and 'runQuery' and return the
-- verdict, the model, the options it stripped, the solver's error text and
-- the time taken.
--
-- The solver runs on this machine, not through the terminal tool's
-- sandbox: 'runQuery' already gives it a temp file for the query (never
-- argv), stdin from /dev/null, a 64 KB output cap and a kill one second
-- after its own limit.
module Fisher.Tools.Smt
  ( smtCheckToolDef
  , smtCheckToolDefWith
  , SmtDeps (..)
  , smtToolFor
  , registerSmtTool
  , queryLimit
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory (findExecutable)

import Fisher.Py.Value
import Fisher.Smt
import Fisher.Tools.Registry (Registry, ToolDef (..), appendToToolsets, register)

-- | How the tool finds a solver by name and runs one ('findSolverWith' and
-- 'runQuery' in production; stand-ins in tests).
data SmtDeps = SmtDeps
  { sdLookup :: Text -> IO (Maybe Solver)
  , sdRun :: Solver -> Int -> Text -> IO SmtResult
  }

-- | The longest query accepted, in characters.
queryLimit :: Int
queryLimit = 65536

-- | The tool on the configured solver (resolved once, when the tools are
-- built); another solver named in a call is looked up on PATH then.
smtCheckToolDef :: SmtConfig -> Solver -> ToolDef
smtCheckToolDef = smtCheckToolDefWith (productionDeps (findExecutable . T.unpack))

-- | 'findSolverWith' on the given lookup for a named solver, and 'runQuery'.
productionDeps :: (Text -> IO (Maybe FilePath)) -> SmtDeps
productionDeps which = SmtDeps (\n -> findSolverWith which defaultSmtConfig {smtcSolver = n}) runQuery

smtCheckToolDefWith :: SmtDeps -> SmtConfig -> Solver -> ToolDef
smtCheckToolDefWith deps cfg solver =
  ToolDef
    { tdName = "smt_check", tdToolset = "smt", tdSchema = schema
    , tdHandler = \_ args -> handle deps cfg solver args
    , tdCheck = Just (pure True), tdRequiresEnv = [], tdEmoji = "\129518" }

-- | The tool when SMT is on and the lookup (the caller's @which@) finds a
-- solver, else Nothing.
smtToolFor :: (Text -> IO (Maybe FilePath)) -> SmtConfig -> IO (Maybe ToolDef)
smtToolFor which cfg
  | not (smtcEnabled cfg) = pure Nothing
  | otherwise = fmap (smtCheckToolDefWith (productionDeps which) cfg) <$> findSolverWith which cfg

-- | Register the tool when 'smtToolFor' gives one, and add it to every
-- @fisher-*@ toolset so the native agent's default toolsets carry it.
registerSmtTool :: (Text -> IO (Maybe FilePath)) -> Registry -> SmtConfig -> IO ()
registerSmtTool which reg cfg =
  smtToolFor which cfg >>= mapM_ (\td -> register reg td >> appendToToolsets reg "fisher-" [tdName td])

schema :: PyVal
schema =
  PDict
    [ ("name", PStr "smt_check")
    , ( "description"
      , PStr
          "Run an SMT solver (z3 or cvc5) on SMT-LIB 2 text and get sat, unsat, unknown or error back, with the model's \
          \constants after sat. (check-sat) and (get-model) are added when the query lacks them; set-option forms that \
          \name a file or a channel are removed and listed in notes. Use it to test an arithmetic or bit-vector side \
          \condition or to look for a counterexample before you try to prove it." )
    , ( "parameters"
      , PDict
          [ ("type", PStr "object")
          , ( "properties"
            , PDict
                [ ("query", PDict [("type", PStr "string"), ("description", PStr "The SMT-LIB 2 text, at most 64 KB.")])
                , ( "solver"
                  , PDict
                      [ ("type", PStr "string"), ("enum", PList [PStr "z3", PStr "cvc5"])
                      , ("description", PStr "The solver to run; the configured one when omitted.") ] )
                , ( "timeout_secs"
                  , PDict
                      [ ("type", PStr "integer")
                      , ("description", PStr "Seconds before the solver gives up; the configured limit when omitted, and never above it.") ] )
                , ( "models"
                  , PDict
                      [ ("type", PStr "boolean"), ("default", PBool True)
                      , ("description", PStr "Ask for the model after sat.") ] )
                ] )
          , ("required", PList [PStr "query"])
          ] )
    ]

errorJson :: Text -> Text
errorJson e = dumpsUnicode (PDict [("error", PStr e)])

handle :: SmtDeps -> SmtConfig -> Solver -> PyVal -> IO Text
handle deps cfg configured args = case (,,) <$> query <*> secs <*> wantModel of
  Left e -> pure (errorJson e)
  Right (q, n, m) -> pickSolver >>= either (pure . errorJson) (run q n m)
  where
    arg k = case get k args of
      Just PNull -> Nothing
      v -> v
    query = case arg "query" of
      Nothing -> Left "query is required"
      Just (PStr q)
        | T.length q > queryLimit -> Left "query over 64 KB"
        | otherwise -> Right q
      Just _ -> Left "query must be a string"
    cap = max 1 (smtcTimeoutSecs cfg)
    clamp n = fromInteger (max 1 (min (toInteger cap) n))
    -- an integer, or a float with no fraction (2.0)
    secs = case arg "timeout_secs" of
      Nothing -> Right cap
      Just (PInt n) -> Right (clamp n)
      Just (PFloat d)
        | not (isNaN d || isInfinite d), d == fromInteger (truncate d) -> Right (clamp (truncate d))
      Just _ -> Left "timeout_secs must be an integer"
    -- a boolean, or the string true or false
    wantModel = case arg "models" of
      Nothing -> Right True
      Just (PBool b) -> Right b
      Just (PStr t)
        | T.toLower t == "true" -> Right True
        | T.toLower t == "false" -> Right False
      Just _ -> Left "models must be a boolean"
    pickSolver = case get "solver" args of
      Just (PStr n)
        | n `notElem` ["z3", "cvc5"] -> pure (Left ("unknown solver: " <> n <> " (use z3 or cvc5)"))
        | n == solverName configured -> pure (Right configured)
        | otherwise -> maybe (Left (n <> " is not on PATH")) Right <$> sdLookup deps n
      _ -> pure (Right configured)
    run q n m s = do
      let (body, notes) = prepareQuery m q
      t0 <- getMonotonicTime
      res <- sdRun deps s n body
      t1 <- getMonotonicTime
      pure (dumpsUnicode (resultJson notes (t1 - t0) res))

resultJson :: [Text] -> Double -> SmtResult -> PyVal
resultJson notes dt res =
  PDict
    [ ("result", PStr verdict)
    , ("model", PList [PDict [("name", PStr n), ("sort", PStr (sortText s)), ("value", valueJson v)] | (n, s, v) <- assigns])
    , ("functions", PList (map PStr funs))
    , ("notes", PList (map PStr (notes <> extra)))
    , ("stderr", PStr err)
    , ("seconds", PFloat (fromIntegral (round (dt * 100) :: Integer) / 100))
    ]
  where
    (verdict, assigns, funs, err, extra) = case res of
      Sat m -> ("sat", mAssign m, mFunctions m, "", [])
      Unsat -> ("unsat", [], [], "", [])
      Unknown why -> ("unknown", [], [], "", ["unknown: " <> why | why /= "unknown"])
      SolverError e -> ("error", [], [], T.take 2048 e, [])

sortText :: Sort -> Text
sortText = \case
  SInt -> "Int"
  SBool -> "Bool"
  SBitVec w -> "(_ BitVec " <> T.pack (show w) <> ")"
  SOther t -> t

valueJson :: Value -> PyVal
valueJson = \case
  VInt n -> PInt n
  VBool b -> PBool b
  VBitVec _ n -> PInt n
  VOther t -> PStr t
