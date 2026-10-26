-- | Running z3 or cvc5 on SMT-LIB text: the query goes to a file in a fresh
-- temp directory, which is also the solver's working directory (the text
-- never goes onto argv), the solver reads nothing from stdin, its output is
-- cut at 64 KB and it is killed one second after its own time limit. The
-- model printed after @sat@ is parsed into constants and function names.
module Fisher.Smt
  ( Solver (..)
  , solverName
  , solverVersion
  , SmtConfig (..)
  , defaultSmtConfig
  , smtConfig
  , findSolver
  , findSolverWith
  , Sort (..)
  , Value (..)
  , Model (..)
  , SmtResult (..)
  , parseModel
  , prepareQuery
  , runQuery
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isDigit, isHexDigit, isSpace)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (findExecutable, makeAbsolute)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), hClose, hSetBinaryMode, withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process
import System.Timeout (timeout)

import Fisher.Config (smtEnabled, smtQueryTactic, smtSolver, smtTimeoutSecs)

data Solver = Z3 FilePath | Cvc5 FilePath
  deriving (Eq, Show)

solverName :: Solver -> Text
solverName (Z3 _) = "z3"
solverName (Cvc5 _) = "cvc5"

solverPath :: Solver -> FilePath
solverPath (Z3 p) = p
solverPath (Cvc5 p) = p

-- | The first line of @--version@, if the solver answers within 5 seconds.
solverVersion :: Solver -> IO (Maybe Text)
solverVersion s = do
  r <- try $ withFile "/dev/null" ReadMode $ \devNull -> do
    let cp = (proc (solverPath s) ["--version"]) {std_in = UseHandle devNull, std_out = CreatePipe, std_err = NoStream}
    bracket (createProcess cp) cleanup $ \(_, mout, _, ph) -> case mout of
      Nothing -> pure Nothing
      Just h -> do
        hSetBinaryMode h True
        r <- timeout 5000000 (BS.hGetSome h 4096 <* waitForProcess ph)
        pure $ case r of
          Nothing -> Nothing
          Just bs ->
            let l = T.strip (T.takeWhile (/= '\n') (decode bs))
             in if T.null l then Nothing else Just l
  pure $ either (\e -> const Nothing (e :: SomeException)) id r

data SmtConfig = SmtConfig
  { smtcEnabled :: Bool
  , smtcSolver :: Text
  , smtcTimeoutSecs :: Int
  , smtcQueryTactic :: Text
  }
  deriving (Eq, Show)

defaultSmtConfig :: SmtConfig
defaultSmtConfig = SmtConfig True "auto" 10 "smt_show"

-- | The four @fisher.smt.*@ keys; the timeout is clamped to 1..60.
smtConfig :: A.Value -> SmtConfig
smtConfig cfg = SmtConfig (smtEnabled cfg) (smtSolver cfg) (smtTimeoutSecs cfg) (smtQueryTactic cfg)

-- | Look the configured solver up on PATH. @auto@ tries z3, then cvc5; a
-- forced name that is missing (or unknown) gives Nothing.
findSolver :: SmtConfig -> IO (Maybe Solver)
findSolver = findSolverWith (findExecutable . T.unpack)

-- | 'findSolver' with the executable lookup given (the chat's @which@).
-- The path found is made absolute: the solver runs in a temp directory,
-- where a relative PATH entry would name nothing.
findSolverWith :: (Text -> IO (Maybe FilePath)) -> SmtConfig -> IO (Maybe Solver)
findSolverWith which c = case smtcSolver c of
  "z3" -> one "z3" Z3
  "cvc5" -> one "cvc5" Cvc5
  "auto" -> do
    z <- one "z3" Z3
    maybe (one "cvc5" Cvc5) (pure . Just) z
  _ -> pure Nothing
  where
    one n k = which n >>= traverse (fmap k . makeAbsolute)

data Sort = SInt | SBool | SBitVec Int | SOther Text
  deriving (Eq, Show)

data Value = VInt Integer | VBool Bool | VBitVec Int Integer | VOther Text
  deriving (Eq, Show)

data Model = Model
  { mAssign :: [(Text, Sort, Value)]
  , mFunctions :: [Text]
  }
  deriving (Eq, Show)

data SmtResult = Unsat | Sat Model | Unknown Text | SolverError Text
  deriving (Eq, Show)

-- S-expressions -------------------------------------------------------------

data SExp = Atom Text | List [SExp]
  deriving (Eq, Show)

-- | Tokens: parentheses, @|quoted|@ symbols, @"strings"@, atoms; @;@ comments
-- run to the end of the line.
tokenize :: Text -> [Text]
tokenize t = case T.uncons t of
  Nothing -> []
  Just (c, r)
    | isSpace c -> tokenize r
    | c == ';' -> tokenize (T.dropWhile (/= '\n') r)
    | c == '(' || c == ')' -> T.singleton c : tokenize r
    | c == '|' -> let (a, b) = T.break (== '|') r in T.cons '|' (a <> "|") : tokenize (T.drop 1 b)
    | c == '"' -> let (a, b) = T.break (== '"') r in T.cons '"' (a <> "\"") : tokenize (T.drop 1 b)
    | otherwise -> let (a, b) = T.break (\x -> isSpace x || x == '(' || x == ')' || x == ';') t in a : tokenize b

-- | Top-level expressions; an unclosed list is closed at the end of input.
parseSExps :: Text -> [SExp]
parseSExps = fst . go . tokenize
  where
    go :: [Text] -> ([SExp], [Text])
    go [] = ([], [])
    go (")" : ts) = ([], ts)
    go ("(" : ts) =
      let (inner, rest) = go ts
          (more, rest') = go rest
       in (List inner : more, rest')
    go (a : ts) = let (more, rest) = go ts in (Atom a : more, rest)

render :: SExp -> Text
render (Atom a) = a
render (List xs) = "(" <> T.unwords (map render xs) <> ")"

-- | The text after the result line. Accepts z3's @(model ...)@ wrapper and
-- cvc5's bare list of @define-fun@s.
parseModel :: Text -> Model
parseModel = modelOf . modelItems

modelOf :: [Either (Text, Sort, Value) Text] -> Model
modelOf items = Model [a | Left a <- items] [f | Right f <- items]

modelItems :: Text -> [Either (Text, Sort, Value) Text]
modelItems txt = concatMap collect (parseSExps txt)
  where
    collect :: SExp -> [Either (Text, Sort, Value) Text]
    collect (List (Atom "define-fun" : Atom n : List [] : srt : body : _)) =
      let s = toSort srt in [Left (n, s, toValue s body)]
    collect (List (Atom "define-fun" : Atom n : _)) = [Right n]
    collect (List xs) = concatMap collect xs
    collect _ = []

toSort :: SExp -> Sort
toSort (Atom "Int") = SInt
toSort (Atom "Bool") = SBool
toSort (List [Atom "_", Atom "BitVec", Atom n]) | Just w <- readNat n = SBitVec (fromInteger w)
toSort e = SOther (render e)

readNat :: Text -> Maybe Integer
readNat t
  | not (T.null t) && T.all isDigit t = Just (read (T.unpack t))
  | otherwise = Nothing

toValue :: Sort -> SExp -> Value
toValue SInt e | Just n <- intLit e = VInt n
toValue SBool (Atom "true") = VBool True
toValue SBool (Atom "false") = VBool False
toValue (SBitVec w) (Atom a) | Just n <- bitLit a = VBitVec w n
toValue (SBitVec w) (List [Atom "_", Atom bv, Atom wd])
  | Just n <- readNat =<< T.stripPrefix "bv" bv
  , Just _ <- readNat wd =
      VBitVec w n
toValue _ e = VOther (render e)

intLit :: SExp -> Maybe Integer
intLit (Atom a) = readNat a
intLit (List [Atom "-", Atom a]) = negate <$> readNat a
intLit _ = Nothing

bitLit :: Text -> Maybe Integer
bitLit a
  | Just h <- T.stripPrefix "#x" a, not (T.null h), T.all isHexDigit h = Just (digits 16 h)
  | Just b <- T.stripPrefix "#b" a, not (T.null b), T.all (`elem` ['0', '1']) b = Just (digits 2 b)
  | otherwise = Nothing
  where
    digits base = T.foldl' (\acc c -> acc * base + toInteger (digitToInt c)) 0

-- Queries -------------------------------------------------------------------

-- | Split source text into chunks: (True, a top-level parenthesised form)
-- or (False, whatever lies between forms). Comments, strings and quoted
-- symbols are skipped when matching parentheses.
chunks :: String -> [(Bool, String)]
chunks = go 0 [] []
  where
    -- depth, current chunk (reversed), finished chunks (reversed)
    go :: Int -> String -> [(Bool, String)] -> String -> [(Bool, String)]
    go _ cur acc [] = reverse (flush False cur acc)
    go d cur acc (c : cs)
      | c == ';' = let (a, b) = break (== '\n') cs in go d (reverse a ++ c : cur) acc b
      | c == '"' = let (a, b) = quoted '"' cs in go d (reverse a ++ c : cur) acc b
      | c == '|' = let (a, b) = quoted '|' cs in go d (reverse a ++ c : cur) acc b
      | c == '(' && d == 0 = go 1 [c] (flush False cur acc) cs
      | c == '(' = go (d + 1) (c : cur) acc cs
      | c == ')' && d == 1 = go 0 [] ((True, reverse (c : cur)) : acc) cs
      | c == ')' && d > 1 = go (d - 1) (c : cur) acc cs
      | otherwise = go d (c : cur) acc cs
    flush isForm cur acc = if null cur then acc else (isForm, reverse cur) : acc
    quoted q s = let (a, b) = break (== q) s in (a ++ take 1 b, drop 1 b)

-- | Does a @set-option@ name a path, a channel or a file? Looked at in the
-- key and the value; the @:produce-*@ switches are not about files.
riskyOption :: SExp -> Bool
riskyOption (List (Atom "set-option" : rest)) = any bad rest
  where
    bad (Atom a) = not (":produce-" `T.isPrefixOf` a) && hit (T.toLower a)
    bad (List xs) = any bad xs
    hit a = T.any (\c -> c == '/' || c == '\\') a || any (`T.isInfixOf` a) ["output-channel", "trace", "proof", "dump", "file"]
riskyOption _ = False

-- | The top-level commands a query may hold; any other form (@include@,
-- @get-proof@, solver extensions, and @echo@, which could print a forged
-- result line) is dropped.
allowedCommands :: [Text]
allowedCommands =
  [ "set-logic", "set-info", "set-option"
  , "declare-sort", "declare-const", "declare-fun", "declare-datatype", "declare-datatypes"
  , "define-sort", "define-fun", "define-fun-rec", "define-funs-rec"
  , "assert", "check-sat", "check-sat-assuming", "get-model", "get-value", "get-unsat-core"
  , "push", "pop", "reset", "reset-assertions", "exit"
  ]

-- | Add @(check-sat)@ and, when models are wanted, @(get-model)@ if the
-- query lacks them as top-level commands. Top-level forms outside
-- 'allowedCommands' are dropped (an @include@ would read any file), and so
-- are @set-option@ forms that name a path, a channel or a file (the solver
-- could write files with them). Each drop is a note: @dropped: @ and the
-- first 60 characters of the form, or @removed option: @ and the option. A
-- @check-sat-assuming@ counts as a check.
prepareQuery :: Bool -> Text -> (Text, [Text])
prepareQuery wantModel q = (body, notes)
  where
    cs = chunks (T.unpack q)
    parsed = [(isF, T.pack s, if isF then parseSExps (T.pack s) else []) | (isF, s) <- cs]
    headOf es = case es of [List (Atom h : _)] -> Just h; _ -> Nothing
    unknownCmd (isF, _, es) = isF && maybe True (`notElem` allowedCommands) (headOf es)
    risky (isF, _, es) = isF && any riskyOption es
    flat = T.unwords . T.words
    note x@(_, t, _)
      | unknownCmd x = Just ("dropped: " <> T.take 60 (flat t))
      | risky x = Just ("removed option: " <> flat t)
      | otherwise = Nothing
    notes = [n | x <- parsed, Just n <- [note x]]
    kept = [x | x <- parsed, Nothing <- [note x]]
    stripped = if null notes then q else T.concat [t | (_, t, _) <- kept]
    cmds = [h | (True, _, es) <- kept, Just h <- [headOf es]]
    needCheck = not (any (`elem` ["check-sat", "check-sat-assuming"]) cmds)
    needModel = wantModel && "get-model" `notElem` cmds
    extra = ["(check-sat)" | needCheck] <> ["(get-model)" | needModel]
    body
      | null extra = stripped
      | otherwise = (if T.null stripped || T.isSuffixOf "\n" stripped then stripped else stripped <> "\n") <> T.unlines extra

-- Running -------------------------------------------------------------------

outputCap :: Int
outputCap = 64 * 1024

decode :: BS.ByteString -> Text
decode = TE.decodeUtf8With TEE.lenientDecode

-- | Read a handle to EOF, keeping the first @cap@ bytes and dropping the rest.
drain :: Int -> Handle -> IO (BS.ByteString, Bool)
drain cap h = do
  hSetBinaryMode h True
  ref <- newIORef (0 :: Int, [] :: [BS.ByteString])
  let loop = do
        r <- try (BS.hGetSome h 8192)
        case r of
          Left e -> const (pure ()) (e :: SomeException)
          Right c
            | BS.null c -> pure ()
            | otherwise -> do
                modifyIORef' ref $ \(n, acc) ->
                  if n >= cap then (n + BS.length c, acc) else (n + BS.length c, BS.take (cap - n) c : acc)
                loop
  loop
  (n, acc) <- readIORef ref
  pure (BS.concat (reverse acc), n > cap)

cleanup :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanup (i, o, e, ph) = do
  mapM_ (\h -> try (hClose h) :: IO (Either SomeException ())) (catMaybes [i, o, e])
  _ <- try (terminateProcess ph) :: IO (Either SomeException ())
  _ <- try (waitForProcess ph) :: IO (Either SomeException ExitCode)
  pure ()

killGroup :: ProcessHandle -> IO ()
killGroup ph = do
  mp <- getPid ph
  _ <- try (maybe (pure ()) (signalProcessGroup sigKILL) mp) :: IO (Either SomeException ())
  _ <- try (terminateProcess ph) :: IO (Either SomeException ())
  pure ()

-- | Run the solver on the query for at most @secs@ seconds (plus one before
-- it is killed). The query is written to a fresh temp directory and named on
-- argv; the solver runs with that directory as its working directory, stdin
-- is /dev/null, and the directory is removed afterwards.
runQuery :: Solver -> Int -> Text -> IO SmtResult
runQuery solver secs0 query = do
  let secs = max 1 (min 60 secs0)
  r <- try $ withSystemTempDirectory "fisher-smt" $ \dir -> do
    let path = dir </> "query.smt2"
    BS.writeFile path (TE.encodeUtf8 query)
    runFile dir path secs
  pure $ case r of
    Left e -> SolverError (T.take 2048 (T.pack (show (e :: SomeException))))
    Right x -> x
  where
    args p n = case solver of
      Z3 _ -> ["-smt2", "-T:" <> show n, p]
      Cvc5 _ -> ["--tlimit=" <> show n <> "000", "--produce-models", p]
    runFile dir p n = withFile "/dev/null" ReadMode $ \devNull -> do
      let cp = (proc (solverPath solver) (args p n))
            {cwd = Just dir, std_in = UseHandle devNull, std_out = CreatePipe, std_err = CreatePipe, create_group = True}
      bracket (createProcess cp) cleanup $ \(_, mout, merr, ph) -> case (mout, merr) of
        (Just o, Just e) -> do
          outV <- newEmptyMVar
          errV <- newEmptyMVar
          t1 <- forkIO (drain outputCap o >>= putMVar outV)
          t2 <- forkIO (drain 2048 e >>= putMVar errV)
          done <- timeout ((n + 1) * 1000000) $ do
            _ <- waitForProcess ph
            (,) <$> takeMVar outV <*> takeMVar errV
          case done of
            Nothing -> do
              killGroup ph
              killThread t1
              killThread t2
              pure (Unknown "killed at cap")
            Just ((out, cut), (err, _)) -> pure (classify cut (decode out) (decode err))
        _ -> pure (SolverError "no pipes")

-- | Classify by the first non-blank line of stdout.
classify :: Bool -> Text -> Text -> SmtResult
classify cut out err = case dropWhile T.null (map T.strip (T.lines out)) of
  ("sat" : rest) ->
    -- when the output was cut, its last model item may be a fragment
    let items = modelItems (T.unlines rest)
     in Sat (modelOf (if cut && not (null items) then init items else items))
  ("unsat" : _) -> Unsat
  ("unknown" : _) -> Unknown "unknown"
  ("timeout" : _) -> Unknown "timeout"
  ls ->
    let e = T.strip err
        msg
          | not (T.null e) = e
          | otherwise = T.unlines (take 1 ls)
     in SolverError (T.take 2048 msg)
