-- | The automation pre-pass: before a model is asked for a goal, try a
-- fixed portfolio of tactics at each of its holes through Lean's language
-- server. Lean access comes in as an 'Lsp' record so this package does not
-- depend on fisher-lean; tests pass a fake.
--
-- Nothing here marks a goal proved: a fill found by Auto is only a
-- candidate, and the kernel check judges it like any model proof.
module Fisher.Search.Auto
  ( Hole (..)
  , Severity (..)
  , Diag (..)
  , Trial (..)
  , Lsp (..)
  , AutoConfig (..)
  , defaultAutoConfig
  , defaultTactics
  , holesIn
  , wrapSnippet
  , suggestion
  , winner
  , fillHole
  , fillHoleAt
  , cutAfter
  , cutAtHole
  , AutoFail (..)
  , guarded
  , autoHole
  , autoGoal
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (find, isPrefixOf, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import System.Timeout (timeout)

import Fisher.Search.Lean (Decl (..), declarations, findDecl)

-- | A @sorry@ in a proof.
data Hole = Hole
  { hLine :: Int
  -- ^ 1-based line
  , hCol :: Int
  -- ^ 0-based column of the @sorry@, in code points
  , hTerm :: Bool
  -- ^ a term (@:= sorry@): filled as @by \<tactic\>@
  }
  deriving (Eq, Show)

data Severity = SevError | SevWarning | SevInfo
  deriving (Eq, Show)

data Diag = Diag
  { dgLine :: Int
  , dgCol :: Int
  , dgSeverity :: Severity
  , dgMessage :: Text
  }
  deriving (Eq, Show)

-- | What Lean said about one snippet tried at a hole.
data Trial = Trial
  { tSnippet :: Text
  , tGoals :: [Text]
  -- ^ goals left after the snippet
  , tDiags :: [Diag]
  }
  deriving (Eq, Show)

-- | Lean's language server, as the search sees it. @Left@ means the
-- session failed; the caller turns the automation off.
data Lsp = Lsp
  { lspAttempts :: Text -> Hole -> [Text] -> IO (Either Text [Trial])
  -- ^ file content, hole, snippets: one 'Trial' per snippet, in order
  , lspGoal :: Text -> Hole -> IO (Either Text (Maybe Text))
  -- ^ the goal state at a hole
  , lspDiagnostics :: Text -> IO (Either Text [Diag])
  -- ^ the diagnostics of a whole file
  , lspClose :: IO ()
  , lspTimedOut :: IO ()
  -- ^ called when a call to this server, or a step around it, is cut off
  -- at its cap; the search sets it per step to log the timeout (the
  -- server's own value does nothing)
  }

data AutoConfig = AutoConfig
  { acTactics :: [Text]
  , acSeconds :: Double
  -- ^ wall cap per hole
  , acHeartbeats :: Int
  , acRepair :: Bool
  , acDisprove :: Bool
  , acRepairSeconds :: Double
  -- ^ wall cap on one Repair as a whole, on top of the per-call caps
  }
  deriving (Eq, Show)

defaultAutoConfig :: AutoConfig
defaultAutoConfig = AutoConfig defaultTactics 60 200000 True True 120

defaultTactics :: [Text]
defaultTactics = ["rfl", "decide", "omega", "simp_all", "aesop", "grind", "exact?"]

-- | The holes of a declaration's proof, in order: each token @sorry@
-- outside comments, strings and char literals.
--
-- A hole is a tactic hole when the @sorry@ is the first token of its line
-- (after indentation, a bullet @·@, or @case ... =>@) or directly follows
-- @;@, @<;>@, @=>@, @by@, or a combinator taking a tactic (@|@ as in
-- @first |@, @<|>@, @try@, @repeat@, @all_goals@, @any_goals@, @focus@;
-- not the term operators @<|@ and @||@). Every other hole is a term hole, and so is a
-- @sorry@ that is the whole of a term proof (@:= sorry@, on the
-- statement's line or below it).
holesIn :: Text -> Decl -> [Hole]
holesIn src d = go (0, False) True (zip [dStmtEnd d ..] proofLines)
  where
    proofLines = take (dEnd d - dStmtEnd d) (drop (dStmtEnd d) (T.splitOn "\n" src))
    stmtBy = endsWithBy (dStatement d)
    cutCol = case proofLines of
      (l : _) -> statementCut l (dStatement d)
      [] -> 0
    -- blank: nothing but blanks since the statement's cut so far
    go _ _ [] = []
    go st blank ((i, l) : rest) =
      let (masked, st') = maskLine st l
          from = if i == dStmtEnd d then cutCol else 0
          code = T.drop from masked
          hs =
            [ Hole (i + 1) (c + from) term
            | c <- sorryCols code
            , let pre = T.take c code
                  wholeProof = blank && not stmtBy && T.all isSpace pre
                  term = wholeProof || not (tacticPosition pre)
            ]
       in hs <> go st' (blank && T.all isSpace code) rest

-- | Whether a @sorry@ preceded on its line by this code stands where a
-- tactic goes.
tacticPosition :: Text -> Bool
tacticPosition pre =
  let p = T.strip pre
   in T.null p
        || p `elem` ["·", "."]
        || any (`T.isSuffixOf` p) [";", "<;>", "=>", "<|>"]
        || ("|" `T.isSuffixOf` p && not (any (`T.isSuffixOf` p) ["<|", "||"]))
        || any (endsWithWord p) ["by", "try", "repeat", "all_goals", "any_goals", "focus"]

-- | Where the statement stops on its last line: the column just past its
-- @:=@ (or @:= by@).
statementCut :: Text -> Text -> Int
statementCut line stmt =
  let lastL = last (T.splitOn "\n" stmt)
      (pre, hit) = T.breakOn lastL line
   in if T.null lastL || T.null hit then T.length line else T.length pre + T.length lastL

-- | The line with comment, string and char literal characters blanked,
-- column for column; the state is the block-comment depth and whether a
-- string is open, carried from line to line.
maskLine :: (Int, Bool) -> Text -> (Text, (Int, Bool))
maskLine st0 l = let (s, st) = go ' ' st0 (T.unpack l) in (T.pack s, st)
  where
    go _ st [] = ([], st)
    go prev (depth, inStr) cs@(c : rest)
      | depth > 0 = case cs of
          ('/' : '-' : r) -> blanks 2 (depth + 1, False) r
          ('-' : '/' : r) -> blanks 2 (depth - 1, False) r
          _ -> blanks 1 (depth, False) rest
      | inStr = case cs of
          ('\\' : _ : r) -> blanks 2 (0, True) r
          ('"' : r) -> blanks 1 (0, False) r
          _ -> blanks 1 (0, True) rest
      | otherwise = case cs of
          ('-' : '-' : _) -> (map (const ' ') cs, (0, False))
          ('/' : '-' : r) -> blanks 2 (1, False) r
          ('"' : r) -> blanks 1 (0, True) r
          ('\'' : r) | not (identChar prev), Just k <- charLit r -> blanks (k + 1) (0, False) (drop k r)
          _ -> let (s, st) = go c (0, False) rest in (c : s, st)
    blanks k st r = let (s, st') = go ' ' st r in (replicate k ' ' <> s, st')
    -- the length of a char literal's body and closing quote, after the
    -- opening quote: 'x', '"', '\n', '\x41', '\u{2200}'
    charLit r = case r of
      ('\\' : r') -> case break (== '\'') (take 12 r') of
        (body, '\'' : _) | not (null body) -> Just (length body + 2)
        _ -> Nothing
      (_ : '\'' : _) -> Just 2
      _ -> Nothing

identChar :: Char -> Bool
identChar c = isAlphaNum c || c `elem` ("_'!?" :: String)

-- | The columns of the token @sorry@ in masked code.
sorryCols :: Text -> [Int]
sorryCols t = go 0 ' ' (T.unpack t)
  where
    go :: Int -> Char -> String -> [Int]
    go _ _ [] = []
    go i prev cs@(c : rest)
      | "sorry" `isPrefixOf` cs
      , not (identChar prev || prev == '.')
      , case drop 5 cs of
          (x : _) -> not (identChar x)
          [] -> True =
          i : go (i + 5) 'y' (drop 5 cs)
      | otherwise = go (i + 1) c rest

endsWithBy :: Text -> Bool
endsWithBy t = endsWithWord t "by"

-- | Whether the text ends in the word (not in a longer identifier).
endsWithWord :: Text -> Text -> Bool
endsWithWord t w =
  let s = T.stripEnd t
      r = T.dropEnd (T.length w) s
   in w `T.isSuffixOf` s && (T.null r || not (identChar (T.last r) || T.last r == '.'))

-- | A snippet as it is sent: under the heartbeat cap.
wrapSnippet :: Int -> Text -> Text
wrapSnippet hb t = "set_option maxHeartbeats " <> T.pack (show hb) <> " in " <> t

-- | The text of a @Try this:@ suggestion (from @exact?@, @apply?@,
-- @aesop?@) in a trial's info or warning messages. Lean 4.33 prints it on
-- the lines below the heading, after @[apply]@ (and, from @aesop?@, after
-- a blank line):
--
-- > Try this:
-- >   [apply] exact Nat.le_add_right_of_le h
--
-- An older Lean put it on the heading's line. A suggestion over several
-- lines keeps their indentation relative to each other; its first line
-- starts the snippet.
suggestion :: Trial -> Maybe Text
suggestion tr =
  case [ s | Diag _ _ sev m <- tDiags tr, sev /= SevError
       , let (_, hit) = T.breakOn "Try this:" m, not (T.null hit)
       , Just s <- [suggestionText (T.drop (T.length "Try this:") hit)] ] of
    (s : _) -> Just s
    [] -> Nothing

-- | The suggestion in the text after @Try this:@.
suggestionText :: Text -> Maybe Text
suggestionText rest =
  case dropWhile (T.all isSpace) (T.splitOn "\n" rest) of
    [] -> Nothing
    (l : more) ->
      let first = T.strip (dropApply (T.strip l))
          -- the lines below the first, up to the next unindented line
          below = takeWhile (\x -> T.all isSpace x || " " `T.isPrefixOf` x) more
          body = reverse (dropWhile (T.all isSpace) (reverse below))
          ind = minimum (maxBound : [T.length (T.takeWhile (== ' ') x) | x <- body, not (T.all isSpace x)])
          lines' = first : [T.stripEnd (T.drop ind x) | x <- body]
       in if T.null first then Nothing else Just (T.intercalate "\n" lines')
  where
    dropApply t = maybe t T.stripStart (T.stripPrefix "[apply]" t)

-- | The first trial that closes its goal: no goals left, no error and no
-- @sorry@ warning. A trial carrying a suggestion gives the suggestion in
-- place of the searching tactic, under the same heartbeat wrapper.
winner :: [Trial] -> Maybe Text
winner = winnerWith False

-- | 'winner', ignoring the declaration's @sorry@ warning when the flag is
-- set: on a goal with several holes the other holes raise it for every
-- trial. A fill whose own text holds @sorry@ or @admit@ always loses.
winnerWith :: Bool -> [Trial] -> Maybe Text
winnerWith siblings trs = snd <$> find closes [(tr, pick tr) | tr <- trs]
  where
    closes (tr, fill) =
      null (tGoals tr)
        && not (any (\g -> dgSeverity g == SevError) (tDiags tr))
        && (siblings || not (any (T.isInfixOf "declaration uses 'sorry'" . dgMessage) (tDiags tr)))
        && not (cheats fill)
        && not (searching fill)
    cheats fill = any (`elem` ["sorry", "admit"]) (T.split (not . identChar) fill)
    -- a search tactic left in the fill (no suggestion read) never wins
    searching fill = any (`T.isInfixOf` fill) ["exact?", "apply?", "aesop?"]
    pick tr = case suggestion tr of
      Nothing -> tSnippet tr
      Just s -> wrapperOf (tSnippet tr) <> s
    wrapperOf s = case T.stripPrefix "set_option maxHeartbeats " s of
      Just r
        | let (num, rest) = T.span (`elem` ['0' .. '9']) r
        , not (T.null num)
        , Just _ <- T.stripPrefix " in " rest ->
            "set_option maxHeartbeats " <> num <> " in "
      _ -> ""

-- | Replace a hole's @sorry@ with a snippet. A term hole gets
-- @(by \<snippet\>)@, or a bare @by \<snippet\>@ right after a @:=@ (or
-- alone on its line, a term proof below its statement). Continuation
-- lines of a multi-line snippet are indented to the column where the
-- snippet starts. A hole that does not hold @sorry@ leaves the source
-- unchanged.
fillHole :: Text -> Hole -> Text -> Text
fillHole src h snip = maybe src fst (fillHoleAt src h snip)

-- | 'fillHole', with where the snippet ends in the result: the 0-based
-- line and the column (in code points) just past its last character.
-- Nothing when the hole does not hold @sorry@.
fillHoleAt :: Text -> Hole -> Text -> Maybe (Text, (Int, Int))
fillHoleAt src h snip =
  let ls = T.splitOn "\n" src
      i = hLine h - 1
   in case splitAt i ls of
        (pre, l : post)
          | i >= 0
          , (a, b) <- T.splitAt (hCol h) l
          , "sorry" `T.isPrefixOf` b ->
              let before = T.strip a
                  bare = T.null before || ":=" `T.isSuffixOf` before
                  (open, close)
                    | not (hTerm h) = ("", "")
                    | bare = ("by ", "")
                    | otherwise = ("(by ", ")")
                  ind = T.replicate (hCol h + T.length open) " "
                  body = case T.splitOn "\n" snip of
                    (x : xs) -> x : [if T.null y then y else ind <> y | y <- xs]
                    [] -> [snip]
                  end = case body of
                    [x] -> (i, hCol h + T.length open + T.length x)
                    _ -> (i + length body - 1, T.length (last body))
               in Just (T.intercalate "\n" (pre <> [a <> open <> T.intercalate "\n" body <> close <> T.drop 5 b] <> post), end)
        _ -> Nothing

-- | The file up to the end of a declaration's proof: all Lean needs to
-- elaborate it, and nothing after it is elaborated again on each trial.
-- The file may end inside an open @namespace@ or @section@; Lean accepts
-- that.
cutAfter :: Text -> Decl -> Text
cutAfter src d = T.intercalate "\n" (take (dEnd d) (T.splitOn "\n" src))

-- | 'cutAfter' the declaration holding the hole; the whole file when no
-- declaration holds it.
cutAtHole :: Text -> Hole -> Text
cutAtHole src h = maybe src (cutAfter src) (holder src h)

holder :: Text -> Hole -> Maybe Decl
holder src h = find (\d -> dStmtEnd d <= hLine h - 1 && hLine h - 1 < dEnd d) (declarations src)

data AutoFail
  = LspDown Text
  -- ^ the language server failed; turn the automation off
  | Missed
  -- ^ no snippet closed the hole within the cap
  deriving (Eq, Show)

-- | One Lsp call, cut off after the given seconds: @Right Nothing@ when
-- it timed out (after calling the server's 'lspTimedOut'), 'LspDown' when
-- it returned @Left@ or threw a synchronous exception (an asynchronous one
-- is rethrown).
guarded :: Lsp -> Double -> IO (Either Text a) -> IO (Either AutoFail (Maybe a))
guarded lsp secs act = do
  r <- try (timeout (max 1 (ceiling (secs * 1000000) :: Int)) act)
  case r of
    Left ex
      | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
      | otherwise -> pure (Left (LspDown (T.pack (displayException (ex :: SomeException)))))
    Right Nothing -> lspTimedOut lsp >> pure (Right Nothing)
    Right (Just (Left e)) -> pure (Left (LspDown e))
    Right (Just (Right a)) -> pure (Right (Just a))

-- | Try the portfolio at one hole, one snippet per call, in order, under
-- the per-hole wall cap (each call is cut off at the time remaining). The
-- winning fill text, or why there is none. When the hole's declaration
-- has other holes, their @sorry@ warning does not count against a trial.
-- A synchronous exception from the Lsp counts as 'LspDown'. The Lsp gets
-- the file only up to the end of the hole's declaration ('cutAtHole').
autoHole :: AutoConfig -> Lsp -> IO Double -> Text -> Hole -> IO (Either AutoFail Text)
autoHole cfg lsp clock src0 h = do
  t0 <- clock
  let enclosing = holder src0 h
      src = cutAtHole src0 h
      siblings = maybe False ((> 1) . length . holesIn src) enclosing
  let loop [] = pure (Left Missed)
      loop (t : ts) = do
        now <- clock
        let left = acSeconds cfg - (now - t0)
        if left <= 0
          then pure (Left Missed)
          else do
            r <- guarded lsp left (lspAttempts lsp src h [wrapSnippet (acHeartbeats cfg) t])
            case r of
              Left e -> pure (Left e)
              Right Nothing -> pure (Left Missed)
              Right (Just trs) -> maybe (loop ts) (pure . Right) (winnerWith siblings trs)
  loop (acTactics cfg)

-- | Run Auto at every hole of a goal (by name) in the base. Only when
-- every hole closes: the base with all of them filled (from the last
-- upward, so earlier positions stay valid) and the fill per hole, in hole
-- order. A goal that is not found or has no holes is 'Missed'.
autoGoal :: AutoConfig -> Lsp -> IO Double -> Text -> Text -> IO (Either AutoFail (Text, [Text]))
autoGoal cfg lsp clock base name =
  case findDecl name (declarations base) of
    Nothing -> pure (Left Missed)
    Just d -> case holesIn base d of
      [] -> pure (Left Missed)
      hs -> do
        r <- each hs
        pure $ case r of
          Left e -> Left e
          Right fills ->
            let pairs = zip hs fills
                byPos = sortBy (comparing (\(h, _) -> Down (hLine h, hCol h))) pairs
             in Right (foldl (\s (h, f) -> fillHole s h f) base byPos, fills)
  where
    each [] = pure (Right [])
    each (h : hs) =
      autoHole cfg lsp clock base h >>= \case
        Left e -> pure (Left e)
        Right f -> fmap (f :) <$> each hs
