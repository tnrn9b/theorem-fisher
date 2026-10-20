-- | The prompt primitives of the setup wizard (upstream @cli/setup.py@:
-- @print_header@ and friends, @prompt@, @prompt_choice@,
-- @prompt_yes_no@, @prompt_checklist@), the checklist of
-- upstream @cli/curses_ui.py@ and the gentler copies @tools_config.py@ keeps
-- for itself, all over 'Sys'.
--
-- Upstream tries a curses menu first and falls back to a numbered list
-- when curses cannot start (no terminal, or a @TERM@ it does not know).
-- Fisher always uses the numbered list, which is what upstream shows in
-- every recorded run.
module Fisher.Cli.Prompt
  ( UI (..)
  , newUI
  , say
  , sayLn
  , paint
  , magenta
  , In (..)
  , readLine
  , readSecret
  , exitWith
    -- * setup.py
  , printHeader
  , printInfo
  , printSuccess
  , printWarning
  , printError
  , prompt
  , promptChoice
  , promptYesNo
  , promptChecklist
  , cursesChecklist
    -- * tools_config.py
  , toolsPrompt
  , toolsPromptYesNo
  , toolsPromptChoice
    -- * Python coercions
  , showT
  , pyIntOf
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Cli.Status (sysStyle)
import Fisher.LLM.Auth.Error (SystemExit (..))
import Fisher.LLM.Auth.Util (pyIntText, tryInterrupt)
import Fisher.LLM.Sys
import Fisher.Term

-- | The effects and the colour decision (@colors.color@) for one run.
data UI = UI
  { uiSys :: Sys
  , uiStyle :: Style
  }

newUI :: Sys -> IO UI
newUI sys = UI sys <$> sysStyle sys

say :: UI -> Text -> IO ()
say ui = sysOut (uiSys ui)

-- | @print(t)@.
sayLn :: UI -> Text -> IO ()
sayLn ui t = say ui (t <> "\n")

-- | @color(t, *codes)@.
paint :: UI -> [Colour] -> Text -> Text
paint ui = color (uiStyle ui)

magenta :: Colour
magenta = "\ESC[35m"

showT :: Show a => a -> Text
showT = T.pack . show

-- | @int(s)@ (surrounding whitespace allowed), Nothing where Python raises.
pyIntOf :: Text -> Maybe Integer
pyIntOf = pyIntText

-- | What one @input()@ gave.
data In = Typed Text | Eof | Interrupted
  deriving (Eq, Show)

readWith :: (Text -> IO (Maybe Text)) -> Text -> IO In
readWith f p =
  tryInterrupt (f p) >>= \case
    Left () -> pure Interrupted
    Right Nothing -> pure Eof
    Right (Just t) -> pure (Typed t)

-- | @input(prompt)@.
readLine :: UI -> Text -> IO In
readLine ui = readWith (sysInput (uiSys ui))

-- | @getpass.getpass(prompt)@.
readSecret :: UI -> Text -> IO In
readSecret ui = readWith (sysGetPass (uiSys ui))

-- | @sys.exit(n)@.
exitWith :: Int -> IO a
exitWith = throwIO . SystemExit

-- ------------------------------------------------------------------ setup.py

printHeader :: UI -> Text -> IO ()
printHeader ui t = sayLn ui "" >> sayLn ui (paint ui [cyan, bold] ("◆ " <> t))

printInfo :: UI -> Text -> IO ()
printInfo ui t = sayLn ui (paint ui [dim] ("  " <> t))

printSuccess :: UI -> Text -> IO ()
printSuccess ui t = sayLn ui (paint ui [green] ("✓ " <> t))

printWarning :: UI -> Text -> IO ()
printWarning ui t = sayLn ui (paint ui [yellow] ("⚠ " <> t))

printError :: UI -> Text -> IO ()
printError ui t = sayLn ui (paint ui [red] ("✗ " <> t))

-- | @prompt(question, default, password)@: the stripped answer, else the
-- default, else empty. Ctrl-C or EOF prints a newline and exits 1.
prompt :: UI -> Text -> Maybe Text -> Bool -> IO Text
prompt ui q mdflt password = do
  let dflt = case mdflt of
        Just d | not (T.null d) -> Just d
        _ -> Nothing
      display = maybe (q <> ": ") (\d -> q <> " [" <> d <> "]: ") dflt
  r <- (if password then readSecret else readLine) ui (paint ui [yellow] display)
  case r of
    Typed v -> pure (let s = T.strip v in if T.null s then maybe "" id dflt else s)
    _ -> sayLn ui "" >> exitWith 1

-- | @prompt_choice(question, choices, default)@, the numbered fallback.
promptChoice :: UI -> Text -> [Text] -> Int -> IO Int
promptChoice ui q choices dflt = do
  sayLn ui (paint ui [yellow] q)
  forM_ (zip [0 :: Int ..] choices) $ \(i, c) ->
    if i == dflt then sayLn ui (paint ui [green] ("  ● " <> c)) else sayLn ui ("  ○ " <> c)
  printInfo ui ("  Enter for default (" <> showT (dflt + 1) <> ")  Ctrl+C to exit")
  let n = length choices
      loop =
        readLine ui (paint ui [dim] ("  Select [1-" <> showT n <> "] (" <> showT (dflt + 1) <> "): ")) >>= \case
          Typed v
            | T.null v -> pure dflt
            | otherwise -> case pyIntOf v of
                Nothing -> printError ui "Please enter a number" >> loop
                Just k
                  | k - 1 >= 0 && k - 1 < toInteger n -> pure (fromInteger k - 1)
                  | otherwise -> printError ui ("Please enter a number between 1 and " <> showT n) >> loop
          _ -> sayLn ui "" >> exitWith 1
  loop

-- | @prompt_yes_no(question, default)@.
promptYesNo :: UI -> Text -> Bool -> IO Bool
promptYesNo ui q dflt = loop
  where
    loop =
      readLine ui (paint ui [yellow] (q <> " [" <> (if dflt then "Y/n" else "y/N") <> "]: ")) >>= \case
        Typed v -> case T.toLower (T.strip v) of
          "" -> pure dflt
          a | a `elem` ["y", "yes"] -> pure True
            | a `elem` ["n", "no"] -> pure False
          _ -> printError ui "Please enter 'y' or 'n'" >> loop
        _ -> sayLn ui "" >> exitWith 1

-- | @prompt_checklist(title, items, pre_selected)@: the sorted indices.
promptChecklist :: UI -> Text -> [Text] -> [Int] -> IO [Int]
promptChecklist ui title items pre =
  Set.toAscList <$> cursesChecklist ui title items (Set.fromList pre) (Set.fromList pre)

-- | @curses_checklist(title, items, selected, cancel_returns=...)@, its
-- numbered fallback: toggle by number, Enter to confirm; anything that is
-- not a number (or EOF, Ctrl-C) cancels.
cursesChecklist :: UI -> Text -> [Text] -> Set.Set Int -> Set.Set Int -> IO (Set.Set Int)
cursesChecklist ui title items selected cancel = do
  sayLn ui (paint ui [yellow] ("\n  " <> title))
  sayLn ui (paint ui [dim] "  Toggle by number, Enter to confirm.\n")
  let n = length items
      loop chosen = do
        forM_ (zip [0 :: Int ..] items) $ \(i, label) -> do
          let marker = if Set.member i chosen then paint ui [green] "[✓]" else "[ ]"
          sayLn ui ("  " <> marker <> " " <> T.justifyRight 2 ' ' (showT (i + 1)) <> ". " <> label)
        sayLn ui ""
        readLine ui (paint ui [dim] "  Toggle # (or Enter to confirm): ") >>= \case
          Typed v
            | T.null (T.strip v) -> pure chosen
            | otherwise -> case pyIntOf (T.strip v) of
                Nothing -> pure cancel
                Just k -> do
                  let idx = fromInteger k - 1
                      chosen' =
                        if idx >= 0 && idx < n
                          then (if Set.member idx chosen then Set.delete idx chosen else Set.insert idx chosen)
                          else chosen
                  sayLn ui ""
                  loop chosen'
          _ -> pure cancel
  loop selected

-- ---------------------------------------------------------- tools_config.py

-- | @tools_config._prompt@: as 'prompt', but Ctrl-C or EOF gives the
-- default.
toolsPrompt :: UI -> Text -> Maybe Text -> Bool -> IO Text
toolsPrompt ui q mdflt password = do
  let dflt = case mdflt of
        Just d | not (T.null d) -> Just d
        _ -> Nothing
      display = maybe (q <> ": ") (\d -> q <> " [" <> d <> "]: ") dflt
  r <- (if password then readSecret else readLine) ui (paint ui [yellow] display)
  case r of
    Typed v -> pure (let s = T.strip v in if T.null s then maybe "" id dflt else s)
    _ -> sayLn ui "" >> pure (maybe "" id dflt)

-- | @tools_config._prompt_yes_no@: anything else asks again silently.
toolsPromptYesNo :: UI -> Text -> Bool -> IO Bool
toolsPromptYesNo ui q dflt = loop
  where
    loop =
      readLine ui (paint ui [yellow] (q <> " [" <> (if dflt then "Y/n" else "y/N") <> "]: ")) >>= \case
        Typed v -> case T.toLower (T.strip v) of
          "" -> pure dflt
          a | a `elem` ["y", "yes"] -> pure True
            | a `elem` ["n", "no"] -> pure False
          _ -> loop
        _ -> sayLn ui "" >> pure dflt

-- | @tools_config._prompt_choice@, its numbered fallback: numbered items,
-- no hint line; an out-of-range number asks again, anything else (or EOF,
-- Ctrl-C) gives the default.
toolsPromptChoice :: UI -> Text -> [Text] -> Int -> IO Int
toolsPromptChoice ui q choices dflt = do
  sayLn ui (paint ui [yellow] q)
  forM_ (zip [0 :: Int ..] choices) $ \(i, c) -> do
    let line = (if i == dflt then "  ● " else "  ○ ") <> showT (i + 1) <> ". " <> c
    sayLn ui (if i == dflt then paint ui [green] line else line)
  let n = length choices
      loop =
        readLine ui (paint ui [dim] ("  Select [1-" <> showT n <> "] (" <> showT (dflt + 1) <> "): ")) >>= \case
          Typed v
            | T.null v -> pure dflt
            | otherwise -> case pyIntOf v of
                Nothing -> sayLn ui "" >> pure dflt
                Just k
                  | k - 1 >= 0 && k - 1 < toInteger n -> pure (fromInteger k - 1)
                  | otherwise -> loop
          _ -> sayLn ui "" >> pure dflt
  loop
