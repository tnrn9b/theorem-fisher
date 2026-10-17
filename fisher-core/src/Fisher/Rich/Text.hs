-- | Rich's @Text@: a string with style spans, console markup, and the
-- word wrapping, justification and overflow Rich applies when it prints
-- (@rich/text.py@, @rich/markup.py@, @rich/_wrap.py@,
-- @rich/containers.py@, Rich 15.0.0).
module Fisher.Rich.Text
  ( RText (..)
  , Span (..)
  , Justify (..)
  , Overflow (..)
  , plainText
  , fromMarkup
  , escapeMarkup
  , setPlain
  , cellLength
  , appendText
  , joinTexts
  , splitText
  , divide
  , wrap
  , truncateText
  , padLeft
  , padRight
  , padBoth
  , rstripText
  , expandTabs
  , measureText
  , textSegments
  , stylizeBefore
  , blankCopy
  , parseJustify
  , parseOverflow
  ) where

import Data.List (intersperse, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Rich.Cells
import Fisher.Rich.Style

data Justify = JDefault | JLeft | JCenter | JRight | JFull
  deriving (Eq, Show)

data Overflow = OFold | OCrop | OEllipsis | OIgnore
  deriving (Eq, Show)

parseJustify :: Text -> Maybe Justify
parseJustify = \case
  "default" -> Just JDefault
  "left" -> Just JLeft
  "center" -> Just JCenter
  "right" -> Just JRight
  "full" -> Just JFull
  _ -> Nothing

parseOverflow :: Text -> Maybe Overflow
parseOverflow = \case
  "fold" -> Just OFold
  "crop" -> Just OCrop
  "ellipsis" -> Just OEllipsis
  "ignore" -> Just OIgnore
  _ -> Nothing

data Span = Span {spStart :: !Int, spEnd :: !Int, spStyle :: !Style}
  deriving (Eq, Show)

data RText = RText
  { tPlain :: !Text
  , tSpans :: [Span]
  , tStyle :: Style
  , tJustify :: Maybe Justify
  , tOverflow :: Maybe Overflow
  , tNoWrap :: Maybe Bool
  , tEnd :: Text
  , tTabSize :: Maybe Int
  }
  deriving (Eq, Show)

-- | @strip_control_codes@: BEL, BS, VT, FF and CR are dropped.
stripControl :: Text -> Text
stripControl = T.filter (`notElem` ['\a', '\b', '\v', '\f', '\r'])

plainText :: Text -> RText
plainText t = RText (stripControl t) [] nullStyle Nothing Nothing Nothing "\n" Nothing

blank :: RText -> Text -> RText
blank t p = (plainText p) {tStyle = tStyle t, tJustify = tJustify t, tOverflow = tOverflow t}

-- | The @plain@ setter: sanitise, and trim spans past a shorter end.
setPlain :: Text -> RText -> RText
setPlain new t
  | new == tPlain t = t
  | otherwise =
      let s = stripControl new
          t' = t {tPlain = s}
       in if T.length s < T.length (tPlain t) then trimSpans t' else t'

trimSpans :: RText -> RText
trimSpans t =
  let maxOff = T.length (tPlain t)
   in t {tSpans = [sp {spEnd = min maxOff (spEnd sp)} | sp <- tSpans t, spStart sp < maxOff]}

cellLength :: RText -> Int
cellLength = cellLen . tPlain

-- ------------------------------------------------------------------ markup

-- | @rich.markup.render@ (without emoji codes): @[style]...[/]@ tags,
-- backslash escapes. A closing tag that matches nothing is dropped, where
-- Rich would raise.
fromMarkup :: Text -> RText
fromMarkup markup
  | not (T.any (== '[') markup) = plainText markup
  | otherwise =
      let (pieces, spans0, stack) = foldl step ([], [], []) (parseMarkup markup)
          plain = T.concat (reverse pieces)
          len = T.length plain
          closing = [Span s len (parseStyle n) | (s, n) <- stack, not (T.null n)]
          allSpans = reverse spans0 <> closing
       in (plainText plain) {tSpans = sortOn spStart (reverse allSpans)}
  where
    step (pieces, spans, stack) item =
      let pos = sum (map T.length pieces)
       in case item of
            Left txt -> (stripControl (T.replace "\\[" "[" txt) : pieces, spans, stack)
            Right tag
              | Just name <- T.stripPrefix "/" tag ->
                  let n = T.toLower (T.strip name)
                   in if T.null n
                        then case stack of
                          ((s, open) : rest) -> (pieces, close s open pos : spans, rest)
                          [] -> (pieces, spans, stack)
                        else case break ((== n) . snd) stack of
                          (before, (s, open) : after) -> (pieces, close s open pos : spans, before <> after)
                          _ -> (pieces, spans, stack)
              | otherwise -> (pieces, spans, (pos, normalizeTag tag) : stack)
    close s open pos = Span s pos (if "@" `T.isPrefixOf` open then nullStyle else parseStyle open)
    normalizeTag tag =
      let (name, rest) = T.breakOn "=" tag
       in T.toLower (T.strip name) <> (if T.null rest then "" else " " <> T.drop 1 rest)

-- | @rich.markup._parse@: text runs and tags, honouring backslash escapes.
parseMarkup :: Text -> [Either Text Text]
parseMarkup s = go 0 0
  where
    n = T.length s
    at i = T.index s i
    go pos i
      | i >= n = [Left (T.drop pos s) | pos < n]
      | otherwise = case matchAt i of
          Nothing -> go pos (i + 1)
          Just (bs, tagStart, end) ->
            let before = [Left (slice pos i) | i > pos]
                (half, odd') = bs `divMod` 2
                tagText = slice (tagStart + 1) (end - 1)
                lit = [Left (T.replicate half "\\") | half > 0]
             in if odd' == 1
                  then before <> lit <> [Left (slice tagStart end)] <> go end end
                  else before <> lit <> [Right tagText] <> go end end
    slice a b = T.take (b - a) (T.drop a s)
    -- backslashes, then [x...] with x in [a-z#/@] and no '[' before ']'
    matchAt i =
      let bs = T.length (T.takeWhile (== '\\') (T.drop i s))
          j = i + bs
       in if j + 1 < n && at j == '[' && validFirst (at (j + 1))
            then case T.findIndex (\c -> c == ']' || c == '[') (T.drop (j + 1) s) of
              Just k | at (j + 1 + k) == ']' -> Just (bs, j, j + 1 + k + 1)
              _ -> Nothing
            else Nothing
    validFirst c = (c >= 'a' && c <= 'z') || c `elem` ['#', '/', '@']

-- | @rich.markup.escape@.
escapeMarkup :: Text -> Text
escapeMarkup = T.pack . go . T.unpack
  where
    go [] = []
    go s@('[' : c : _)
      | validFirst c, Just _ <- closeOf (drop 1 s) = '\\' : '[' : go (drop 1 s)
    go (x : xs) = x : go xs
    validFirst c = (c >= 'a' && c <= 'z') || c `elem` ['#', '/', '@']
    closeOf = lookup True . map (\c -> (c == ']', ())) . takeWhileInclusive (\c -> c /= ']' && c /= '[')
    takeWhileInclusive p xs = let (a, b) = span p xs in a <> take 1 b

-- ------------------------------------------------------------ composition

appendText :: RText -> RText -> RText
appendText a b =
  let off = T.length (tPlain a)
      own = [Span off (off + T.length (tPlain b)) (tStyle b) | tStyle b /= nullStyle, not (T.null (tPlain b))]
      moved = [Span (s + off) (e + off) st | Span s e st <- tSpans b]
   in a {tPlain = tPlain a <> tPlain b, tSpans = tSpans a <> own <> moved}

-- | @Text.join@: the separator between each line, in a copy of the
-- separator's settings.
joinTexts :: RText -> [RText] -> RText
joinTexts sep ts =
  foldl appendText (blankCopy sep) (if T.null (tPlain sep) then ts else intersperse sep ts)

-- | @blank_copy@.
blankCopy :: RText -> RText
blankCopy t = t {tPlain = "", tSpans = []}

-- | @Text.divide@.
divide :: [Int] -> RText -> [RText]
divide [] t = [t]
divide offs t =
  let txt = tPlain t
      bounds = 0 : offs <> [T.length txt]
      ranges = zip bounds (drop 1 bounds)
   in [ (blank t (T.take (e - s) (T.drop s txt)))
          { tSpans =
              [ Span ns ne st
              | Span ss se st <- tSpans t
              , let ns = max 0 (ss - s)
              , let ne = min (se - s) (e - s)
              , ne > ns ] }
      | (s, e) <- ranges ]

-- | @Text.split@ on a separator.
splitText :: Text -> Bool -> Bool -> RText -> [RText]
splitText sep includeSep allowBlank t
  | not (sep `T.isInfixOf` txt) = [t]
  | otherwise =
      let matches = findAll 0
          lines'
            | includeSep = divide [e | (_, e) <- matches] t
            | otherwise = filter ((/= sep) . tPlain) (divide (concat [[s, e] | (s, e) <- matches]) t)
       in if not allowBlank && sep `T.isSuffixOf` txt then init lines' else lines'
  where
    txt = tPlain t
    sl = T.length sep
    findAll from =
      let (pre, post) = T.breakOn sep (T.drop from txt)
       in if T.null post then [] else let s = from + T.length pre in (s, s + sl) : findAll (s + sl)

-- ------------------------------------------------------------- adjusting

padLeft, padRight :: Int -> RText -> RText
padLeft k t
  | k <= 0 = t
  | otherwise =
      (setPlain (T.replicate k " " <> tPlain t) t) {tSpans = [Span (s + k) (e + k) st | Span s e st <- tSpans t]}
padRight k t
  | k <= 0 = t
  | otherwise = setPlain (tPlain t <> T.replicate k " ") t

padBoth :: Int -> RText -> RText
padBoth k t
  | k <= 0 = t
  | otherwise =
      (setPlain (T.replicate k " " <> tPlain t <> T.replicate k " ") t)
        {tSpans = [Span (s + k) (e + k) st | Span s e st <- tSpans t]}

rightCrop :: Int -> RText -> RText
rightCrop amount t =
  let maxOff = T.length (tPlain t) - amount
   in t { tSpans = [sp {spEnd = min maxOff (spEnd sp)} | sp <- tSpans t, spStart sp < maxOff]
        , tPlain = T.take maxOff (tPlain t) }

rstripText :: RText -> RText
rstripText t = setPlain (T.dropWhileEnd isPySpace (tPlain t)) t

-- | @rstrip_end@: drop trailing whitespace beyond @size@ characters.
rstripEnd :: Int -> RText -> RText
rstripEnd size t
  | len > size =
      let ws = T.length (T.takeWhileEnd isPySpace (tPlain t))
       in if ws > 0 then rightCrop (min ws (len - size)) t else t
  | otherwise = t
  where
    len = T.length (tPlain t)

-- | @Text.truncate@.
truncateText :: Int -> Maybe Overflow -> Bool -> RText -> RText
truncateText maxWidth overflow pad t
  | ov == OIgnore = t
  | otherwise =
      let len = cellLen (tPlain t)
          t'
            | len > maxWidth = case ov of
                OEllipsis -> setPlain (setCellSize (tPlain t) (maxWidth - 1) <> "…") t
                _ -> setPlain (setCellSize (tPlain t) maxWidth) t
            | otherwise = t
       in if pad && len < maxWidth then t' {tPlain = tPlain t' <> T.replicate (maxWidth - len) " "} else t'
  where
    ov = fromMaybe OFold (maybe (tOverflow t) Just overflow)

-- | @extend_style@: spaces at the end, carrying spans that reach it.
extendStyle :: Int -> RText -> RText
extendStyle k t
  | k <= 0 = t
  | null (tSpans t) = setPlain (tPlain t <> T.replicate k " ") t
  | otherwise =
      let endOff = T.length (tPlain t)
       in t { tSpans = [if e >= endOff then Span s (e + k) st else sp | sp@(Span s e st) <- tSpans t]
            , tPlain = tPlain t <> T.replicate k " " }

-- | @Text.expand_tabs@.
expandTabs :: Int -> RText -> RText
expandTabs tab t
  | not (T.any (== '\t') (tPlain t)) = t
  | otherwise =
      let pieces = concatMap line (splitText "\n" True False t)
          joined = joinTexts (blankCopy t) pieces
       in t {tPlain = tPlain joined, tSpans = tSpans joined}
  where
    line l
      | not (T.any (== '\t') (tPlain l)) = [l]
      | otherwise = go 0 (splitText "\t" True False l)
    go _ [] = []
    go pos (p : ps)
      | "\t" `T.isSuffixOf` tPlain p =
          let p1 = p {tPlain = T.init (tPlain p) <> " "}
              pos1 = pos + cellLength p1
              r = pos1 `mod` tab
              (p2, pos2) = if r /= 0 then (extendStyle (tab - r) p1, pos1 + tab - r) else (p1, pos1)
           in p2 : go pos2 ps
      | otherwise = p : go (pos + cellLength p) ps

-- -------------------------------------------------------------- wrapping

-- | @divide_line@: offsets to break a line at so it fits @width@ cells.
divideLine :: Text -> Int -> Bool -> [Int]
divideLine txt width fold = go (wordsOf txt) 0 []
  where
    go [] _ acc = reverse acc
    go ((start, word) : rest) offset acc =
      let wlen = cellLen (T.dropWhileEnd isPySpace word)
          remaining = width - offset
       in if remaining >= wlen
            then go rest (offset + cellLen word) acc
            else
              if wlen > width
                then
                  if fold
                    then
                      let folded = chopCells word width
                          walk _ [] off acc' = (off, acc')
                          walk st [l] _ acc' = (cellLen l, if st /= 0 then st : acc' else acc')
                          walk st (l : ls) off acc' = walk (st + T.length l) ls off (if st /= 0 then st : acc' else acc')
                          (off', acc'') = walk start folded offset acc
                       in go rest off' acc''
                    else go rest (cellLen word) (if start /= 0 then start : acc else acc)
                else
                  if offset /= 0 && start /= 0
                    then go rest (cellLen word) (start : acc)
                    else go rest offset acc

-- | @re_word@ matches: @\\s*\\S+\\s*@, with their start offsets.
wordsOf :: Text -> [(Int, Text)]
wordsOf = go 0
  where
    go pos s
      | T.null s = []
      | otherwise =
          let (lead, r1) = T.span isPySpace s
              (body, r2) = T.break isPySpace r1
              (trail, r3) = T.span isPySpace r2
           in if T.null body then [] else
                let w = lead <> body <> trail in (pos, w) : go (pos + T.length w) r3

-- | @Text.wrap@.
wrap :: Int -> Maybe Justify -> Maybe Overflow -> Int -> Maybe Bool -> RText -> [RText]
wrap width justify overflow tabSize noWrapOpt t =
  concatMap one (splitText "\n" False True t)
  where
    wJustify = fromMaybe JDefault (maybe (tJustify t) Just justify)
    wOverflow = fromMaybe OFold (maybe (tOverflow t) Just overflow)
    noWrap = fromMaybe False (maybe (tNoWrap t) Just noWrapOpt) || overflow == Just OIgnore
    one line0 =
      let line = expandTabs tabSize line0
       in if noWrap && overflow == Just OIgnore
            then [line]
            else
              let newLines
                    | noWrap = [line]
                    | otherwise = map (rstripEnd width) (divide (divideLine (tPlain line) width (wOverflow == OFold)) line)
               in map (truncateText width (Just wOverflow) False) (justifyLines width wJustify wOverflow newLines)

-- | @Lines.justify@ (the full method spreads spaces as Rich does).
justifyLines :: Int -> Justify -> Overflow -> [RText] -> [RText]
justifyLines width j ov ls = case j of
  JLeft -> map (truncateText width (Just ov) True) ls
  JCenter ->
    [ let l1 = truncateText width (Just ov) False (rstripText l)
          l2 = padLeft ((width - cellLength l1) `div` 2) l1
       in padRight (width - cellLength l2) l2
    | l <- ls ]
  JRight ->
    [ let l1 = truncateText width (Just ov) False (rstripText l) in padLeft (width - cellLength l1) l1 | l <- ls ]
  JFull -> zipWith full [0 :: Int ..] ls
  JDefault -> ls
  where
    n = length ls
    full i l
      | i == n - 1 = l
      | otherwise =
          let ws = splitText " " False False l
              wsize = sum (map cellLength ws)
              k = length ws - 1
              spread sp cnt idx
                | null sp = sp
                | wsize + cnt < width =
                    let p = length sp - idx - 1
                        sp' = take p sp <> [sp !! p + 1] <> drop (p + 1) sp
                     in spread sp' (cnt + 1) ((idx + 1) `mod` length sp)
                | otherwise = sp
              spaces = spread (replicate k 1) k 0
              tokens = concat [w : [plainText (T.replicate s " ") | Just s <- [lookupAt idx spaces]] | (idx, w) <- zip [0 ..] ws]
           in joinTexts (blankCopy l) tokens
    lookupAt idx xs = if idx < length xs then Just (xs !! idx) else Nothing

-- | @Text.__rich_measure__@: (longest word, longest line), in cells.
measureText :: RText -> (Int, Int)
measureText t =
  let txt = tPlain t
      ls = pySplitLines txt
      maxW = if null ls then 0 else maximum (map cellLen ls)
      ws = filter (not . T.null) (T.split isPySpace txt)
      minW = if null ws then maxW else maximum (map cellLen ws)
   in (minW, maxW)

-- | Python's @str.splitlines()@.
pySplitLines :: Text -> [Text]
pySplitLines = go
  where
    isBreak c = c `elem` ['\n', '\r', '\v', '\f', '\x1c', '\x1d', '\x1e', '\x85', '\x2028', '\x2029']
    go s
      | T.null s = []
      | otherwise =
          let (l, rest) = T.break isBreak s
           in case T.uncons rest of
                Nothing -> [l]
                Just ('\r', r) | Just ('\n', r') <- T.uncons r -> l : go r'
                Just (_, r) -> l : go r

-- | @stylize_before@: a style under every other span.
stylizeBefore :: Style -> RText -> RText
stylizeBefore st t
  | st == nullStyle || T.null (tPlain t) = t
  | otherwise = t {tSpans = Span 0 (T.length (tPlain t)) st : tSpans t}

-- -------------------------------------------------------------- segments

-- | @Text.render@: runs of text in one style, then the end string.
textSegments :: Style -> RText -> [(Text, Style)]
textSegments base t =
  let txt = tPlain t
      n = T.length txt
      styleAt i = foldl (<>) (base <> tStyle t) [spStyle sp | sp <- tSpans t, spStart sp <= i, i < spEnd sp]
      bounds = uniq (sortOn id (0 : n : concat [[spStart sp, spEnd sp] | sp <- tSpans t, spStart sp < n]))
      runs = [ (T.take (b - a) (T.drop a txt), styleAt a) | (a, b) <- zip bounds (drop 1 bounds), b > a, a < n ]
   in merge runs <> [(tEnd t, base) | not (T.null (tEnd t))]
  where
    uniq (a : b : r) | a == b = uniq (b : r) | otherwise = a : uniq (b : r)
    uniq r = r
    merge ((a, s) : (b, s') : r) | s == s' = merge ((a <> b, s) : r)
    merge (x : r) = x : merge r
    merge [] = []
