-- | Rich's layout: rendering to lines of styled segments, padding,
-- tables and panels, and @Console.print@ (@rich/console.py@,
-- @rich/segment.py@, @rich/padding.py@, @rich/table.py@, @rich/panel.py@,
-- @rich/box.py@, @rich/_ratio.py@, @rich/measure.py@, Rich 15.0.0).
-- Only what Fisher's output uses is here; the algorithms are Rich's, so
-- widths, wrapping and truncation come out the same.
module Fisher.Rich.Render
  ( Renderable (..)
  , Options (..)
  , options
  , Seg
  , Line
  , Pad
  , Box
  , boxAscii, boxSquare, boxRounded, boxHeavyHead, boxHorizontals
  , boxByName
  , Column (..)
  , column
  , Table (..)
  , table
  , grid
  , Panel (..)
  , panel
  , VAlign (..)
  , markup
  , render
  , renderLines
  , measure
  , printLines
  , linePlain
  , lineAnsi
  ) where

import Control.Applicative ((<|>))
import Data.List (transpose)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Rich.Cells
import Fisher.Rich.Style
import Fisher.Rich.Text

type Seg = (Text, Style)

type Line = [Seg]

-- | (top, right, bottom, left).
type Pad = (Int, Int, Int, Int)

data VAlign = VTop | VMiddle | VBottom
  deriving (Eq, Show)

data Renderable
  = RTxt RText
  | RPadding Pad Style Bool Renderable
  | RTable Table
  | RPanel Panel
  | RAlign Justify Renderable
  deriving (Show)

-- | Console markup as a renderable (@console.render_str@).
markup :: Text -> Renderable
markup = RTxt . fromMarkup

data Options = Options
  { oWidth :: !Int
  , oJustify :: Maybe Justify
  , oOverflow :: Maybe Overflow
  , oNoWrap :: Maybe Bool
  , oHeight :: Maybe Int
  }
  deriving (Show)

options :: Int -> Options
options w = Options w Nothing Nothing Nothing Nothing

-- ------------------------------------------------------------------ boxes

data Box = Box
  { bName :: Text
  , bTop :: (Char, Char, Char, Char)
  , bHead :: (Char, Char, Char, Char)
  , bHeadRow :: (Char, Char, Char, Char)
  , bMid :: (Char, Char, Char, Char)
  , bRow :: (Char, Char, Char, Char)
  , bFootRow :: (Char, Char, Char, Char)
  , bFoot :: (Char, Char, Char, Char)
  , bBottom :: (Char, Char, Char, Char)
  }
  deriving (Show)

mkBox :: Text -> [String] -> Box
mkBox name ls = case map four ls of
  [a, b, c, d, e, f, g, h] -> Box name a b c d e f g h
  _ -> error "box needs eight lines"
  where
    four [w, x, y, z] = (w, x, y, z)
    four _ = error "box line needs four characters"

boxAscii, boxSquare, boxRounded, boxHeavyHead, boxHorizontals :: Box
boxHorizontals = mkBox "HORIZONTALS" [" ── ", "    ", " ── ", "    ", " ── ", " ── ", "    ", " ── "]
boxAscii = mkBox "ASCII" ["+--+", "| ||", "|-+|", "| ||", "|-+|", "|-+|", "| ||", "+--+"]
boxSquare = mkBox "SQUARE" ["┌─┬┐", "│ ││", "├─┼┤", "│ ││", "├─┼┤", "├─┼┤", "│ ││", "└─┴┘"]
boxRounded = mkBox "ROUNDED" ["╭─┬╮", "│ ││", "├─┼┤", "│ ││", "├─┼┤", "├─┼┤", "│ ││", "╰─┴╯"]
boxHeavyHead = mkBox "HEAVY_HEAD" ["┏━┳┓", "┃ ┃┃", "┡━╇┩", "│ ││", "├─┼┤", "├─┼┤", "│ ││", "└─┴┘"]

boxByName :: Text -> Maybe Box
boxByName = \case
  "ASCII" -> Just boxAscii
  "SQUARE" -> Just boxSquare
  "ROUNDED" -> Just boxRounded
  "HEAVY_HEAD" -> Just boxHeavyHead
  "HORIZONTALS" -> Just boxHorizontals
  _ -> Nothing

-- | @get_plain_headed_box@.
plainHeaded :: Box -> Box
plainHeaded b = if bName b == "HEAVY_HEAD" then boxSquare else b

edgeRow :: (Char, Char, Char, Char) -> [Int] -> Text
edgeRow (l, h, d, r) ws = T.singleton l <> T.intercalate (T.singleton d) [T.replicate w (T.singleton h) | w <- ws] <> T.singleton r

-- | @Box.get_row@.
boxRow :: Box -> Text -> Bool -> [Int] -> Text
boxRow b level edge ws =
  let (l, h, x, r) = case level of
        "head" -> bHeadRow b
        "row" -> bRow b
        "foot" -> bFootRow b
        _ -> let (ml, _, mv, mr) = bMid b in (ml, ' ', mv, mr)
      inner = T.intercalate (T.singleton x) [T.replicate w (T.singleton h) | w <- ws]
   in (if edge then T.singleton l else "") <> inner <> (if edge then T.singleton r else "")

-- ------------------------------------------------------------ table types

data Column = Column
  { cHeader :: Renderable
  , cStyle :: Style
  , cHeaderStyle :: Style
  , cJustify :: Justify
  , cVertical :: VAlign
  , cOverflow :: Maybe Overflow
  , cWidth :: Maybe Int
  , cMinWidth :: Maybe Int
  , cMaxWidth :: Maybe Int
  , cRatio :: Maybe Int
  , cNoWrap :: Bool
  }
  deriving (Show)

-- | @Table.add_column@'s defaults.
column :: Text -> Column
column h = Column (markup h) nullStyle nullStyle JLeft VTop (Just OEllipsis) Nothing Nothing Nothing Nothing False

data Table = Table
  { tbColumns :: [Column]
  , tbRows :: [[Renderable]]
  , tbTitle :: Maybe Text
  , tbTitleStyle :: Style
  , tbBox :: Maybe Box
  , tbShowHeader :: Bool
  , tbShowEdge :: Bool
  , tbShowLines :: Bool
  , tbPadEdge :: Bool
  , tbCollapsePadding :: Bool
  , tbExpand :: Bool
  , tbPadding :: Pad
  , tbBorderStyle :: Style
  , tbHeaderStyle :: Style
  , tbWidth :: Maybe Int
  , tbMinWidth :: Maybe Int
  }
  deriving (Show)

-- | @Table()@'s defaults.
table :: [Column] -> [[Renderable]] -> Table
table cols rows =
  Table cols rows Nothing (parseStyle "table.title") (Just boxHeavyHead) True True False True False False (0, 1, 0, 1)
    nullStyle (parseStyle "table.header") Nothing Nothing

-- | @Table.grid@: no box, no header, collapsed padding, no edge padding.
grid :: Pad -> [Column] -> [[Renderable]] -> Table
grid pad cols rows =
  (table cols rows) {tbBox = Nothing, tbShowHeader = False, tbPadEdge = False, tbCollapsePadding = True, tbPadding = pad}

data Panel = Panel
  { pBody :: Renderable
  , pBox :: Box
  , pTitle :: Maybe Text
  , pTitleAlign :: Justify
  , pPadding :: Pad
  , pExpand :: Bool
  , pBorderStyle :: Style
  , pWidth :: Maybe Int
  , pStyle :: Style
  -- ^ the panel's own style: under the body and the border
  }
  deriving (Show)

panel :: Renderable -> Panel
panel body = Panel body boxRounded Nothing JCenter (0, 1, 0, 1) True nullStyle Nothing nullStyle

-- --------------------------------------------------------------- segments

newline :: Seg
newline = ("\n", nullStyle)

segLen :: Seg -> Int
segLen = cellLen . fst

-- | @Segment.adjust_line_length@.
adjustLine :: Int -> Bool -> Style -> Line -> Line
adjustLine len pad st line
  | total < len = if pad then line <> [(T.replicate (len - total) " ", st)] else line
  | total > len = crop 0 line
  | otherwise = line
  where
    total = sum (map segLen line)
    crop _ [] = []
    crop acc ((t, s) : rest)
      | acc + cellLen t < len = (t, s) : crop (acc + cellLen t) rest
      | otherwise = [(setCellSize t (len - acc), s)]

-- | @Segment.split_and_crop_lines@ (without the newlines).
splitAndCrop :: Int -> Bool -> Style -> [Seg] -> [Line]
splitAndCrop len pad st = go []
  where
    go cur [] = [adjustLine len pad st (reverse cur) | not (null cur)]
    go cur ((t, s) : rest)
      | T.any (== '\n') t =
          let (before, after) = T.breakOn "\n" t
              cur' = if T.null before then cur else (before, s) : cur
           in adjustLine len pad st (reverse cur') : go [] (if T.length after > 1 then (T.drop 1 after, s) : rest else rest)
      | otherwise = go ((t, s) : cur) rest

-- | @Segment.split_lines@: lines without cropping or padding.
splitLines :: [Seg] -> [Line]
splitLines = go []
  where
    go cur [] = [reverse cur | not (null cur)]
    go cur ((t, s) : rest)
      | T.any (== '\n') t =
          let (before, after) = T.breakOn "\n" t
              cur' = if T.null before then cur else (before, s) : cur
           in reverse cur' : go [] (if T.length after > 1 then (T.drop 1 after, s) : rest else rest)
      | otherwise = go ((t, s) : cur) rest

applyStyle :: Style -> [Seg] -> [Seg]
applyStyle st
  | st == nullStyle = id
  | otherwise = map (\(t, s) -> (t, st <> s))

-- | @Console.render_lines@.
renderLines :: Options -> Style -> Bool -> Renderable -> [Line]
renderLines opts st pad r =
  let ls = splitAndCrop (oWidth opts) pad st (applyStyle st (render opts r))
   in case oHeight opts of
        Nothing -> ls
        Just h -> let ls' = take (max 0 h) ls in ls' <> replicate (h - length ls') [(T.replicate (oWidth opts) " ", st)]

-- | @Segment.set_shape@.
setShape :: Int -> Int -> Style -> [Line] -> [Line]
setShape width height st ls =
  let shaped = map (adjustLine width True st) (take height ls)
   in shaped <> replicate (height - length shaped) [(T.replicate width " ", st)]

alignCell :: VAlign -> Int -> Int -> Style -> [Line] -> [Line]
alignCell v width height st ls =
  let blankLine = [(T.replicate width " ", st)]
      extra = height - length ls
   in if extra <= 0
        then ls
        else case v of
          VTop -> ls <> replicate extra blankLine
          VBottom -> replicate extra blankLine <> ls
          VMiddle -> let top = extra `div` 2 in replicate top blankLine <> ls <> replicate (extra - top) blankLine

-- ---------------------------------------------------------------- render

-- | @Console.render@: a stream of segments, lines ending in @\\n@.
render :: Options -> Renderable -> [Seg]
render opts r
  | oWidth opts < 1 = []  -- Console.render: no space, nothing rendered
  | otherwise = render' opts r

render' :: Options -> Renderable -> [Seg]
render' opts = \case
  RTxt t ->
    let justify = Just (fromMaybe JDefault (tJustify t <|> oJustify opts))
        overflow = Just (fromMaybe OFold (tOverflow t <|> oOverflow opts))
        noWrap = Just (fromMaybe False (tNoWrap t <|> oNoWrap opts))
        lines' = wrap (oWidth opts) justify overflow (fromMaybe 8 (tTabSize t)) noWrap t
        joined = (joinTexts (plainText "\n") lines') {tEnd = tEnd t}
     in textSegments nullStyle joined
  RPadding (top, right, bottom, left) st expand inner ->
    let width = if expand then oWidth opts else min (snd (measure opts inner) + left + right) (oWidth opts)
        inner' = opts {oWidth = width - left - right, oHeight = fmap (\h -> h - top - bottom) (oHeight opts)}
        ls = renderLines inner' st True inner
        blankLine = [(T.replicate width " " <> "\n", st)]
        l = [(T.replicate left " ", st) | left > 0]
        r = [(T.replicate right " ", st) | right > 0] <> [newline]
     in concat (replicate top blankLine) <> concat [l <> line <> r | line <- ls] <> concat (replicate bottom blankLine)
  RTable tb -> renderTable opts tb
  RPanel p -> renderPanel opts p
  RAlign j inner ->
    -- Align: render at the measured width, then pad the block.
    let w0 = snd (measure opts inner)
        rendered = render opts {oWidth = min w0 (oWidth opts), oHeight = Nothing} inner
        ls = splitLines rendered
        width = maximum (0 : map (sum . map segLen) ls)
        shaped = setShape width (length ls) nullStyle ls
        excess = oWidth opts - width
        out line
          | excess <= 0 = line
          | otherwise = case j of
              JCenter -> let left = excess `div` 2 in [(T.replicate left " ", nullStyle) | left > 0] <> line <> [(T.replicate (excess - left) " ", nullStyle)]
              JRight -> (T.replicate excess " ", nullStyle) : line
              _ -> line <> [(T.replicate excess " ", nullStyle)]
     in concat [out line <> [newline] | line <- shaped]

-- | @Measurement.get@: (minimum, maximum) cells.
measure :: Options -> Renderable -> (Int, Int)
measure opts r
  | maxW < 1 = (0, 0)
  | otherwise =
      let (mn, mx) = withMaximum maxW (normalize (raw r))
       in if mx < 1 then (0, 0) else normalize (mn, mx)
  where
    maxW = oWidth opts
    raw = \case
      RTxt t -> measureText t
      RPadding (_, right, _, left) _ _ inner ->
        let extra = left + right
         in if maxW - extra < 1
              then (maxW, maxW)
              else let (a, b) = measure opts inner in withMaximum maxW (a + extra, b + extra)
      RTable tb -> measureTable opts tb
      RPanel p -> measurePanel opts p
      RAlign _ inner -> measure opts inner

normalize :: (Int, Int) -> (Int, Int)
normalize (mn, mx) = let mn' = min (max 0 mn) mx in (max 0 mn', max 0 (max mn' mx))

withMaximum :: Int -> (Int, Int) -> (Int, Int)
withMaximum w (a, b) = (min a w, min b w)

withMinimum :: Int -> (Int, Int) -> (Int, Int)
withMinimum w (a, b) = let w' = max 0 w in (max a w', max b w')

-- ------------------------------------------------------------------ ratio

-- | @ratio_reduce@.
ratioReduce :: Int -> [Int] -> [Int] -> [Int] -> [Int]
ratioReduce total ratios0 maximums values =
  let ratios = [if m /= 0 then r else 0 | (r, m) <- zip ratios0 maximums]
      totalRatio = sum ratios
   in if totalRatio == 0 then values else go total totalRatio (zip3 ratios maximums values)
  where
    go _ _ [] = []
    go remaining tr ((r, m, v) : rest)
      | r /= 0 && tr > 0 =
          let d = min m (pyRound (fromIntegral r * fromIntegral remaining / fromIntegral tr))
           in (v - d) : go (remaining - d) (tr - r) rest
      | otherwise = v : go remaining tr rest

-- | @ratio_distribute@.
ratioDistribute :: Int -> [Int] -> Maybe [Int] -> [Int]
ratioDistribute total ratios0 minimums =
  let ratios = case minimums of
        Just ms -> [if m /= 0 then r else 0 | (r, m) <- zip ratios0 ms]
        Nothing -> ratios0
      mins = fromMaybe (map (const 0) ratios) minimums
   in go total (sum ratios) (zip ratios mins)
  where
    go _ _ [] = []
    go remaining tr ((r, m) : rest) =
      let d = if tr > 0 then max m (ceiling (fromIntegral r * fromIntegral remaining / fromIntegral tr :: Double)) else remaining
       in d : go (remaining - d) (tr - r) rest

-- | Python's @round@ on a float: halves to even.
pyRound :: Double -> Int
pyRound = round

-- ----------------------------------------------------------------- tables

data Cell = Cell {clStyle :: Style, clBody :: Renderable, clVertical :: VAlign}

extraWidth :: Table -> Int
extraWidth tb = case tbBox tb of
  Nothing -> 0
  Just _ -> (if tbShowEdge tb then 2 else 0) + length (tbColumns tb) - 1

paddingWidth :: Table -> Int -> Int
paddingWidth tb i =
  let (_, r0, _, l0) = tbPadding tb
      (l1, r1) = if tbCollapsePadding tb then (0, abs (0 - r0)) else (l0, r0)
      l2 = if not (tbPadEdge tb) && i == 0 then 0 else l1
      r2 = if not (tbPadEdge tb) && i == length (tbColumns tb) - 1 then 0 else r1
   in l2 + r2

-- | @Table._get_cells@: header, then the column's cells, each padded.
columnCells :: Table -> Int -> Column -> [Cell]
columnCells tb i col =
  let (top0, right0, bottom0, left0) = tbPadding tb
      firstCol = i == 0
      lastCol = i == length (tbColumns tb) - 1
      padFor firstRow lastRow =
        let (l1, b1) =
              if tbCollapsePadding tb
                then (if not firstCol then max 0 (left0 - right0) else left0, if not lastRow then max 0 (top0 - bottom0) else bottom0)
                else (left0, bottom0)
            l2 = if not (tbPadEdge tb) && firstCol then 0 else l1
            r2 = if not (tbPadEdge tb) && lastCol then 0 else right0
            t2 = if not (tbPadEdge tb) && firstRow then 0 else top0
            b2 = if not (tbPadEdge tb) && lastRow then 0 else b1
         in (t2, r2, b2, l2)
      raw =
        [(tbHeaderStyle tb <> cHeaderStyle col, cHeader col) | tbShowHeader tb]
          <> [(cStyle col, row !! i) | row <- tbRows tb]
      n = length raw
      anyPad = let (a, b, c, d) = tbPadding tb in any (/= 0) [a, b, c, d]
   in [ Cell st (if anyPad then RPadding (padFor (k == 0) (k == n - 1)) nullStyle True body else body) (cVertical col)
      | (k, (st, body)) <- zip [0 ..] raw ]

measureColumn :: Table -> Options -> Int -> Column -> (Int, Int)
measureColumn tb opts i col
  | maxW < 1 = (0, 0)
  | Just w <- cWidth col = withMaximum maxW (w + pw, w + pw)
  | otherwise =
      let ms = [measure opts (clBody c) | c <- columnCells tb i col]
          mn = if null ms then 1 else maximum (map fst ms)
          mx = if null ms then maxW else maximum (map snd ms)
          m1 = withMaximum maxW (mn, mx)
          m2 = maybe m1 (\v -> withMinimum (v + pw) m1) (cMinWidth col)
       in maybe m2 (\v -> withMaximum (v + pw) m2) (cMaxWidth col)
  where
    maxW = oWidth opts
    pw = paddingWidth tb i

-- | @Table._collapse_widths@.
collapseWidths :: [Int] -> [Bool] -> Int -> [Int]
collapseWidths widths0 wrapable maxWidth
  | or wrapable = go widths0
  | otherwise = widths0
  where
    go ws =
      let total = sum ws
          excess = total - maxWidth
       in if total == 0 || excess <= 0
            then ws
            else
              let maxCol = maximum [w | (w, a) <- zip ws wrapable, a]
                  second = maximum (0 : [if a && w /= maxCol then w else 0 | (w, a) <- zip ws wrapable])
                  diff = maxCol - second
                  ratios = [if w == maxCol && a then 1 else 0 | (w, a) <- zip ws wrapable]
               in if not (any (/= 0) ratios) || diff == 0
                    then ws
                    else go (ratioReduce excess ratios (replicate (length ws) (min excess diff)) ws)

-- | @Table._calculate_column_widths@.
columnWidths :: Table -> Options -> [Int]
columnWidths tb opts =
  let cols = tbColumns tb
      maxW = oWidth opts
      ranges = [measureColumn tb opts i c | (i, c) <- zip [0 ..] cols]
      widths0 = [if mx == 0 then 1 else mx | (_, mx) <- ranges]
      flexible c = isJust (cRatio c)
      widths1
        | tbExpand tb && any (\c -> fromMaybe 0 (cRatio c) /= 0) (filter flexible cols) =
            let fixed = [if flexible c then 0 else mx | ((_, mx), c) <- zip ranges cols]
                flexMin = [fromMaybe 1 (cWidth c) + paddingWidth tb i | (i, c) <- zip [0 ..] cols, flexible c]
                flexWs = ratioDistribute (maxW - sum fixed) [fromMaybe 0 (cRatio c) | c <- cols, flexible c] (Just flexMin)
                place [] _ = []
                place ((i, c) : rest) fs
                  | flexible c, (f : fs') <- fs = (fixed !! i + f) : place rest fs'
                  | otherwise = (widths0 !! i) : place rest fs
             in place (zip [0 ..] cols) flexWs
        | otherwise = widths0
      tableWidth1 = sum widths1
      widths2
        | tableWidth1 > maxW =
            let c1 = collapseWidths widths1 [isNothing (cWidth c) && not (cNoWrap c) | c <- cols] maxW
                c2 = if sum c1 > maxW then ratioReduce (sum c1 - maxW) (map (const 1) c1) c1 c1 else c1
             in [snd (measureColumn tb opts {oWidth = w} i c) | (i, (w, c)) <- zip [0 ..] (zip c2 cols)]
        | otherwise = widths1
      tableWidth2 = if tableWidth1 > maxW then sum (collapsedOnly widths1) else tableWidth1
      collapsedOnly ws =
        let c1 = collapseWidths ws [isNothing (cWidth c) && not (cNoWrap c) | c <- cols] maxW
         in if sum c1 > maxW then ratioReduce (sum c1 - maxW) (map (const 1) c1) c1 c1 else c1
      minW = fmap (\m -> m - extraWidth tb) (tbMinWidth tb)
   in if (tableWidth2 < maxW && tbExpand tb) || maybe False (tableWidth2 <) minW
        then
          let target = maybe maxW (min maxW) minW
           in zipWith (+) widths2 (ratioDistribute (target - tableWidth2) widths2 Nothing)
        else widths2

measureTable :: Options -> Table -> (Int, Int)
measureTable opts tb
  | maxW0 < 0 = (0, 0)
  | otherwise =
      let extra = extraWidth tb
          maxW = sum (columnWidths tb opts {oWidth = maxW0 - extra})
          ms = [measureColumn tb opts {oWidth = maxW} i c | (i, c) <- zip [0 ..] (tbColumns tb)]
          mn = sum (map fst ms) + extra
          mx = maybe (sum (map snd ms) + extra) id (tbWidth tb)
       in maybe (mn, mx) (\m -> withMinimum m (mn, mx)) (tbMinWidth tb)
  where
    maxW0 = fromMaybe (oWidth opts) (tbWidth tb)

renderTable :: Options -> Table -> [Seg]
renderTable opts tb
  | null (tbColumns tb) = [newline]
  | otherwise =
      let maxW = fromMaybe (oWidth opts) (tbWidth tb)
          extra = extraWidth tb
          widths = columnWidths tb opts {oWidth = maxW - extra}
          tableWidth = sum widths + extra
          ropts = opts {oWidth = tableWidth, oHeight = Nothing}
          title = case tbTitle tb of
            Nothing -> []
            Just t ->
              let rt = (fromMarkup t) {tStyle = tbTitleStyle tb}
               in render ropts {oJustify = Just JCenter} (RTxt rt)
       in title <> renderTableBody ropts tb widths

renderTableBody :: Options -> Table -> [Int] -> [Seg]
renderTableBody opts tb widths =
  let cols = tbColumns tb
      border = tbBorderStyle tb
      cellsByCol = [columnCells tb i c | (i, c) <- zip [0 ..] cols]
      rows = transpose cellsByCol
      nrows = length rows
      box' = fmap (\b -> if tbShowHeader tb then b else plainHeaded b) (tbBox tb)
      edge = tbShowEdge tb
      top = case box' of
        Just b | edge -> [(edgeRow (bTop b) widths, border), newline]
        _ -> []
      bottom = case box' of
        Just b | edge -> [(edgeRow (bBottom b) widths, border), newline]
        _ -> []
      renderRow idx rowCells =
        let firstRow = idx == 0
            lastRow = idx == nrows - 1
            headerRow = firstRow && tbShowHeader tb
            rendered =
              [ renderLines opts {oWidth = w, oJustify = Just (cJustify c), oNoWrap = Just (cNoWrap c), oOverflow = cOverflow c, oHeight = Nothing}
                  (clStyle cell) True (clBody cell)
              | (w, cell, c) <- zip3 widths rowCells cols ]
            height = maximum (1 : map length rendered)
            shaped =
              [ setShape w height (clStyle cell)
                  (alignCell (if headerRow then VBottom else clVertical cell) w height (clStyle cell) ls)
              | (w, cell, ls) <- zip3 widths rowCells rendered ]
            body = case box' of
              Just b ->
                let (lft, _, vert, rgt) = if firstRow then bHead b else if lastRow then bFoot b else bMid b
                    divider = (T.singleton vert, border)
                 in concat
                      [ [(T.singleton lft, border) | edge]
                          <> concat (interleave divider [shaped !! k !! ln | k <- [0 .. length shaped - 1]])
                          <> [(T.singleton rgt, border) | edge]
                          <> [newline]
                      | ln <- [0 .. height - 1] ]
              Nothing -> concat [concat [shaped !! k !! ln | k <- [0 .. length shaped - 1]] <> [newline] | ln <- [0 .. height - 1]]
            headSep = case box' of
              Just b | firstRow && tbShowHeader tb -> [(boxRow b "head" edge widths, border), newline]
              _ -> []
            lineSep = case box' of
              Just b
                | tbShowLines tb && not lastRow && not (tbShowHeader tb && headerRow) ->
                    [(boxRow b "row" edge widths, border), newline]
              _ -> []
         in body <> headSep <> lineSep
   in top <> concat (zipWith renderRow [0 ..] rows) <> bottom
  where
    interleave _ [] = []
    interleave _ [x] = [x]
    interleave d (x : xs) = x : [d] : interleave d xs

-- ----------------------------------------------------------------- panels

panelTitle :: Panel -> Maybe RText
panelTitle p = case pTitle p of
  Nothing -> Nothing
  Just t | T.null t -> Nothing
  Just t ->
    let rt = fromMarkup t
        rt1 = setPlain (T.replace "\n" " " (tPlain rt)) rt
     in Just (padBoth 1 (expandTabs 8 rt1 {tNoWrap = Just True, tEnd = ""}))

measurePanel :: Options -> Panel -> (Int, Int)
measurePanel opts p =
  let (_, right, _, left) = pPadding p
      padding = left + right
      inner = opts {oWidth = oWidth opts - padding - 2}
      parts = [snd (measure inner (pBody p))] <> maybe [] (\t -> [snd (measure inner (RTxt t))]) (panelTitle p)
      w = fromMaybe (maximum parts + padding + 2) (pWidth p)
   in (w, w)

renderPanel :: Options -> Panel -> [Seg]
renderPanel opts p =
  let (t, r, b, l) = pPadding p
      body = if any (/= 0) [t, r, b, l] then RPadding (t, r, b, l) (pStyle p) True (pBody p) else pBody p
      border = pStyle p <> pBorderStyle p
      width0 = maybe (oWidth opts) (min (oWidth opts)) (pWidth p)
      box' = pBox p
      title = fmap (stylizeBefore border) (panelTitle p)
      childW0 = if pExpand p then width0 - 2 else snd (measure opts {oWidth = width0 - 2} body)
      childW = case title of
        Just tt -> min (oWidth opts - 2) (max childW0 (cellLength tt + 2))
        Nothing -> childW0
      width = childW + 2
      childOpts = opts {oWidth = childW, oHeight = fmap (subtract 2) (oHeight opts)}
      ls = renderLines childOpts (pStyle p) True body
      (tl, tc, _, tr) = bTop box'
      (ml, _, _, mr) = bMid box'
      topLine = case title of
        Just tt | width > 4 ->
          let aligned = alignTitle tt (width - 4) (pTitleAlign p) tc border
           in [(T.pack [tl, tc], border)] <> render childOpts {oWidth = width - 4} (RTxt aligned) <> [(T.pack [tc, tr], border)]
        _ -> [(edgeRow (bTop box') [width - 2], border)]
   in topLine <> [newline]
        <> concat [[(T.singleton ml, border)] <> line <> [(T.singleton mr, border), newline] | line <- ls]
        <> [(edgeRow (bBottom box') [width - 2], border), newline]

alignTitle :: RText -> Int -> Justify -> Char -> Style -> RText
alignTitle t0 width align ch st =
  let t = truncateText width Nothing False t0
      excess = width - cellLength t
      fill k = (plainText (T.replicate k (T.singleton ch))) {tStyle = st}
      assemble parts = (joinTexts (plainText "") parts) {tNoWrap = Just True, tEnd = ""}
   in if excess <= 0
        then t
        else case align of
          JLeft -> assemble [t, fill excess]
          JCenter -> let left = excess `div` 2 in assemble [fill left, t, fill (excess - left)]
          _ -> assemble [fill excess, t]

-- ---------------------------------------------------------------- console

-- | @Console.print@ into a console @width@ wide: lines, not padded.
-- Strings and texts given to @print@ are joined into one new text that
-- carries only @print@'s own justify (@_collect_renderables@), so a text's
-- justify, overflow and no-wrap settings do not survive at the top level.
printLines :: Int -> Maybe Justify -> Renderable -> [Line]
printLines width justify r = splitAndCrop width False nullStyle (render (options width) {oJustify = justify} aligned)
  where
    r' = case r of
      RTxt t -> RTxt t {tJustify = justify, tOverflow = Nothing, tNoWrap = Nothing, tEnd = "\n", tTabSize = Nothing}
      other -> other
    -- left, center and right wrap the renderable in Align
    aligned = case justify of
      Just j | j `elem` [JLeft, JCenter, JRight] -> RAlign j r'
      _ -> r'

linePlain :: Line -> Text
linePlain = T.concat . map fst

lineAnsi :: Line -> Text
lineAnsi = T.concat . map (\(t, s) -> styled s t)
