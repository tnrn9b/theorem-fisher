-- | @tools.fuzzy_match.fuzzy_find_and_replace@: eight matching strategies
-- tried in upstream's order (exact, line-trimmed, whitespace-normalized,
-- indentation-flexible, escape-normalized, trimmed-boundary, block-anchor,
-- context-aware); the first that finds anything decides. Positions and
-- their quirks (the normalized-position mapping, end offsets clipped to
-- the content) are reproduced exactly.
module Fisher.Tools.Fuzzy
  ( fuzzyFindAndReplace
  , exactFindAndReplace
  ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Difflib (ratio)
import Fisher.Py.Re (compileOrDie, subText)
import Fisher.Tools.Py (pyStrip)

type Span = (Int, Int)

-- | @(new_content, match_count, error)@.
fuzzyFindAndReplace :: Text -> Text -> Text -> Bool -> (Text, Int, Maybe Text)
fuzzyFindAndReplace = findAndReplaceWith allStrategies

-- | Fisher only: the exact strategy alone (for the CLM context file,
-- where a fuzzy match could land on the wrong turn).
exactFindAndReplace :: Text -> Text -> Text -> Bool -> (Text, Int, Maybe Text)
exactFindAndReplace = findAndReplaceWith [strategyExact]

findAndReplaceWith :: [Text -> Text -> [Span]] -> Text -> Text -> Text -> Bool -> (Text, Int, Maybe Text)
findAndReplaceWith strategies content old new replaceAll
  | T.null old = (content, 0, Just "old_string cannot be empty")
  | old == new = (content, 0, Just "old_string and new_string are identical")
  | otherwise = go strategies
  where
    go [] = (content, 0, Just "Could not find a match for old_string in the file")
    go (s : rest) = case s content old of
      [] -> go rest
      ms
        | length ms > 1 && not replaceAll ->
            ( content
            , 0
            , Just ("Found " <> T.pack (show (length ms)) <> " matches for old_string. Provide more context to make it unique, or use replace_all=True.")
            )
        | otherwise -> (applyReplacements content ms new, length ms, Nothing)

allStrategies :: [Text -> Text -> [Span]]
allStrategies =
      [ strategyExact
      , strategyLineTrimmed
      , strategyWhitespaceNormalized
      , strategyIndentationFlexible
      , strategyEscapeNormalized
      , strategyTrimmedBoundary
      , strategyBlockAnchor
      , strategyContextAware
      ]

-- | Replace from the last position to the first, as upstream does (so
-- overlapping spans behave as in Python's string slicing).
applyReplacements :: Text -> [Span] -> Text -> Text
applyReplacements content ms new = foldl step content (sortBy (comparing (Down . fst)) ms)
  where
    step acc (s, e) = T.take s acc <> new <> T.drop e acc

strategyExact :: Text -> Text -> [Span]
strategyExact content pat = go 0
  where
    plen = T.length pat
    go start =
      case findFrom start of
        Nothing -> []
        Just pos -> (pos, pos + plen) : go (pos + 1)
    findFrom start =
      let (before, after) = T.breakOn pat (T.drop start content)
       in if T.null after then Nothing else Just (start + T.length before)

-- Python's str.split('\n') and '\n'.join
splitNl :: Text -> [Text]
splitNl = T.splitOn "\n"

joinNl :: [Text] -> Text
joinNl = T.intercalate "\n"

-- | Start of line @i@ and the end of a block of @n@ lines from @i@, both as
-- offsets into the original content.
blockSpan :: Text -> [Int] -> Int -> Int -> Span
blockSpan content lineLens i n =
  let start = sum (map (+ 1) (take i lineLens))
      end0 = sum (map (+ 1) (take (i + n) lineLens)) - 1
   in (start, if end0 >= T.length content then T.length content else end0)

findNormalizedMatches :: Text -> [Text] -> [Text] -> Text -> [Span]
findNormalizedMatches content contentLines normLines patNorm =
  let patLines = splitNl patNorm
      n = length patLines
      lens = map T.length contentLines
      total = length normLines
   in [ blockSpan content lens i n
      | i <- [0 .. total - n]
      , joinNl (take n (drop i normLines)) == patNorm
      ]

strategyLineTrimmed :: Text -> Text -> [Span]
strategyLineTrimmed content pat =
  let patNorm = joinNl (map pyStrip (splitNl pat))
      cl = splitNl content
   in findNormalizedMatches content cl (map pyStrip cl) patNorm

wsRe :: Text -> Text
wsRe = subText (compileOrDie [] "[ \\t]+") " "

strategyWhitespaceNormalized :: Text -> Text -> [Span]
strategyWhitespaceNormalized content pat =
  let pn = wsRe pat
      cn = wsRe content
   in case strategyExact cn pn of
        [] -> []
        ms -> mapNormalizedPositions content cn ms

strategyIndentationFlexible :: Text -> Text -> [Span]
strategyIndentationFlexible content pat =
  let cl = splitNl content
      pl = map (T.dropWhile isPyWsL) (splitNl pat)
   in findNormalizedMatches content cl (map (T.dropWhile isPyWsL) cl) (joinNl pl)
  where
    isPyWsL c = T.null (pyStrip (T.singleton c))

strategyEscapeNormalized :: Text -> Text -> [Span]
strategyEscapeNormalized content pat =
  let un = T.replace "\\r" "\r" (T.replace "\\t" "\t" (T.replace "\\n" "\n" pat))
   in if un == pat then [] else strategyExact content un

strategyTrimmedBoundary :: Text -> Text -> [Span]
strategyTrimmedBoundary content pat =
  let pl0 = splitNl pat
      trimEnds xs = case xs of
        [] -> []
        [x] -> [pyStrip x]
        (x : rest) -> pyStrip x : (init rest <> [pyStrip (last rest)])
      modified = joinNl (trimEnds pl0)
      cl = splitNl content
      lens = map T.length cl
      n = length pl0
   in [ blockSpan content lens i n
      | i <- [0 .. length cl - n]
      , joinNl (trimEnds (take n (drop i cl))) == modified
      ]

unicodeNormalize :: Text -> Text
unicodeNormalize t0 = foldl (\t (c, r) -> T.replace (T.singleton c) r t) t0 table
  where
    table =
      [ ('\x201c', "\""), ('\x201d', "\""), ('\x2018', "'"), ('\x2019', "'")
      , ('\x2014', "--"), ('\x2013', "-"), ('\x2026', "..."), ('\x00a0', " ")
      ]

strategyBlockAnchor :: Text -> Text -> [Span]
strategyBlockAnchor content pat =
  let normPat = unicodeNormalize pat
      normContent = unicodeNormalize content
      pl = splitNl normPat
      n = length pl
      firstL = pyStrip (head pl)
      lastL = pyStrip (last pl)
      ncl = splitNl normContent
      ocl = splitNl content
      lens = map T.length ocl
      at xs k = xs !! k
      potential =
        [ i
        | i <- [0 .. length ncl - n]
        , pyStrip (at ncl i) == firstL
        , pyStrip (at ncl (i + n - 1)) == lastL
        ]
      threshold = if length potential == 1 then 0.10 else 0.30 :: Double
      sim i
        | n <= 2 = 1.0
        | otherwise =
            ratio
              (T.unpack (joinNl (take (n - 2) (drop (i + 1) ncl))))
              (T.unpack (joinNl (take (n - 2) (drop 1 pl))))
      spanOf i =
        let start = sum (map (+ 1) (take i lens))
            end0 = sum (map (+ 1) (take (i + n) lens)) - 1
         in (start, min end0 (T.length content))
   in if n < 2 then [] else [spanOf i | i <- potential, sim i >= threshold]

strategyContextAware :: Text -> Text -> [Span]
strategyContextAware content pat =
  let pl = splitNl pat
      cl = splitNl content
      lens = map T.length cl
      n = length pl
      high i =
        length
          [ ()
          | (p, c) <- zip pl (take n (drop i cl))
          , ratio (T.unpack (pyStrip p)) (T.unpack (pyStrip c)) >= 0.80
          ]
   in [ blockSpan content lens i n
      | i <- [0 .. length cl - n]
      , fromIntegral (high i) >= (fromIntegral n * 0.5 :: Double)
      ]

-- | @_map_normalized_positions@, step for step.
mapNormalizedPositions :: Text -> Text -> [Span] -> [Span]
mapNormalizedPositions original normalized ms =
  let o = T.unpack original
      nrm = T.unpack normalized
      olen = length o
      nlen = length nrm
      -- orig_to_norm, built as upstream's while loop does
      build oi ni os ns acc
        | oi >= olen || ni >= nlen = reverse acc <> replicate (olen - oi) nlen
        | otherwise = case (os, ns) of
            (oc : orest, nc : _)
              | oc == nc -> build (oi + 1) (ni + 1) orest (drop 1 ns) (ni : acc)
              | oc `elem` (" \t" :: String) && nc == ' ' ->
                  let ni' = case orest of
                        (nxt : _) | nxt `notElem` (" \t" :: String) -> ni + 1
                        _ -> ni
                      ns' = if ni' /= ni then drop 1 ns else ns
                   in build (oi + 1) ni' orest ns' (ni : acc)
              | oc `elem` (" \t" :: String) -> build (oi + 1) ni orest ns (ni : acc)
              | otherwise -> build (oi + 1) ni orest ns (ni : acc)
            _ -> reverse acc
      o2n = build 0 0 o nrm []
      startMap = M.fromListWith (\_ old -> old) [(nPos, oPos) | (oPos, nPos) <- zip [0 ..] o2n]
      endMap = M.fromList [(nPos, oPos) | (oPos, nPos) <- zip [0 ..] o2n]
      ov = T.unpack original
      isWsAt k = k < olen && (ov !! k) `elem` (" \t" :: String)
      mapOne (ns, ne) =
        let os = case M.lookup ns startMap of
              Just v -> v
              Nothing -> minimum [i | (i, nPos) <- zip [0 ..] o2n, nPos >= ns]
            oe0 = case M.lookup (ne - 1) endMap of
              Just v -> v + 1
              Nothing -> os + (ne - ns)
            oe = until (\k -> not (isWsAt k)) (+ 1) oe0
         in (os, min oe olen)
   in map mapOne ms
