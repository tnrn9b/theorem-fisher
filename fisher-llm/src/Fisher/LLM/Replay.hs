-- | Replaying a run that @tools/corpus/b1_auth.py@ recorded from upstream:
-- the same files, environment, clock, HTTP answers, subprocess results
-- and typed input, through a scripted 'Sys'; then what Fisher did, in the
-- recording's form, so a spec can compare the two. Used by the golden
-- specs of fisher-llm and fisher-cli.
module Fisher.LLM.Replay
  ( Case (..)
  , loadCases
  , World (..)
  , Observed (..)
  , replay
  , replaySteps
  , cleanTranscript
  , maskText
  , maskVal
  , snapshotFiles
  , expectedFiles
  , httpLog
  ) where

import Control.Exception (AsyncException (UserInterrupt), SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.IORef
import Data.List (isSuffixOf, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Time (TimeZone (..), utc)
import Data.Time.Calendar (addDays, dayOfWeek, fromGregorian, toGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Numeric (readOct, showOct)
import System.Directory
import System.FilePath
import System.IO.Temp (createTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes, setFileMode)

import Fisher.LLM.Sys
import Fisher.Py.Value

-- | One recorded case (or a later step of one).
newtype Case = Case PyVal

loadCases :: FilePath -> IO (PyVal, [Case])
loadCases path = do
  doc <- either fail pure . loads =<< BS.readFile path
  let cs = case get "cases" doc of
        Just (PList xs) -> xs
        _ -> []
  pure (doc, map Case cs)

data World = World
  { wRoot :: FilePath
  , wHome :: FilePath
  , wFisherHome :: FilePath
  }

data Observed = Observed
  { oTranscript :: Text
  , oExit :: Int
  , oHttp :: [PyVal]
  , oSleeps :: [Double]
  , oBrowser :: [Text]
  , oSubprocess :: [[Text]]
  , oPrompts :: [PyVal]
  , oInputsLeft :: Int
  }

-- | A directory under @base@ whose canonical path is exactly @len@ long.
padded :: FilePath -> String -> Int -> IO FilePath
padded base prefix len = do
  b <- canonicalizePath base
  d <- createTempDirectory b prefix >>= canonicalizePath
  let k = len - length d - 1
  when (k < 1) (fail ("scratch base " <> d <> " is too long for " <> show len))
  let p = d </> replicate k 'p'
  createDirectory p
  pure p

makeWorld :: IO World
makeWorld = do
  root <- padded "/tmp" "b1r" 100
  gh <- padded "/var/tmp" "b1g" 90
  let home = root </> "home"
  createDirectoryIfMissing True home
  pure (World root home gh)

-- | The placeholders a recording uses, longest first.
placeholders :: World -> [(Text, Text)]
placeholders w =
  [ (T.pack (wFisherHome w), "<GAUSS_HOME>")
  , (T.pack (wHome w), "<HOME>")
  , (T.pack (wRoot w), "<TMP>") ]

-- | Scratch paths to their tags, also when a /private prefix (macOS) or a
-- shortened form appears.
maskText :: World -> Text -> Text
maskText w t0 = foldl (\t (p, tag) -> T.replace ("/private" <> p) tag (T.replace p tag t)) t0 (placeholders w)

unmask :: World -> Text -> Text
unmask w t0 = foldl (\t (p, tag) -> T.replace tag p t) t0 (placeholders w)

maskVal :: World -> PyVal -> PyVal
maskVal w = \case
  PStr s -> PStr (maskText w s)
  PList xs -> PList (map (maskVal w) xs)
  PDict kvs -> PDict [(maskText w k, maskVal w v) | (k, v) <- kvs]
  v -> v

-- | The recording's transcript form: ANSI escapes out, @\\r@ out, every
-- line right-trimmed.
cleanTranscript :: Text -> Text
cleanTranscript = T.intercalate "\n" . map T.stripEnd . T.splitOn "\n" . T.replace "\r" "" . stripAnsi
  where
    stripAnsi t = case T.breakOn "\ESC" t of
      (a, b) | T.null b -> a
      (a, b) ->
        let rest = T.drop 1 b
         in a <> case T.uncons rest of
              Just ('[', r) -> stripAnsi (T.drop 1 (T.dropWhile (\c -> isDigit c || c `elem` (";?" :: String)) r))
              Just (']', r) -> stripAnsi (T.drop 1 (T.dropWhile (/= '\a') r))
              Just (_, r) -> stripAnsi r
              Nothing -> ""

str' :: Text -> PyVal -> Text
str' k v = maybe "" pyStrOf (get k v)

-- | Lay the case's files down: @{"gauss_home": {...}, "home": {...}}@.
-- The recordings come from upstream, so their own names (@gauss_home@,
-- @<GAUSS_HOME>@, @GAUSS_HOME@) stay; @gauss_home@ is the Fisher home.
writeFiles :: World -> PyVal -> IO ()
writeFiles w files = do
  forM_ [("gauss_home", wFisherHome w), ("home", wHome w)] $ \(side, base) ->
    forM_ (maybe [] (\case PDict kvs -> kvs; _ -> []) (get side files)) $ \(rel, spec) -> do
      let p = base </> T.unpack rel
          mode = fst . head <$> nonEmpty (readOct (T.unpack (str' "mode" spec)))
      if "/" `T.isSuffixOf` rel
        then createDirectoryIfMissing True p
        else do
          createDirectoryIfMissing True (takeDirectory p)
          BS.writeFile p (TE.encodeUtf8 (unmask w (str' "text" spec)))
      forM_ mode $ \m -> setFileMode p m
  where
    nonEmpty xs = if null xs then Nothing else Just xs

-- | Every file and directory under the home and the Fisher home, in the
-- recording's form. Fisher's own @fisher/@ directory is left out.
snapshotFiles :: World -> Text -> IO PyVal
snapshotFiles w soul = do
  sides <- forM [("gauss_home", wFisherHome w), ("home", wHome w)] $ \(side, base) -> do
    exists <- doesDirectoryExist base
    entries <- if exists then walk base "" else pure []
    pure (side, PDict entries)
  pure (PDict sides)
  where
    walk base rel = do
      names <- sort <$> listDirectory (base </> rel)
      fmap concat . forM names $ \n -> do
        let r = if null rel then n else rel </> n
            p = base </> r
        if r == "fisher" && base == wFisherHome w
          then pure []
          else do
            isDir <- doesDirectoryExist p
            st <- getFileStatus p
            let mode = T.justifyRight 4 '0' (T.pack (showOct (fromIntegral (fileMode st `intersectFileModes` 0o7777) :: Int) ""))
            if isDir
              then do
                inner <- walk base r
                pure ((T.pack (r <> "/"), PDict [("mode", PStr mode)]) : inner)
              else do
                bytes <- BS.readFile p
                let text = maskText w (TE.decodeUtf8With TEE.lenientDecode bytes)
                    entry
                      | n == "SOUL.md" && text == soul = [("mode", PStr mode), ("same_as", str "soul_md")]
                      | ".json" `isSuffixOf` n, Right j <- loads (TE.encodeUtf8 text) = [("mode", PStr mode), ("text", PStr text), ("json", j)]
                      | otherwise = [("mode", PStr mode), ("text", PStr text)]
                pure [(T.pack r, PDict entry)]

-- | The recording's final files, for comparison with 'snapshotFiles'.
expectedFiles :: PyVal -> PyVal
expectedFiles c = fromMaybe (obj []) (get "final_files" c)

-- | The recorded HTTP log, without the parts Fisher does not reproduce
-- (httpx's own @accept: */*@).
httpLog :: PyVal -> [PyVal]
httpLog c = case get "http" c of
  Just (PList xs) -> map trim xs
  _ -> []
  where
    trim e =
      let hs = case get "headers" e of
            Just (PDict kvs) -> PDict [(k, v) | (k, v) <- kvs, not (k == "accept" && v == PStr "*/*")]
            _ -> PDict []
       in set "headers" hs e

-- | US Eastern for America/New_York, else UTC: the zones the recordings use.
zoneFor :: Maybe Text -> Double -> TimeZone
zoneFor tz t = case tz of
  Just "America/New_York" ->
    let UTCTime day secs = posixSecondsToUTCTime (realToFrac t)
        (y, _, _) = toGregorian day
        secondSunday m = nthSunday y m 2
        firstSunday m = nthSunday y m 1
        dstStart = UTCTime (secondSunday 3) (secondsToDiffTime (7 * 3600))
        dstEnd = UTCTime (firstSunday 11) (secondsToDiffTime (6 * 3600))
        now = UTCTime day secs
     in if now >= dstStart && now < dstEnd then TimeZone (-240) True "EDT" else TimeZone (-300) False "EST"
  _ -> utc
  where
    nthSunday y m n =
      let first = fromGregorian y m 1
          off = (7 - fromEnum (dayOfWeek first) `mod` 7) `mod` 7
       in addDays (toInteger (off + 7 * (n - 1))) first

-- | Run one recorded case: @action@ gets the scripted Sys and returns the
-- exit code. The world is fresh unless one is given (later steps of a
-- case share it).
replay :: PyVal -> Maybe World -> PyVal -> (Sys -> IO Int) -> IO (World, Observed)
replay doc mworld c action = do
  w <- maybe makeWorld pure mworld
  case mworld of
    Nothing -> writeFiles w (fromMaybe (obj []) (get "initial_files" c))
    Just _ -> pure ()
  let epoch = case get "epoch" c of
        Just (PInt e) -> fromInteger e
        Just (PFloat e) -> e
        _ -> case get "epoch" doc of
          Just (PInt e) -> fromInteger e
          _ -> 1790000000
      base = case get "env_base" doc of
        Just (PDict kvs) -> [(k, pyStrOf v) | (k, v) <- kvs]
        _ -> []
      caseEnv = case get "env" c of
        Just (PDict kvs) -> kvs
        _ -> []
      ghEnv = case lookup "GAUSS_HOME" caseEnv of
        Just PNull -> []
        _ -> [("GAUSS_HOME", T.pack (wFisherHome w))]
      env0 = Map.fromList (base <> [("HOME", T.pack (wHome w))] <> ghEnv <> [(k, unmask w (pyStrOf v)) | (k, v) <- caseEnv, v /= PNull])
      scen = fromMaybe (obj []) (get "scenario" c)
      pty = get "mode" c == Just (PStr "pty")
      platform = case get "platform" scen of
        Just (PStr p) -> p
        _ -> "linux"
      routes = case get "routes" scen of
        Just (PList rs) -> [((T.toUpper (str' "method" r), str' "url" r), case get "responses" r of Just (PList xs) -> xs; _ -> []) | r <- rs]
        _ -> []
      subRules = case get "subprocess" scen of
        Just (PList rs) -> rs
        _ -> []
      whichMap = case get "which" scen of
        Just (PDict kvs) -> [(k, v) | (k, v) <- kvs]
        _ -> []
      browserOk = get "browser" scen == Just (PBool True)
      inputs0 = case get "inputs" c of
        Just (PList xs) -> [pyStrOf x | x <- xs]
        _ -> []
  clock <- newIORef epoch
  envRef <- newIORef env0
  outRef <- newIORef ([] :: [Text])
  httpRef <- newIORef ([] :: [PyVal])
  routeRef <- newIORef (Map.fromListWith (flip (<>)) routes)
  sleepRef <- newIORef ([] :: [Double])
  browserRef <- newIORef ([] :: [Text])
  subRef <- newIORef ([] :: [[Text]])
  promptRef <- newIORef ([] :: [PyVal])
  inputRef <- newIORef inputs0
  uuidRef <- newIORef (0 :: Integer)
  let say t = modifyIORef outRef (t :)
      nextInput kind prompt = do
        modifyIORef promptRef (<> [obj [("kind", str kind), ("prompt", str prompt)]])
        xs <- readIORef inputRef
        case xs of
          [] -> pure Nothing
          (x : rest) -> writeIORef inputRef rest >> pure (Just x)
      route req = do
        let key = (T.toUpper (arMethod req), arUrl req)
            hs = PDict (sortOn fst [(T.toLower k, str v) | (k, v) <- arHeaders req, T.toLower k `elem` ["authorization", "content-type", "accept", "x-api-key", "anthropic-version", "anthropic-beta"]])
            body = case arBody req of
              NoBody -> PNull
              FormBody kvs -> obj [("form", PDict [(k, str v) | (k, v) <- kvs])]
              JsonBody j -> obj [("json", j)]
            via = case arVia req of
              ViaHttpx -> "httpx"
              ViaUrllib -> "urllib"
            entry0 = obj [("via", str via), ("method", str (arMethod req)), ("url", str (arUrl req)), ("headers", hs), ("body", body), ("timeout", if arTimeout req == fromInteger (round (arTimeout req)) then PInt (round (arTimeout req)) else PFloat (arTimeout req))]
        rs <- readIORef routeRef
        case Map.lookup key rs of
          Nothing -> do
            modifyIORef httpRef (<> [set "unmatched" (PBool True) entry0])
            pure $ case arVia req of
              ViaHttpx -> Right (AuthResp 599 [("content-type", "text/plain")] "b1 mock: no route")
              ViaUrllib -> Left (HttpFail "URLError" "<urlopen error b1 mock: no route>")
          Just [] -> pure (Left (HttpFail "URLError" "<urlopen error b1 mock: no route>"))
          Just (resp : rest) -> do
            unless (null rest) (modifyIORef routeRef (Map.insert key rest))
            case get "raise" resp of
              Just (PStr msg) -> do
                modifyIORef httpRef (<> [set "raised" (str msg) entry0])
                pure . Left $ case arVia req of
                  ViaHttpx -> HttpFail "ConnectError" msg
                  ViaUrllib -> HttpFail "URLError" ("<urlopen error " <> msg <> ">")
              _ -> do
                let status = case get "status" resp of
                      Just (PInt s) -> fromInteger s
                      _ -> 200
                    (bytes, ctype) = case get "json" resp of
                      Just j -> (TE.encodeUtf8 (dumpsAscii j), "application/json")
                      Nothing -> (TE.encodeUtf8 (str' "text" resp), "text/plain")
                    extra = case get "headers" resp of
                      Just (PDict kvs) -> [(k, pyStrOf v) | (k, v) <- kvs]
                      _ -> []
                modifyIORef httpRef (<> [set "status" (PInt (toInteger status)) entry0])
                pure (Right (AuthResp status (("content-type", ctype) : extra) bytes))
      ruleFor argv = case argv of
        [] -> Nothing
        (a0 : rest) ->
          let baseName = T.pack (takeFileName (T.unpack a0))
           in case [r | r <- subRules, Just (PList want) <- [get "argv" r], (PStr w0 : wrest) <- [want], w0 == baseName, take (length wrest) rest == [x | PStr x <- wrest]] of
                (r : _) -> Just r
                [] -> Nothing
      runRule argv = do
        modifyIORef subRef (<> [argv])
        case ruleFor argv of
          Nothing -> pure (Left "[Errno 2] No such file or directory")
          Just r
            | get "missing" r == Just (PBool True) -> pure (Left "[Errno 2] No such file or directory")
            | otherwise -> do
                forM_ (case get "write_files" r of Just (PDict kvs) -> kvs; _ -> []) $ \(path, content) -> do
                  let p = T.unpack (unmask w (T.replace "~" (T.pack (wHome w)) path))
                  createDirectoryIfMissing True (takeDirectory p)
                  TIO.writeFile p (pyStrOf content)
                forM_ (get "print" r) $ \pr -> say (pyStrOf pr)
                pure (Right (RunResult (case get "returncode" r of Just (PInt n) -> fromInteger n; _ -> 0) (str' "stdout" r) (str' "stderr" r)))
      sys =
        Sys
          { sysNow = readIORef clock
          , sysMonotonic = (\t -> t - epoch + 1000) <$> readIORef clock
          , sysSleep = \s -> modifyIORef sleepRef (<> [s]) >> modifyIORef clock (+ s)
          , sysGetEnv = \k -> Map.lookup k <$> readIORef envRef
          , sysSetEnv = \k v -> modifyIORef envRef (Map.insert k v)
          , sysHttp = route
          , sysOpenBrowser = \u -> modifyIORef browserRef (<> [u]) >> pure browserOk
          , sysRun = runRule
          , sysRunInteractive = \argv -> fmap (fmap rrCode) (runRule argv)
          , sysWhich = \cmd -> pure (case lookup cmd whichMap of Just (PStr p) -> Just (T.unpack (unmask w p)); _ -> Nothing)
          , sysPlatform = platform
          , sysUuidHex = do
              n <- atomicModifyIORef' uuidRef (\x -> (x + 1, x + 1))
              pure (T.justifyRight 32 '0' (T.pack (showHex' n)))
          , sysTimeZoneAt = \t -> do
              tz <- Map.lookup "TZ" <$> readIORef envRef
              pure (zoneFor tz t)
          , sysHome = wHome w
          , sysFisherHome = case lookup "GAUSS_HOME" caseEnv of
              Just PNull -> wHome w </> ".gauss"
              _ -> wFisherHome w
          , sysOut = say
          , sysInput = \prompt -> do
              say prompt
              -- What the terminal shows: the typed line echoed, @^C@ for
              -- Ctrl-C and nothing for ^D. Any newline after those is the
              -- program's own (upstream's handlers print one).
              nextInput "input" prompt >>= \case
                Nothing -> pure Nothing
                Just "<EOF>" -> pure Nothing
                Just "<CTRL-C>" -> when pty (say "^C") >> throwIO UserInterrupt
                Just x -> when pty (say (x <> "\n")) >> pure (Just x)
          , sysGetPass = \prompt -> do
              say prompt
              r <- nextInput (if pty then "getpass" else "getpass-notty") prompt
              when pty (say "\n")
              case r of
                Nothing -> pure Nothing
                Just "<EOF>" -> pure Nothing
                Just "<CTRL-C>" -> throwIO UserInterrupt
                Just x -> pure (Just x)
          , sysStdinTty = pure pty
          , sysStdoutTty = pure pty
          , sysPid = 4242
          }
  r <- try (action sys) :: IO (Either SomeException Int)
  code <- case r of
    Right n -> pure n
    Left e -> say ("\n<uncaught: " <> T.pack (show e) <> ">\n") >> pure 99
  transcript <- T.concat . reverse <$> readIORef outRef
  obs <-
    Observed (maskText w (cleanTranscript transcript)) code
      <$> (map (maskVal w) <$> readIORef httpRef)
      <*> readIORef sleepRef
      <*> readIORef browserRef
      <*> (map (map (maskText w)) <$> readIORef subRef)
      <*> readIORef promptRef
      <*> (length <$> readIORef inputRef)
  pure (w, obs)
  where
    showHex' n = let s = go n "" in if null s then "0" else s
      where
        go 0 acc = acc
        go k acc = go (k `div` 16) ("0123456789abcdef" !! fromInteger (k `mod` 16) : acc)

-- | A case and its @then@ steps, sharing one world, the clock an hour on
-- per step.
replaySteps :: PyVal -> PyVal -> (PyVal -> Sys -> IO Int) -> IO [(PyVal, World, Observed)]
replaySteps doc c action = do
  (w, o) <- replay doc Nothing c (action c)
  let steps = case get "then" c of
        Just (PList xs) -> xs
        _ -> []
  rest <- forM steps $ \s -> do
    (_, o') <- replay doc (Just w) s (action s)
    pure (s, w, o')
  pure ((c, w, o) : rest)
