module Fisher.RLM.LogSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (isPrefixOf, isSuffixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Fisher.Py.Value (PyVal (..), get, loads)
import Fisher.RLM.Config (RlmConfig (..), defaultRlmConfig)
import Fisher.RLM.Log
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl (CallKind (..))
import Fisher.RLM.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

cfg :: RlmConfig
cfg = defaultRlmConfig {rcModel = "test/root", rcSubModel = Just "test/sub"}

record :: IterationRecord
record =
  IterationRecord
    { irPrompt = [("system", "sys"), ("user", "go \19990\30028")]
    , irResponse = "Trying.\n```repl\nx = 1\n```"
    , irBlocks =
        [ ( "x = llm_query('hi')\nprint(x)"
          , BlockResult "hello\n" "" ["x", "n"] ["str", "int"] Nothing
          , [ SubCall CallLlm "hi" "hello" 1 "test/sub"
            , SubCall CallRlm "why? caf\233" "because" 1 "test/root"
            ]
          )
        , ("1/0", BlockResult "" "\nZeroDivisionError: division by zero" [] [] (Just "42"), [])
        ]
    , irFinal = Just "42"
    , irSeconds = 1.5
    }

parseLine :: BS.ByteString -> PyVal
parseLine l = either (error . ("bad JSON line: " ++)) id (loads l)

-- | Replace a timestamp by "" after checking there is one.
blank :: PyVal -> IO PyVal
blank (PDict kvs) = case lookup "timestamp" kvs of
  Just (PStr t) | not (T.null t) -> pure (PDict [(k, if k == "timestamp" then PStr "" else v) | (k, v) <- kvs])
  other -> expectationFailure ("timestamp missing or empty: " ++ show other) >> pure (PDict kvs)
blank v = expectationFailure "entry is not an object" >> pure v

isoLike :: T.Text -> Bool
isoLike t = case T.unpack t of
  (y1 : y2 : y3 : y4 : '-' : _) -> all (`elem` ['0' .. '9']) [y1, y2, y3, y4] && 'T' `elem` T.unpack t
  _ -> False

spec :: Spec
spec = describe "Fisher.RLM.Log" $ do
  golden <- runIO (BS.readFile "test/golden/log.json")
  let g = either (error . ("log.json: " ++)) id (loads golden)
      want k = maybe (error ("golden lacks " ++ k)) id (get (T.pack k) g)

  it "writes the reference's metadata and iteration lines, key for key" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg
      logIteration t record
      ls <- BS.split 10 <$> BS.readFile (trajectoryPath t)
      length ls `shouldBe` 3
      last ls `shouldBe` ""
      meta <- blank (parseLine (head ls))
      it1 <- blank (parseLine (ls !! 1))
      meta `shouldBe` want "metadata"
      it1 `shouldBe` want "iteration"

  it "stamps both entries with an ISO local time" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg
      logIteration t record
      ls <- filter (not . BS.null) . BS.split 10 <$> BS.readFile (trajectoryPath t)
      mapM_
        ( \l -> case get "timestamp" (parseLine l) of
            Just (PStr s) -> (T.unpack s, isoLike s) `shouldSatisfy` snd
            other -> expectationFailure ("timestamp: " ++ show other)
        )
        ls

  it "numbers iterations from 1 and appends one line each" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg
      mapM_ (logIteration t) [record, record, record]
      ls <- filter (not . BS.null) . BS.split 10 <$> BS.readFile (trajectoryPath t)
      map (get "iteration" . parseLine) ls `shouldBe` [Nothing, Just (PInt 1), Just (PInt 2), Just (PInt 3)]

  it "leaves a valid prefix after each call" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg
      first <- BS.readFile (trajectoryPath t)
      (get "type" . parseLine . head . BS.split 10) first `shouldBe` Just (PStr "metadata")
      logIteration t record
      second <- BS.readFile (trajectoryPath t)
      first `shouldSatisfy` (`BS.isPrefixOf` second)

  it "gives null other_backends without a sub model" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg {rcSubModel = Nothing}
      meta <- parseLine . head . BS.split 10 <$> BS.readFile (trajectoryPath t)
      get "other_backends" meta `shouldBe` Just PNull

  it "names the model each call was served by, an override included" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg
      let calls = [SubCall CallLlm "a" "A" 1 "x/override", SubCall CallLlm "b" "B" 1 "test/sub", SubCall CallRlm "c" "C" 1 "y/kid"]
      logIteration t record {irBlocks = [("pass", BlockResult "" "" [] [] Nothing, calls)]}
      ls <- BS.split 10 <$> BS.readFile (trajectoryPath t)
      let models = case get "code_blocks" (parseLine (ls !! 1)) of
            Just (PList (b : _)) ->
              [ m
              | Just r <- [get "result" b]
              , Just (PList cs) <- [get "rlm_calls" r]
              , c <- cs
              , Just m <- [get "root_model" c]
              ]
            _ -> []
      models `shouldBe` [PStr "x/override", PStr "test/sub", PStr "y/kid"]

  it "creates the directory and names the file rlm_<date>_<time>_<8 hex>.jsonl" $
    withSystemTempDirectory "rlmlog" $ \base -> do
      let dir = base </> "a" </> "b"
      t <- openTrajectory dir cfg
      doesDirectoryExist dir >>= (`shouldBe` True)
      fs <- listDirectory dir
      map (dir </>) fs `shouldBe` [trajectoryPath t]
      let n = takeFileName (trajectoryPath t)
          body = take (length n - length (".jsonl" :: String)) n
      n `shouldSatisfy` ("rlm_" `isPrefixOf`)
      n `shouldSatisfy` (".jsonl" `isSuffixOf`)
      -- rlm_YYYY-MM-DD_HH-MM-SS_xxxxxxxx
      length body `shouldBe` 4 + 19 + 1 + 8
      let ds = take 19 (drop 4 body)
          hex = drop 24 body
      [i | (i, c) <- zip [0 :: Int ..] ds, c `elem` ['-', '_']] `shouldBe` [4, 7, 10, 13, 16]
      all (`elem` ("0123456789abcdef" :: String)) hex `shouldBe` True

  it "does not reuse a name between runs in the same second" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      ts <- mapM (const (openTrajectory dir cfg)) [1 .. 20 :: Int]
      length (sort (map trajectoryPath ts)) `shouldBe` 20
      fs <- listDirectory dir
      length fs `shouldBe` 20

  it "writes non-ASCII as \\u escapes, as json.dump does" $
    withSystemTempDirectory "rlmlog" $ \dir -> do
      t <- openTrajectory dir cfg
      logIteration t record
      raw <- BS.readFile (trajectoryPath t)
      BS.all (< 128) raw `shouldBe` True
      T.isInfixOf "\\u4e16" (TE.decodeUtf8 raw) `shouldBe` True
