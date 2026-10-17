-- | The trajectory log: a JSONL file per run, in the shape RLMLogger writes
-- (rlm/logger/rlm_logger.py, with to_dict from rlm/core/types.py; alexzhang13/rlm
-- at d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT; see NOTICE), so the
-- reference's visualizer can read it. One metadata line comes first, then one
-- line per iteration. Each line is appended and closed on its own, so a run
-- that dies leaves a valid prefix.
--
-- Not carried, by ruling: variable values (a block's @locals@ map each name to
-- its type name), per-block execution time (0.0) and per-call usage (empty).
module Fisher.RLM.Log
  ( Trajectory
  , openTrajectory
  , trajectoryPath
  , logIteration
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, finally, try)
import qualified Data.ByteString as BS
import Data.Bits (shiftL, (.|.))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (formatTime, defaultTimeLocale, getZonedTime, getCurrentTime, utctDayTime)
import Fisher.Py.Value (PyVal (..), dumpsAscii)
import Fisher.RLM.Config (RlmConfig (..))
import Fisher.RLM.Parse (BlockResult (..), Message)
import Fisher.RLM.Types (IterationRecord (..), SubCall (..))
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openBinaryFile)

data Trajectory = Trajectory
  { trPath :: FilePath
  , trCount :: MVar Int
  -- ^ iterations written so far; also serialises the appends
  }

-- | The file this run writes to.
trajectoryPath :: Trajectory -> FilePath
trajectoryPath = trPath

-- | Create the directory if needed, choose @rlm_<date>_<time>_<8 hex>.jsonl@
-- in it and write the metadata line.
openTrajectory :: FilePath -> RlmConfig -> IO Trajectory
openTrajectory dir cfg = do
  createDirectoryIfMissing True dir
  now <- getZonedTime
  suffix <- randomHex8
  let stamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
      path = dir </> ("rlm_" <> stamp <> "_" <> suffix <> ".jsonl")
  count <- newMVar 0
  let t = Trajectory path count
  withMVar count $ \_ -> do
    ts <- timestamp
    appendLine path (PDict (("type", PStr "metadata") : ("timestamp", PStr ts) : metadataFields cfg))
  pure t

-- | Append one iteration line.
logIteration :: Trajectory -> IterationRecord -> IO ()
logIteration t r = modifyMVar_ (trCount t) $ \n -> do
  ts <- timestamp
  let entry =
        PDict
          ( [ ("type", PStr "iteration")
            , ("iteration", PInt (fromIntegral (n + 1)))
            , ("timestamp", PStr ts)
            ]
              ++ iterationFields r
          )
  appendLine (trPath t) entry
  pure (n + 1)

appendLine :: FilePath -> PyVal -> IO ()
appendLine path v = do
  h <- openBinaryFile path AppendMode
  BS.hPut h (TE.encodeUtf8 (dumpsAscii v <> "\n")) `finally` hClose h

-- RLMMetadata.to_dict
metadataFields :: RlmConfig -> [(Text, PyVal)]
metadataFields cfg =
  [ ("root_model", PStr (rcModel cfg))
  , ("max_depth", PInt (fromIntegral (rcMaxDepth cfg)))
  , ("max_iterations", PInt (fromIntegral (rcMaxIterations cfg)))
  , ("backend", PStr "openrouter")
  , ("backend_kwargs", PDict [("model_name", PStr (rcModel cfg))])
  , ("environment_type", PStr "local")
  , ("environment_kwargs", PDict [])
  , ("other_backends", maybe PNull (const (PList [PStr "openrouter"])) (rcSubModel cfg))
  ]

-- RLMIteration.to_dict
iterationFields :: IterationRecord -> [(Text, PyVal)]
iterationFields r =
  [ ("prompt", PList (map messageVal (irPrompt r)))
  , ("response", PStr (irResponse r))
  , ("code_blocks", PList (map codeBlock (irBlocks r)))
  , ("final_answer", maybe PNull PStr (irFinal r))
  , ("iteration_time", PFloat (irSeconds r))
  ]

messageVal :: Message -> PyVal
messageVal (role, content) = PDict [("role", PStr role), ("content", PStr content)]

-- CodeBlock.to_dict and REPLResult.to_dict
codeBlock :: (Text, BlockResult, [SubCall]) -> PyVal
codeBlock (code, br, calls) =
  PDict
    [ ("code", PStr code)
    ,
      ( "result"
      , PDict
          [ ("stdout", PStr (brStdout br))
          , ("stderr", PStr (brStderr br))
          , ("locals", PDict [(n, PStr ty) | (n, ty) <- zip (brVars br) (brTypes br)])
          , ("execution_time", PFloat 0)
          , ("rlm_calls", PList (map subCall calls))
          , ("final_answer", maybe PNull PStr (brFinal br))
          ]
      )
    ]

-- RLMChatCompletion.to_dict
subCall :: SubCall -> PyVal
subCall c =
  PDict
    [ ("root_model", PStr (scModel c))
    , ("prompt", PStr (scPrompt c))
    , ("response", PStr (scResponse c))
    , ("usage_summary", PDict [("model_usage_summaries", PDict [])])
    , ("execution_time", PFloat 0)
    ]

-- | Python's @datetime.now().isoformat()@: local time, microseconds unless
-- they are zero.
timestamp :: IO Text
timestamp = do
  now <- getZonedTime
  let base = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      micros = take 6 (formatTime defaultTimeLocale "%q" now)
  pure (T.pack (if micros == "000000" then base else base <> "." <> micros))

-- | Eight hex digits from the system's random source, or from the clock
-- if that cannot be read.
randomHex8 :: IO String
randomHex8 = do
  r <- try (do h <- openBinaryFile "/dev/urandom" ReadMode; b <- BS.hGet h 4; hClose h; pure b)
  w <- case r :: Either SomeException BS.ByteString of
    Right b | BS.length b == 4 -> pure (BS.foldl' (\a x -> (a `shiftL` 8) .|. fromIntegral x) (0 :: Word) b)
    _ -> do
      c <- getCurrentTime
      pure (fromIntegral (floor (realToFrac (utctDayTime c) * (1e9 :: Double)) :: Integer))
  let s = showHex (w `mod` 0x100000000) ""
  pure (replicate (8 - length s) '0' ++ s)
