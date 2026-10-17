-- | @GET /api/context/:id@: what the session's context strategy kept.
--
-- CLM and RLM: the ctx dir (@context.txt@ now, @context.prev.txt@ before
-- the last edit, @state.json@ or @rlm.json@) and the log's @clm_*@ and
-- @rlm_*@ events. The compressor: the summary message that opens each
-- session of the compression chain.
module Fisher.UI.Api.Context
  ( contextApi
  , summaryPrefixes
  ) where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Types (badRequest400, notFound404, ok200)
import Network.Wai (Request (..))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Fisher.UI.Api.Sessions (SessionInfo (..), chainOf, sessionInfo)
import Fisher.UI.Db
import Fisher.UI.Response (Api, jsonResponse)

-- | How a compaction summary begins (and the older form).
summaryPrefixes :: [Text]
summaryPrefixes = ["[CONTEXT COMPACTION]", "[CONTEXT SUMMARY]:"]

contextApi :: UiEnv -> Api
contextApi env r = case pathInfo r of
  ["api", "context", sid]
    | not (safeId sid) -> pure (Just (jsonResponse badRequest400 (err "bad session id")))
    | otherwise -> Just <$> context sid
  _ -> pure Nothing
  where
    err :: Text -> A.Value
    err m = A.object ["error" A..= m]
    context sid = do
      here <- doesFileExist (stateDb env)
      found <-
        if not here
          then pure Nothing
          else withRo (stateDb env) $ \c -> do
            rows <- allSessions c
            case [x | x <- rows, srId x == sid] of
              [] -> pure Nothing
              (row : _) -> do
                let chain = chainOf rows sid
                sums <- concat <$> mapM (\s -> map ((,) s) . summaries <$> sessionMessages c s) chain
                pure (Just (rows, row, chain, sums))
      case found of
        Nothing -> pure (jsonResponse notFound404 (err "no such session"))
        Just (rows, row, chain, sums) -> do
          si <- sessionInfo env rows row
          s <- maybe (pure emptySummary) (logSummary (ueLogs env)) (siLog si)
          dir <- ctxFor (sid : take 1 chain)
          now <- maybe (pure Nothing) (\d -> readText (d </> "context.txt")) dir
          prev <- maybe (pure Nothing) (\d -> readText (d </> "context.prev.txt")) dir
          st <- maybe (pure Nothing) (\d -> firstJson [d </> "state.json", d </> "rlm.json"]) dir
          pure . jsonResponse ok200 $
            A.object
              [ "strategy" A..= siStrategy si, "now" A..= now, "prev" A..= prev, "state" A..= st
              , "events" A..= lsEvents s
              , "summaries" A..= [A.object ["session" A..= k, "text" A..= t] | (k, t) <- sums] ]
    summaries ms = [t | m <- ms, Just t <- [mrContent m], any (`T.isPrefixOf` t) summaryPrefixes]
    -- ids from the database rows are checked as the request's id is
    ctxFor = \case
      [] -> pure Nothing
      (s : ss) | not (safeId s) -> ctxFor ss
      (s : ss) -> do
        let d = ctxDirOf (ueHome env) s
        yes <- doesDirectoryExist d
        if yes then pure (Just d) else ctxFor ss

readText :: FilePath -> IO (Maybe Text)
readText p = either (\(_ :: IOException) -> Nothing) (Just . TE.decodeUtf8With TE.lenientDecode) <$> try (B.readFile p)

firstJson :: [FilePath] -> IO (Maybe A.Value)
firstJson = \case
  [] -> pure Nothing
  (p : ps) -> do
    b <- either (\(_ :: IOException) -> Nothing) Just <$> try (B.readFile p)
    case b >>= A.decodeStrict of
      Just v -> pure (Just v)
      Nothing -> firstJson ps
