-- | Server-sent events, as the OpenAI SDK reads them: @event:@ and @data:@
-- fields, a blank line ends an event, @:@ lines are comments, and
-- @data: [DONE]@ ends the stream.
module Fisher.LLM.Sse
  ( SseEvent (..)
  , SseParser
  , newParser
  , feed
  , finish
  , parseAll
  , eventJson
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Fisher.Py.Value (PyVal, loads)

data SseEvent = SseEvent
  { seEvent :: Maybe Text
  , seData :: Text
  }
  deriving (Eq, Show)

-- | Bytes not yet ending in a newline, the event being built, and whether
-- @[DONE]@ was seen.
data SseParser = SseParser
  { spBuffer :: BS.ByteString
  , spEvent :: Maybe Text
  , spData :: [Text]
  , spDone :: Bool
  }

newParser :: SseParser
newParser = SseParser BS.empty Nothing [] False

-- | Feed a chunk; the events it completed, in order.
feed :: SseParser -> BS.ByteString -> (SseParser, [SseEvent])
feed p chunk
  | spDone p = (p, [])
  | otherwise = go p {spBuffer = spBuffer p <> chunk} []
  where
    go st acc = case B8.elemIndex '\n' (spBuffer st) of
      Nothing -> (st, reverse acc)
      Just i ->
        let raw = BS.take i (spBuffer st)
            line = if not (BS.null raw) && B8.last raw == '\r' then BS.init raw else raw
            st' = st {spBuffer = BS.drop (i + 1) (spBuffer st)}
         in case lineStep st' (TE.decodeUtf8With TEE.lenientDecode line) of
              (st'', Just ev)
                | seData ev == "[DONE]" -> (st'' {spDone = True, spBuffer = BS.empty}, reverse acc)
                | otherwise -> go st'' (ev : acc)
              (st'', Nothing) -> go st'' acc

lineStep :: SseParser -> Text -> (SseParser, Maybe SseEvent)
lineStep st line
  | T.null line =
      if null (spData st) && spEvent st == Nothing
        then (st, Nothing)
        else (st {spEvent = Nothing, spData = []}, Just (SseEvent (spEvent st) (T.intercalate "\n" (reverse (spData st)))))
  | ":" `T.isPrefixOf` line = (st, Nothing)
  | otherwise =
      let (field, rest) = T.breakOn ":" line
          value = let v = T.drop 1 rest in if " " `T.isPrefixOf` v then T.drop 1 v else v
       in case field of
            "event" -> (st {spEvent = Just value}, Nothing)
            "data" -> (st {spData = value : spData st}, Nothing)
            _ -> (st, Nothing)

-- | End of input: a pending event is dispatched.
finish :: SseParser -> [SseEvent]
finish p
  | spDone p = []
  | otherwise =
      let (p1, evs) = feed p "\n"
       in evs <> maybe [] pure (if spDone p1 then Nothing else snd (lineStep p1 ""))

parseAll :: BS.ByteString -> [SseEvent]
parseAll bs = let (p, evs) = feed newParser bs in evs <> finish p

eventJson :: SseEvent -> Either String PyVal
eventJson = loads . TE.encodeUtf8 . seData
