-- | Properties of the RLM offload cut.
module Fisher.Agent.RlmPropSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Fisher.Agent.Context.Rlm
import Fisher.Py.Value

-- | A history of user, assistant and assistant-with-calls exchanges.
genHistory :: Gen [PyVal]
genHistory = do
  k <- choose (0 :: Int, 30)
  go (0 :: Int) k
  where
    go _ 0 = pure []
    go c k = do
      kind <- choose (0 :: Int, 3)
      case kind of
        0 -> (:) (msg "user" c) <$> go (c + 1) (k - 1)
        1 -> (:) (msg "assistant" c) <$> go (c + 1) (k - 1)
        _ -> do
          n <- choose (1, 3)
          let ids = [cid c j | j <- [1 .. n]]
              call = obj [ ("role", PStr "assistant"), ("content", PStr "c")
                         , ("tool_calls", PList [obj [("id", PStr i), ("type", PStr "function"), ("function", obj [("name", PStr "t"), ("arguments", PStr "{}")])] | i <- ids]) ]
              rs = [obj [("role", PStr "tool"), ("tool_call_id", PStr i), ("content", PStr ("r" <> i))] | i <- ids]
          rest <- go (c + 1) (k - 1)
          pure (call : rs <> rest)
    msg r c = obj [("role", PStr r), ("content", PStr (r <> T.pack (show c)))]
    cid :: Int -> Int -> Text
    cid c j = "id" <> T.pack (show c) <> "_" <> T.pack (show j)

isToolMsg :: PyVal -> Bool
isToolMsg m = get "role" m == Just (PStr "tool")

callIds :: PyVal -> [Text]
callIds m = [pyStrOf i | c <- maybe [] (maybe [] id . asList) (get "tool_calls" m), Just i <- [get "id" c]]

-- | Every tool result follows an assistant call with its id, contiguously.
wellFormed :: [PyVal] -> Bool
wellFormed = go []
  where
    go _ [] = True
    go open (m : ms)
      | isToolMsg m = maybe False (\i -> pyStrOf i `elem` open) (get "tool_call_id" m) && go open ms
      | otherwise = go (callIds m) ms

spec :: Spec
spec = describe "offload properties" $ do
  let run = withMaxSuccess 5000
  it "keeps every message in order, in the head, the store or the tail" $ run $
    forAll genHistory $ \h -> case offload 3 4 emptyRlmState h of
      Nothing -> True
      Just o ->
        let (hd, rest) = break isStub (ofMessages o)
         in hd <> map stMessage (rsStore (ofState o)) <> drop 1 rest == h
  it "leaves every tool result after its call" $ run $
    forAll genHistory $ \h -> case offload 3 4 emptyRlmState h of
      Nothing -> True
      Just o -> wellFormed (ofMessages o)
  it "leaves at most one stub, also after a second offload" $ run $
    forAll genHistory $ \h -> forAll genHistory $ \extra -> case offload 3 4 emptyRlmState h of
      Nothing -> True
      Just o -> case offload 3 4 (ofState o) (ofMessages o <> extra) of
        Nothing -> True
        Just o2 -> length (filter isStub (ofMessages o2)) == 1 && wellFormed (ofMessages o2)
  it "keeps the first 3 and last 4 unless a snap moved the boundary outward" $ run $
    forAll genHistory $ \h -> case offload 3 4 emptyRlmState h of
      Nothing -> True
      Just o ->
        let ms = ofMessages o
         in take 3 ms == take 3 h && drop (length ms - 4) ms == drop (length h - 4) h
  it "numbers the store by session index across two offloads: contiguous from the head's end, each turn the original message" $ run $
    forAll genHistory $ \h -> forAll genHistory $ \extra -> case offload 3 4 emptyRlmState h of
      Nothing -> True
      Just o ->
        let full = h <> extra
            first = length (takeWhile (not . isStub) (ofMessages o)) + 1
            check st =
              let is = map stIndex (rsStore st)
               in is == [first .. first + length is - 1]
                    && first >= 4
                    && and [i <= length full && full !! (i - 1) == stMessage t | t <- rsStore st, let i = stIndex t]
         in check (ofState o) && case offload 3 4 (ofState o) (ofMessages o <> extra) of
              Nothing -> True
              Just o2 -> check (ofState o2)
