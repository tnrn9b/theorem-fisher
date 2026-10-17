-- | Event parsing, Claude launch normalisation and elapsed formatting
-- against the pinned upstream.
module Fisher.SwarmGoldenSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), eitherDecodeFileStrict, encode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Golden
import Fisher.Swarm.Event
import Fisher.Swarm.Launch
import Fisher.Swarm.Render (formatElapsed)

golden :: FilePath -> FilePath
golden n = "test/golden/a2" </> n

stateOf :: Progress -> Value
stateOf p = Object . KM.fromList $
  [ ("progress", String (pProgress p)), ("result", maybe Null String (pResult p))
  , ("error", maybe Null String (pError p)), ("lean_status", maybe Null String (pLean p)) ]

spec :: Spec
spec = do
  describe "stream-events.json" $ do
    cases <- runIO (goldenCases (golden "stream-events.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ do
      let lines' = field "lines" c :: [Text]
          states = tail (scanl (flip parseStreamEvent) initialProgress lines')
          expected = field "states" c :: [Value]
      case expected of
        [Object o] | KM.member "raised" o ->
          -- Upstream raises here and fails the session; Fisher skips the line.
          states `shouldBe` [initialProgress]
        _ -> map stateOf states `shouldBe` expected

  describe "claude-launch.json" $ do
    whole <- runIO (either fail pure =<< (eitherDecodeFileStrict (golden "claude-launch.json") :: IO (Either String Value)))
    cases <- runIO (goldenCases (golden "claude-launch.json"))
    let isRoot = field "is_root" (field "machine" whole :: Value) :: Bool
    forM_ [c | c <- cases, textField "name" c /= "trust-file"] $ \c ->
      it (T.unpack (textField "name" c)) $ withSystemTempDirectory "fisher-swarm" $ \home -> do
        (argv, env) <- normalizeClaudeLaunch isRoot home (field "argv" c) (field "env" c) Nothing
        argv `shouldBe` (field "out_argv" c :: [Text])
        env `shouldBe` (field "out_env" c :: Map.Map String String)
    forM_ [c | c <- cases, textField "name" c == "trust-file"] $ \c ->
      it "trust-file" $ withSystemTempDirectory "fisher-swarm" $ \root0 -> do
        root <- canonicalizePath root0
        let home = root </> "home"
            proj = root </> "proj"
        mapM_ (createDirectoryIfMissing True) [home, proj]
        BL.writeFile (home </> ".claude.json") $ encode $ Object $ KM.fromList
          [ ("projects", Object (KM.fromList [(fromString' proj, Object (KM.fromList [("allowedTools", Array (pure (String "x")))]))]))
          , ("k", Number 1) ]
        _ <- normalizeClaudeLaunch isRoot home ["claude"] Map.empty (Just proj)
        _ <- normalizeClaudeLaunch isRoot home ["claude"] Map.empty (Just (root </> "fresh"))
        written <- either fail pure =<< (eitherDecodeFileStrict (home </> ".claude.json") :: IO (Either String Value))
        rekey root (untmp root written) `shouldBe` (field "claude_json" c :: Value)

  describe "elapsed.json" $ do
    cases <- runIO (goldenCases (golden "elapsed.json"))
    forM_ cases $ \c -> it (show (field "seconds" c :: Double)) $
      formatElapsed (field "seconds" c) `shouldBe` textField "text" c
  where
    fromString' = read . show
    rekey root = \case
      Object o -> Object (KM.fromList [(K.fromText (T.replace (T.pack root) "<TMP>" (K.toText k)), rekey root v) | (k, v) <- KM.toList o])
      Array a -> Array (fmap (rekey root) a)
      v -> v
