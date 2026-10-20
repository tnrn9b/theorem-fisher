module Fisher.TemplateSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.List (nub)
import Control.Monad (forM, forM_)
import Data.Aeson (Value (..), eitherDecodeFileStrict, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime, getCurrentTime)
import qualified Data.Vector as V
import System.Directory
  ( createDirectoryIfMissing, createDirectoryLink, createFileLink, doesDirectoryExist, doesFileExist, listDirectory
  , setModificationTime, withCurrentDirectory )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process.Typed (proc, readProcessStdout_, runProcess_, setWorkingDir)
import Test.Hspec

import Fisher.Project (renderManifest)
import Fisher.Prover (ProverKind (..))
import Fisher.Template
import Fisher.TestUtil (withTmp)

-- | HOME and FISHER_HOME pointed into a scratch directory for the action.
withHomes :: (FilePath -> IO a) -> IO a
withHomes k = withTmp $ \d -> withEnvs [("HOME", d), ("FISHER_HOME", d </> "fh")] (k d)

withEnvs :: [(String, String)] -> IO a -> IO a
withEnvs kvs act = bracket save restore (const (mapM_ (uncurry setEnv) kvs >> act))
  where
    save = mapM (\(k, _) -> (,) k <$> lookupEnv k) kvs
    restore = mapM_ (\(k, v) -> maybe (unsetEnv k) (setEnv k) v)

-- | A template directory: the manifest lines and the files.
writeTemplate :: FilePath -> [String] -> [(FilePath, String)] -> IO ()
writeTemplate dir manifest files = do
  createDirectoryIfMissing True dir
  BS.writeFile (dir </> manifestName) (TE.encodeUtf8 (T.pack (unlines manifest)))
  forM_ files $ \(p, c) -> do
    createDirectoryIfMissing True (takeDirectory (dir </> p))
    BS.writeFile (dir </> p) (TE.encodeUtf8 (T.pack c))

-- | A manifest as its top-level fields, each with its lines, so a test can
-- replace one.
type Fields = [(String, [String])]

simpleFields :: String -> Fields
simpleFields name =
  [ ("name", ["name: " <> name]), ("prover", ["prover: lean4"]), ("description", ["description: A test template"])
  , ("files", ["files:", "  - Main.lean"]), ("steps", ["steps:", "  - name: build", "    run: [lake, build]"])
  , ("sample", ["sample: Main.lean"]) ]

with :: String -> [String] -> Fields -> Fields
with k ls fs = if any ((== k) . fst) fs then [(k', if k' == k then ls else v) | (k', v) <- fs] else fs <> [(k, ls)]

without :: String -> Fields -> Fields
without k = filter ((/= k) . fst)

manifestOf :: Fields -> [String]
manifestOf = concatMap snd

simpleManifest :: String -> [String]
simpleManifest = manifestOf . simpleFields

filesWith :: [String] -> Fields
filesWith ps = with "files" ("files:" : ["  - '" <> p <> "'" | p <- ps]) (simpleFields "t")

parseDir :: FilePath -> IO (Either Text Template)
parseDir dir = BS.readFile (dir </> manifestName) >>= parseTemplate dir

expectLeft :: Either Text a -> [Text] -> Expectation
expectLeft r needles = case r of
  Right _ -> expectationFailure ("expected a refusal mentioning " <> show needles)
  Left m -> forM_ needles $ \n -> m `shouldSatisfy` T.isInfixOf n

expectRight :: Either Text a -> IO a
expectRight = either (\m -> expectationFailure (T.unpack m) >> fail "Left") pure

builtin :: Text -> IO Template
builtin n = withHomes (\_ -> findTemplate (object []) n) >>= expectRight

noResolver :: Text -> IO (Either Text Text)
noResolver w = fail ("the resolver was called for " <> T.unpack w)

git :: FilePath -> [String] -> IO ()
git dir args = runProcess_ (setWorkingDir dir (proc "git" (identity <> args)))

gitOut :: FilePath -> [String] -> IO Text
gitOut dir args = T.strip . T.pack . BL8.unpack <$> readProcessStdout_ (setWorkingDir dir (proc "git" (identity <> args)))

identity :: [String]
identity = ["-c", "user.name=t", "-c", "user.email=t@example.com", "-c", "init.defaultBranch=main", "-c", "commit.gpgsign=false"]

-- | A bare repository at @<d>/remote.git@ holding a template named @remote@,
-- and the working copy it was pushed from at @<d>/work@.
makeRemote :: FilePath -> IO FilePath
makeRemote d = do
  let work = d </> "work"
      bare = d </> "remote.git"
  writeTemplate work (simpleManifest "remote") [("Main.lean", "theorem t : True := trivial\n")]
  git work ["init", "-q"]
  git work ["add", "-A"]
  git work ["commit", "-q", "-m", "one"]
  git d ["clone", "-q", "--bare", work, bare]
  git work ["remote", "add", "origin", bare]
  pure bare

templatesConfig :: [(Text, Text)] -> Value
templatesConfig kvs = object ["fisher" .= object ["templates" .= object [K.fromText k .= v | (k, v) <- kvs]]]

spec :: Spec
spec = do
  describe "the built-ins" $ do
    it "parses lean with the values in the rulings" $ do
      t <- builtin "lean"
      tplName t `shouldBe` "lean"
      tplProver t `shouldBe` Lean4
      tplDescription t `shouldBe` "A plain Lean 4 package with its toolchain pinned"
      map varName (tplVariables t) `shouldBe` ["project", "lean"]
      map varDefault (tplVariables t) `shouldBe` ["", "latest"]
      map varPattern (tplVariables t) `shouldBe` ["[A-Za-z][A-Za-z0-9_]*", "latest|v4\\.[0-9]+\\.[0-9]+(-rc[0-9]+)?"]
      tplFiles t `shouldBe` ["lakefile.toml", "lean-toolchain", "{{project}}.lean", "{{project}}/Sample.lean"]
      tplSteps t `shouldBe` [Step "build" ["lake", "build"] 30]
      tplSample t `shouldBe` "{{project}}/Sample.lean"
      tplOrigin t `shouldBe` BuiltIn
    it "parses lean-mathlib with the values in the rulings" $ do
      t <- builtin "lean-mathlib"
      tplName t `shouldBe` "lean-mathlib"
      tplProver t `shouldBe` Lean4
      tplDescription t `shouldBe` "A Lean 4 package with Mathlib, its cache fetched"
      map varName (tplVariables t) `shouldBe` ["project", "mathlib"]
      map varDefault (tplVariables t) `shouldBe` ["", "latest"]
      tplFiles t `shouldBe` ["lakefile.toml", "lean-toolchain", "{{project}}.lean", "{{project}}/Sample.lean"]
      tplSteps t `shouldBe` [Step "cache" ["lake", "exe", "cache", "get"] 30, Step "build" ["lake", "build"] 30]
      tplSample t `shouldBe` "{{project}}/Sample.lean"
    it "materialises the built-ins under FISHER_HOME, in a directory named by their hash" $ withHomes $ \d -> do
      t <- findTemplate (object []) "lean-mathlib" >>= expectRight
      takeDirectory (tplRoot t) `shouldBe` d </> "fh" </> "fisher" </> "templates-builtin"
      let leaf = takeFileName (tplRoot t)
      take (length ("lean-mathlib-" :: String)) leaf `shouldBe` "lean-mathlib-"
      length leaf `shouldBe` length ("lean-mathlib-" :: String) + 8
      doesDirectoryExist (tplRoot t) `shouldReturn` True
      doesFileExist (tplRoot t </> "{{project}}" </> "Sample.lean") `shouldReturn` True
      doesFileExist (tplRoot t </> manifestName) `shouldReturn` True
    it "never writes into a directory that is already there, and leaves no temporary one" $ withHomes $ \_ -> do
      t <- findTemplate (object []) "lean" >>= expectRight
      writeFile (tplRoot t </> "marker.txt") "mine\n"
      t' <- findTemplate (object []) "lean" >>= expectRight
      tplRoot t' `shouldBe` tplRoot t
      readFile (tplRoot t </> "marker.txt") `shouldReturn` "mine\n"
      entries <- listDirectory (takeDirectory (tplRoot t))
      filter ((== ".") . take 1) entries `shouldBe` []
    it "lets several threads materialise at once" $ withHomes $ \_ -> do
      dones <- forM [1 .. 8 :: Int] $ \_ -> do
        v <- newEmptyMVar
        _ <- forkIO (try builtinTemplates >>= putMVar v)
        pure v
      results <- mapM takeMVar dones
      forM_ results $ \case
        Left e -> expectationFailure (show (e :: SomeException))
        Right ts -> map tplName ts `shouldBe` builtinNames
      let roots = [map tplRoot ts | Right ts <- results]
      length (nub roots) `shouldBe` 1
      entries <- listDirectory (takeDirectory (concat (take 1 (concat roots))))
      length entries `shouldBe` 2
    forM_ ["lean", "lean-mathlib"] $ \n ->
      it ("renders " <> T.unpack n <> " as its golden") $ do
        Right golden <- eitherDecodeFileStrict ("test/golden/templates/" <> T.unpack n <> ".json") :: IO (Either String Value)
        let vars = [("project", "Demo"), ("lean", "v4.24.0"), ("mathlib", "v4.24.0")]
        files <- withHomes (\_ -> findTemplate (object []) n >>= expectRight >>= \t -> renderFiles t vars) >>= expectRight
        let want = case golden of
              Object o | Just (Array fs) <- KM.lookup "files" o ->
                [ (T.unpack p, TE.encodeUtf8 c)
                | Object f <- V.toList fs, Just (String p) <- [KM.lookup "path" f], Just (String c) <- [KM.lookup "contents" f] ]
              _ -> []
        want `shouldNotBe` []
        files `shouldBe` want
    it "gives a 64-digit revision that does not change between calls" $ do
      t <- builtin "lean"
      r1 <- templateRevision t
      r2 <- templateRevision t
      T.length r1 `shouldBe` 64
      r1 `shouldBe` r2

  describe "parseTemplate" $ do
    it "refuses a manifest without prover" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (without "prover" (simpleFields "t"))) [("Main.lean", "x\n")]
      r <- parseDir d
      expectLeft r ["prover", T.pack manifestName]
    it "refuses an unknown prover" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (with "prover" ["prover: coq"] (simpleFields "t"))) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["coq"]
    forM_ ["rocq", "isabelle"] $ \p ->
      it ("refuses " <> p <> " as not supported yet") $ withTmp $ \d -> do
        writeTemplate d (manifestOf (with "prover" ["prover: " <> p] (simpleFields "t"))) [("Main.lean", "x\n")]
        parseDir d >>= flip expectLeft [T.pack p, "not supported yet"]
    it "refuses a {{name}} with no variable in a file" $ withTmp $ \d -> do
      writeTemplate d (simpleManifest "t") [("Main.lean", "-- {{nope}}\n")]
      parseDir d >>= flip expectLeft ["{{nope}}", "Main.lean"]
    it "refuses a {{name}} with no variable in a step" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (with "steps" ["steps:", "  - name: two", "    run: [echo, '{{other}}']"] (simpleFields "t"))) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["{{other}}"]
    it "refuses a listed file that does not exist" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (filesWith ["Main.lean", "Missing.lean"])) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["Missing.lean"]
    it "refuses a glob that matches nothing" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (filesWith ["Main.lean", "src/**/*.lean"])) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["src/**/*.lean"]
    it "refuses a listed path that leaves the template" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (filesWith ["Main.lean", "../x"])) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["../x"]
    it "refuses a listed file that is a symbolic link" $ withTmp $ \d -> do
      writeFile (d </> "secret") "outside\n"
      writeTemplate (d </> "t") (manifestOf (filesWith ["Main.lean", "Link.lean"])) [("Main.lean", "x\n")]
      createFileLink (d </> "secret") (d </> "t" </> "Link.lean")
      parseDir (d </> "t") >>= flip expectLeft ["Link.lean", "symbolic link"]
    it "refuses a file reached through a linked directory" $ withTmp $ \d -> do
      createDirectoryIfMissing True (d </> "out")
      writeFile (d </> "out" </> "A.lean") "x\n"
      writeTemplate (d </> "t") (manifestOf (filesWith ["Main.lean", "sub/A.lean"])) [("Main.lean", "x\n")]
      createDirectoryLink (d </> "out") (d </> "t" </> "sub")
      parseDir (d </> "t") >>= flip expectLeft ["sub", "symbolic link"]
    it "refuses a link met by a glob, and a link loop does not recurse" $ withTmp $ \d -> do
      writeTemplate (d </> "t") (manifestOf (filesWith ["Main.lean", "**/*.lean"])) [("Main.lean", "x\n"), ("src/A.lean", "a\n")]
      createDirectoryLink (d </> "t") (d </> "t" </> "src" </> "loop")
      parseDir (d </> "t") >>= flip expectLeft ["src/loop", "symbolic link"]
    it "refuses a sample that is not one of the files" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (with "sample" ["sample: Other.lean"] (simpleFields "t"))) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["sample", "Other.lean"]
    it "refuses a bad variable pattern" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (with "variables" ["variables:", "  - name: v", "    pattern: '(unclosed'"] (simpleFields "t"))) [("Main.lean", "x\n")]
      parseDir d >>= flip expectLeft ["pattern", "v"]
    it "expands globs in order, without duplicates, and defaults the timeout to 30" $ withTmp $ \d -> do
      writeTemplate d
        [ "name: g", "prover: lean4", "description: d", "files:", "  - Main.lean", "  - 'src/**/*.lean'", "  - src/A.lean"
        , "steps:", "  - name: s", "    run: [true]", "sample: Main.lean" ]
        [("Main.lean", "x\n"), ("src/A.lean", "a\n"), ("src/deep/B.lean", "b\n"), ("src/C.txt", "c\n")]
      t <- parseDir d >>= expectRight
      tplFiles t `shouldBe` ["Main.lean", "src/A.lean", "src/deep/B.lean"]
      tplSteps t `shouldBe` [Step "s" ["true"] defaultTimeoutMinutes]
      defaultTimeoutMinutes `shouldBe` 30

  describe "resolveVariables" $ do
    it "gives project the target's base name" $ do
      t <- builtin "lean"
      r <- resolveVariables t "/work/proofs/Demo" [("lean", "v4.9.0")] noResolver >>= expectRight
      lookup "project" r `shouldBe` Just "Demo"
      lookup "lean" r `shouldBe` Just "v4.9.0"
    it "takes the project name from an absolute target when given ." $ withTmp $ \d -> do
      t <- builtin "lean"
      createDirectoryIfMissing True (d </> "Demo" </> "sub")
      r1 <- withCurrentDirectory (d </> "Demo") (resolveVariables t "." [("lean", "v4.9.0")] noResolver) >>= expectRight
      lookup "project" r1 `shouldBe` Just "Demo"
      r2 <- withCurrentDirectory (d </> "Demo" </> "sub") (resolveVariables t ".." [("lean", "v4.9.0")] noResolver) >>= expectRight
      lookup "project" r2 `shouldBe` Just "Demo"
    it "refuses a project value that does not match its pattern" $ do
      t <- builtin "lean"
      r <- resolveVariables t "/work/Demo" [("project", "1abc"), ("lean", "v4.9.0")] noResolver
      expectLeft r ["project", "1abc"]
    it "refuses a project name that clashes with a Lean or Mathlib package, ignoring case" $ do
      t <- builtin "lean-mathlib"
      r1 <- resolveVariables t "/work/mathlib" [("mathlib", "v4.24.0")] noResolver
      r1 `shouldBe` Left "project name mathlib clashes with a Lean or Mathlib package; pass --var project=NAME"
      r2 <- resolveVariables t "/work/Demo" [("project", "Batteries"), ("mathlib", "v4.24.0")] noResolver
      expectLeft r2 ["Batteries", "clashes", "--var project=NAME"]
      l <- builtin "lean"
      forM_ ["lean", "Lean", "Init", "Std", "Lake", "Aesop", "Qq", "ProofWidgets", "Plausible", "ImportGraph", "LeanSearchClient"] $ \n -> do
        r <- resolveVariables l "/work/Demo" [("project", n), ("lean", "v4.9.0")] noResolver
        expectLeft r [n, "clashes"]
      r3 <- resolveVariables l "/work/demo-lean" [("project", "LeanDemo"), ("lean", "v4.9.0")] noResolver >>= expectRight
      lookup "project" r3 `shouldBe` Just "LeanDemo"
    it "refuses an unknown variable" $ do
      t <- builtin "lean"
      r <- resolveVariables t "/work/Demo" [("colour", "red")] noResolver
      expectLeft r ["colour"]
    it "calls the resolver only for latest, with the variable's name" $ do
      t <- builtin "lean-mathlib"
      calls <- newIORef []
      let resolver w = modifyIORef' calls (w :) >> pure (Right "v4.24.0")
      r1 <- resolveVariables t "/w/Demo" [("mathlib", "v4.20.0")] resolver >>= expectRight
      lookup "mathlib" r1 `shouldBe` Just "v4.20.0"
      readIORef calls `shouldReturn` []
      r2 <- resolveVariables t "/w/Demo" [] resolver >>= expectRight
      lookup "mathlib" r2 `shouldBe` Just "v4.24.0"
      readIORef calls `shouldReturn` ["mathlib"]
    it "names the --var to pass when the latest release cannot be resolved" $ do
      t <- builtin "lean-mathlib"
      r <- resolveVariables t "/w/Demo" [] (\_ -> pure (Left "no network"))
      r `shouldBe` Left "could not resolve the latest release; pass --var mathlib=v4.x"
    it "refuses a value that does not match before resolving anything" $ do
      t <- builtin "lean"
      r <- resolveVariables t "/w/Demo" [("lean", "v5")] noResolver
      expectLeft r ["lean", "v5"]

  describe "renderFiles" $ do
    it "replaces {{project}} in contents and paths" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (filesWith ["Main.lean", "{{project}}/A.lean"]))
        [("Main.lean", "x\n"), ("{{project}}/A.lean", "namespace {{project}}\n")]
      t <- parseDir d >>= expectRight
      renderFiles t [("project", "Demo")] `shouldReturn` Right [("Main.lean", "x\n"), ("Demo/A.lean", "namespace Demo\n")]
    forM_ [("..", "{{project}}/x", "../x"), ("/", "{{project}}x", "/x")] $ \(v, p, out) ->
      it ("refuses a file that renders to " <> out) $ withTmp $ \d -> do
        writeTemplate d (manifestOf (filesWith ["Main.lean", p]))
          [("Main.lean", "x\n"), (p, "x\n")]
        t <- parseDir d >>= expectRight
        r <- renderFiles t [("project", T.pack v)]
        expectLeft r [T.pack out]
    it "renders {{{{ and }}}} as literal double braces" $ withTmp $ \d -> do
      let lean = "example : ({{{{x}}}} : Set (Set Nat)) = {{{{x}}}} := rfl -- {{project}}\n"
      writeTemplate d (manifestOf (filesWith ["Main.lean"])) [("Main.lean", lean)]
      t <- parseDir d >>= expectRight
      renderFiles t [("project", "Demo")]
        `shouldReturn` Right [("Main.lean", "example : ({{x}} : Set (Set Nat)) = {{x}} := rfl -- Demo\n")]
      renderText [] "{{{{a}}}}" `shouldBe` Right "{{a}}"
    it "copies a file that is not UTF-8 as it is" $ withTmp $ \d -> do
      writeTemplate d (manifestOf (filesWith ["Main.lean", "blob.bin"])) [("Main.lean", "x\n")]
      BS.writeFile (d </> "blob.bin") (BS.pack [0xff, 0x7b, 0x7b, 0xfe])
      t <- parseDir d >>= expectRight
      r <- renderFiles t [("project", "Demo")] >>= expectRight
      lookup "blob.bin" r `shouldBe` Just (BS.pack [0xff, 0x7b, 0x7b, 0xfe])
    it "renders steps and the sample" $ do
      t <- builtin "lean"
      renderSample t [("project", "Demo")] `shouldBe` Right "Demo/Sample.lean"
      renderSteps t [("project", "Demo")] `shouldBe` Right [Step "build" ["lake", "build"] 30]

  describe "listTemplates" $ do
    it "lists the built-ins and a configured directory" $ withHomes $ \d -> do
      writeTemplate (d </> "mine") (simpleManifest "mine") [("Main.lean", "x\n")]
      ts <- listTemplates (templatesConfig [("mine", T.pack (d </> "mine"))])
      map tplName ts `shouldBe` ["lean", "lean-mathlib", "mine"]
      map tplOrigin ts `shouldBe` [BuiltIn, BuiltIn, Configured (T.pack (d </> "mine"))]
    it "lets a configured lean replace the built-in, with a note" $ withHomes $ \d -> do
      writeTemplate (d </> "mylean") (simpleManifest "lean") [("Main.lean", "x\n")]
      (ts, notes) <- listTemplatesWithNotes (templatesConfig [("lean", T.pack (d </> "mylean"))])
      map tplName ts `shouldBe` ["lean", "lean-mathlib"]
      map tplOrigin ts `shouldBe` [Configured (T.pack (d </> "mylean")), BuiltIn]
      notes `shouldSatisfy` any (T.isInfixOf "lean")
      t <- findTemplate (templatesConfig [("lean", T.pack (d </> "mylean"))]) "lean" >>= expectRight
      tplDescription t `shouldBe` "A test template"
    it "turns a configured template that cannot load into a note" $ withHomes $ \d -> do
      writeTemplate (d </> "linked") (manifestOf (filesWith ["Main.lean", "L.lean"])) [("Main.lean", "x\n")]
      createFileLink (d </> "nowhere") (d </> "linked" </> "L.lean")
      createDirectoryIfMissing True (d </> "empty")
      (ts, notes) <- listTemplatesWithNotes (templatesConfig [("linked", T.pack (d </> "linked")), ("empty", T.pack (d </> "empty"))])
      map tplName ts `shouldBe` ["lean", "lean-mathlib"]
      length notes `shouldBe` 2
      notes `shouldSatisfy` any (T.isInfixOf "symbolic link")
    it "uses the content hash for a local git template with changes" $ withHomes $ \d -> do
      writeTemplate (d </> "g") (simpleManifest "g") [("Main.lean", "x\n")]
      git (d </> "g") ["init", "-q"]
      git (d </> "g") ["add", "-A"]
      git (d </> "g") ["commit", "-q", "-m", "one"]
      let config = templatesConfig [("g", T.pack (d </> "g"))]
      t <- findTemplate config "g" >>= expectRight
      headSha <- gitOut (d </> "g") ["rev-parse", "HEAD"]
      templateRevision t `shouldReturn` headSha
      writeFile (d </> "g" </> "Main.lean") "changed\n"
      dirty <- templateRevision t
      T.length dirty `shouldBe` 64
      dirty `shouldNotBe` headSha
    it "names the known templates for an unknown one" $ withHomes $ \_ -> do
      r <- findTemplate (object []) "nope"
      expectLeft r ["nope", "lean", "lean-mathlib"]
    it "clones a git template into the cache and pulls it again only after a day" $ withHomes $ \d -> do
      bare <- makeRemote d
      let config = templatesConfig [("remote", T.pack bare)]
          clone = d </> ".cache" </> "fisher" </> "templates" </> "remote"
      t <- findTemplate config "remote" >>= expectRight
      tplRoot t `shouldBe` clone
      tplOrigin t `shouldBe` Configured (T.pack bare)
      tplDescription t `shouldBe` "A test template"
      head1 <- gitOut (d </> "work") ["rev-parse", "HEAD"]
      templateRevision t `shouldReturn` head1
      -- A new commit upstream.
      writeTemplate (d </> "work") (manifestOf (with "description" ["description: Changed"] (simpleFields "remote"))) []
      git (d </> "work") ["commit", "-q", "-am", "two"]
      git (d </> "work") ["push", "-q", "origin", "HEAD:main"]
      fresh <- findTemplate config "remote" >>= expectRight
      tplDescription fresh `shouldBe` "A test template"
      now <- getCurrentTime
      setModificationTime (clone </> cloneStamp) (addUTCTime (-2 * 86400) now)
      pulled <- findTemplate config "remote" >>= expectRight
      tplDescription pulled `shouldBe` "Changed"
      ts <- listTemplates config
      map tplName ts `shouldBe` ["lean", "lean-mathlib", "remote"]
    it "passes a URL that looks like an option to git as a URL" $ withHomes $ \d -> do
      r <- findTemplate (templatesConfig [("opt", "--upload-pack=touch " <> T.pack (d </> "pwned") <> ".git")]) "opt"
      expectLeft r ["Failed to clone template opt: "]
      doesFileExist (d </> "pwned") `shouldReturn` False
      doesFileExist (d </> "pwned.git") `shouldReturn` False
    it "reports a failing clone with git's message" $ withHomes $ \d -> do
      r <- findTemplate (templatesConfig [("bad", T.pack (d </> "missing.git"))]) "bad"
      case r of
        Left m -> do
          m `shouldSatisfy` T.isPrefixOf "Failed to clone template bad: "
          T.length m `shouldSatisfy` (> T.length "Failed to clone template bad: ")
          T.lines m `shouldBe` [m]
        Right _ -> expectationFailure "expected the clone to fail"
      (ts, notes) <- listTemplatesWithNotes (templatesConfig [("bad", T.pack (d </> "missing.git"))])
      map tplName ts `shouldBe` ["lean", "lean-mathlib"]
      notes `shouldSatisfy` any (T.isPrefixOf "Failed to clone template bad: ")

  describe "latestRelease" $ do
    it "picks the highest v4.N.M tag" $
      pickLatest (T.unlines
        [ "a\trefs/tags/v4.9.0", "b\trefs/tags/v4.10.0", "c\trefs/tags/v4.24.0", "d\trefs/tags/v4.25.0-rc1"
        , "e\trefs/tags/nightly-2026-10-01", "f\trefs/tags/v4.3.10" ])
        `shouldBe` Just "v4.24.0"
    it "reads the tags of a repository with git ls-remote" $ withTmp $ \d -> do
      let r = d </> "r"
      createDirectoryIfMissing True r
      git r ["init", "-q"]
      writeFile (r </> "f") "x\n"
      git r ["add", "-A"]
      git r ["commit", "-q", "-m", "one"]
      forM_ ["v4.9.0", "v4.10.0", "v4.24.0", "v4.25.0-rc1", "nightly"] $ \tag -> git r ["tag", tag]
      latestReleaseFrom r `shouldReturn` Right "v4.24.0"
    it "fails with a message when the repository cannot be read" $ withTmp $ \d -> do
      r <- latestReleaseFrom (d </> "missing")
      expectLeft r [""]
    it "knows the Lean and Mathlib repositories" $ do
      releaseRepository "lean" `shouldBe` Just "https://github.com/leanprover/lean4"
      releaseRepository "mathlib" `shouldBe` Just "https://github.com/leanprover-community/mathlib4"
      releaseRepository "other" `shouldBe` Nothing

  describe "the project manifest" $
    it "puts template right after source" $ do
      let bytes = renderManifest (object ["blueprint" .= object [], "template" .= object ["name" .= ("lean" :: Text)], "source" .= object [], "name" .= ("x" :: Text)])
          keys = [BC.takeWhile (/= ':') l | l <- BC.lines bytes, not (BC.null l), BC.head l /= ' ']
      keys `shouldBe` ["name", "source", "template", "blueprint"]
