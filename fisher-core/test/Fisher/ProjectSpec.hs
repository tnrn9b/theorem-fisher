module Fisher.ProjectSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (newIORef)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Project
import Fisher.ProjectCommand (ProjectSession (..), handleProjectTokens)
import Fisher.Prover (ProverKind (..))
import Fisher.Py (lookupKey)
import Fisher.TestUtil

manifestErrorContaining :: T.Text -> Selector ProjectError
manifestErrorContaining needle = \case
  ProjectManifestInvalid m -> needle `T.isInfixOf` m
  _ -> False

writeManifest :: FilePath -> [String] -> IO ()
writeManifest root ls = writeFile (root </> ".fisher" </> "project.yaml") (unlines ls)

goodManifest :: [String]
goodManifest =
  [ "schema_version: 1", "name: Demo Project", "kind: lean4", "lean_root: ."
  , "created_at: 2026-03-18T00:00:00+00:00", "source:", "  mode: init", "  template_source: ''"
  , "blueprint:", "  markers: []", "paths:", "  runtime: .fisher/runtime", "  cache: .fisher/cache"
  , "  workflows: .fisher/workflows" ]

-- | A manifest of another kind: @kind@ and @root@ in place of @lean_root@.
kindManifest :: String -> String -> [String]
kindManifest kind root =
  replaceLine "lean_root" ("root: " <> root) (replaceLine "kind" ("kind: " <> kind) goodManifest)

manifestError :: T.Text -> Selector ProjectError
manifestError expected = \case
  ProjectManifestInvalid m -> m == expected
  _ -> False

-- | Drop the timestamp line, which changes with the clock.
withoutCreatedAt :: String -> [String]
withoutCreatedAt = filter (\l -> take 11 l /= "created_at:") . lines

session :: FilePath -> IO ProjectSession
session cwd = ProjectSession cwd (object []) [] <$> newIORef Nothing

replaceLine :: String -> String -> [String] -> [String]
replaceLine prefix new = map (\l -> if take (length prefix) l == prefix then new else l)

spec :: Spec
spec = do
  -- Ported from upstream tests/cli/test_project.py.
  it "initializes and loads a project, creating the manifest and runtime dirs" $ withTmp $ \tmp -> do
    let root = tmp </> "Demo"
    writeLeanRoot root
    project <- initializeProject root defaultInitOptions { initName = Just "Demo Project" }
    loaded <- loadProject root
    projectLabel loaded `shouldBe` "Demo Project"
    projectManifestPath loaded `shouldBe` root </> ".fisher" </> "project.yaml"
    projectLeanRoot loaded `shouldBe` root
    projectRuntimeDir loaded `shouldBe` root </> ".fisher" </> "runtime"
    projectCacheDir loaded `shouldBe` root </> ".fisher" </> "cache"
    projectWorkflowsDir loaded `shouldBe` root </> ".fisher" </> "workflows"
    lookupKey "schema_version" (projectManifest loaded) `shouldBe` Just (Number 1)
    project `shouldBe` loaded

  it "discovers the nearest enclosing project" $ withTmp $ \tmp -> do
    let outer = tmp </> "Outer"
        nestedProject = outer </> "Nested" </> "Project"
    writeLeanRoot outer
    writeLeanRoot nestedProject
    outerP <- initializeProject outer defaultInitOptions { initName = Just "Outer Project" }
    nested <- initializeProject nestedProject defaultInitOptions { initName = Just "Nested Project" }
    let active = nestedProject </> "Math" </> "Subdir"
    createDirectoryIfMissing True active
    found <- discoverProject active
    found `shouldBe` nested
    found `shouldNotBe` outerP

  it "rejects a manifest whose lean_root points outside the root" $ withTmp $ \tmp -> do
    let root = tmp </> "Demo"
    writeLeanRoot root
    writeLeanRoot (tmp </> "Outside")
    _ <- initializeProject root defaultInitOptions { initName = Just "Demo Project" }
    writeManifest root (replaceLine "lean_root" "lean_root: ../Outside" goodManifest)
    loadProject root `shouldThrow` manifestErrorContaining "outside the project root"

  it "summarizes with the relative cwd and the blueprint flag" $ withTmp $ \tmp -> do
    let root = tmp </> "BlueprintDemo"
    writeLeanRoot root
    createDirectoryIfMissing True (root </> "templates")
    writeFile (root </> "templates" </> "blueprint.yml") "name: demo\n"
    project <- initializeProject root defaultInitOptions
      { initName = Just "Blueprint Demo", initBlueprintMarkers = Just ["templates/blueprint.yml"] }
    createDirectoryIfMissing True (root </> "Math")
    formatProjectSummary project (Just (root </> "Math"))
      `shouldReturn` "Blueprint Demo · cwd:Math · blueprint"

  it "copies a local template directory" $ withTmp $ \tmp -> do
    let template = tmp </> "template"
        target = tmp </> "generated"
    writeLeanRoot template
    writeFile (template </> "README.md") "# Template\n"
    populateFromTemplate target (T.pack template)
    readFile (target </> "lakefile.lean") `shouldReturn` "-- lean project\n"
    readFile (target </> "README.md") `shouldReturn` "# Template\n"

  it "requires a configured template source" $ withTmp $ \tmp ->
    populateFromTemplate (tmp </> "generated") "" `shouldThrow` \case
      ProjectTemplateUnavailable m -> "template source" `T.isInfixOf` m
      _ -> False

  it "prefers the environment's template source over the config" $ do
    let config = object ["fisher" .= object ["project" .= object ["template_source" .= ("config-template" :: String)]]]
    resolveTemplateSource config [(templateEnvVar, "env-template")] `shouldBe` "env-template"

  -- Beyond upstream's tests: the branches upstream does not exercise.
  describe "edge cases" $ do
    it "falls back to the config template source when the env value is blank" $ do
      let config = object ["fisher" .= object ["project" .= object ["template_source" .= (" from-config " :: String)]]]
      resolveTemplateSource config [(templateEnvVar, "   ")] `shouldBe` "from-config"
      resolveTemplateSource (object []) [] `shouldBe` ""

    it "summarizes without a cwd part at the root or outside it" $ withTmp $ \tmp -> do
      let root = tmp </> "P"
      writeLeanRoot root
      p <- initializeProject root defaultInitOptions { initName = Just "P", initBlueprintMarkers = Just [] }
      formatProjectSummary p (Just root) `shouldReturn` "P"
      formatProjectSummary p (Just tmp) `shouldReturn` "P"

    -- Upstream lists lakefile.lean among the blueprint markers, so any Lean
    -- project that init detects markers for is flagged as a blueprint. This
    -- pins that behaviour; it is upstream's, not a choice made here.
    it "flags every detected Lean project as a blueprint, as upstream does" $ withTmp $ \tmp -> do
      let root = tmp </> "Plain"
      writeLeanRoot root
      p <- initializeProject root defaultInitOptions
      projectBlueprintMarkers p `shouldBe` ["lakefile.lean"]
      formatProjectSummary p Nothing `shouldReturn` "Plain · blueprint"

    it "accepts the manifest path and the .fisher dir as targets" $ withTmp $ \tmp -> do
      let root = tmp </> "Demo"
      writeLeanRoot root
      p <- initializeProject root defaultInitOptions
      loadProject (root </> ".fisher") `shouldReturn` p
      loadProject (root </> ".fisher" </> "project.yaml") `shouldReturn` p

    it "names the project after its directory by default" $ withTmp $ \tmp -> do
      let root = tmp </> "Lemmas"
      writeLeanRoot root
      p <- initializeProject root defaultInitOptions
      projectName p `shouldBe` "Lemmas"
      projectSourceMode p `shouldBe` "init"

    it "reports a missing project" $ withTmp $ \tmp ->
      discoverProject tmp `shouldThrow` \case
        ProjectNotFound m -> "No active Fisher project found" `T.isInfixOf` m
        _ -> False

    it "reports a .fisher dir without a manifest" $ withTmp $ \tmp -> do
      createDirectoryIfMissing True (tmp </> ".fisher")
      loadProject tmp `shouldThrow` manifestErrorContaining "`project.yaml` is missing"

    it "refuses a wrong schema version, quoting it" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (replaceLine "schema_version" "schema_version: 2" goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "has schema_version=2; expected 1."
      writeManifest tmp (filter (\l -> take 6 l /= "schema") goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "has schema_version=None; expected 1."

    it "refuses a blank name and a foreign kind" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (replaceLine "name" "name: '  '" goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "is missing a non-empty `name`."
      writeManifest tmp (replaceLine "kind" "kind: coq" goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "has unsupported `kind`: 'coq'."

    it "refuses absolute, empty and null path fields" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (replaceLine "  cache" "  cache: /tmp/cache" goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "`paths.cache` must be relative."
      writeManifest tmp (replaceLine "lean_root" "lean_root: null" goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "`lean_root` must not be empty."
      writeManifest tmp (replaceLine "paths" "paths: null" (filter (\l -> take 3 l /= "  r" && take 3 l /= "  c" && take 3 l /= "  w") goodManifest))
      loadProject tmp `shouldThrow` manifestErrorContaining "has invalid `paths` metadata."

    it "treats null source and blueprint sections as empty" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp
        [ "schema_version: 1", "name: Bare", "source: null", "blueprint: null" ]
      p <- loadProject tmp
      projectSourceMode p `shouldBe` "init"
      projectBlueprintMarkers p `shouldBe` []
      doesDirectoryExist (tmp </> ".fisher" </> "workflows") `shouldReturn` True

    it "refuses a lean_root that is not a Lean project" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      createDirectoryIfMissing True (tmp </> ".fisher")
      createDirectoryIfMissing True (tmp </> "docs")
      writeManifest tmp (replaceLine "lean_root" "lean_root: docs" goodManifest)
      loadProject tmp `shouldThrow` manifestErrorContaining "declares lean_root='docs', but that directory"

    it "refuses init outside a Lean project" $ withTmp $ \tmp ->
      initializeProject (tmp </> "Empty") defaultInitOptions `shouldThrow` \case
        ProjectCommandFailed m -> "Lean project root not found" `T.isInfixOf` m
        _ -> False

    it "refuses a template target that already exists" $ withTmp $ \tmp ->
      populateFromTemplate tmp "anything" `shouldThrow` \case
        e@(ProjectCommandFailed m) -> isCommandError e && "already exists" `T.isInfixOf` m
        _ -> False

    it "quotes created_at so PyYAML reads it back as text" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      p <- initializeProject tmp defaultInitOptions
      written <- readFile (projectManifestPath p)
      let line = filter (\l -> take 11 l == "created_at:") (lines written)
      map (take 13) line `shouldBe` ["created_at: '"]
      T.length (projectCreatedAt p) `shouldBe` 25

    it "detects blueprint markers present at the root" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      writeFile (tmp </> "lean-toolchain") "leanprover/lean4:v4.20.0\n"
      detectBlueprintMarkers tmp `shouldReturn` ["lean-toolchain", "lakefile.lean"]

  describe "provers" $ do
    -- Review Focus: a manifest written before Rocq and Isabelle loads as before.
    it "loads an old Lean manifest (no kind, lean_root only) unchanged" $ withTmp $ \tmp -> do
      writeLeanRoot (tmp </> "lean")
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp
        [ "schema_version: 1", "name: Old", "lean_root: lean", "created_at: '2026-03-18T00:00:00+00:00'" ]
      p <- loadProject tmp
      projectKind p `shouldBe` "lean4"
      projectProver p `shouldBe` Lean4
      projectLeanRoot p `shouldBe` tmp </> "lean"
      projectProverRoot p `shouldBe` projectLeanRoot p
      projectName p `shouldBe` "Old"

    it "accepts root as a synonym of lean_root, and both when they agree" $ withTmp $ \tmp -> do
      writeLeanRoot (tmp </> "lean")
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (replaceLine "lean_root" "root: lean" goodManifest)
      p <- loadProject tmp
      projectProverRoot p `shouldBe` tmp </> "lean"
      projectLeanRoot p `shouldBe` tmp </> "lean"
      writeManifest tmp (goodManifest <> ["root: ."]) >> writeLeanRoot tmp
      q <- loadProject tmp
      projectProverRoot q `shouldBe` tmp

    it "refuses lean_root and root that disagree" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      writeLeanRoot (tmp </> "lean")
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (goodManifest <> ["root: lean"])
      loadProject tmp `shouldThrow`
        manifestError (T.pack (tmp </> ".fisher" </> "project.yaml") <> " declares lean_root and root differently.")

    it "loads a Rocq manifest with its root" $ withTmp $ \tmp -> do
      writeRocqRoot (tmp </> "coq")
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (kindManifest "rocq" "coq")
      p <- loadProject tmp
      projectKind p `shouldBe` "rocq"
      projectProver p `shouldBe` Rocq
      projectProverRoot p `shouldBe` tmp </> "coq"
      projectLeanRoot p `shouldBe` tmp </> "coq"

    it "loads an Isabelle manifest whose root defaults to the project" $ withTmp $ \tmp -> do
      writeIsabelleRoot tmp
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (filter (\l -> take 4 l /= "root") (kindManifest "isabelle" "."))
      p <- loadProject tmp
      projectProver p `shouldBe` Isabelle
      projectProverRoot p `shouldBe` tmp

    it "refuses a Rocq root that is not a Rocq project root" $ withTmp $ \tmp -> do
      writeLeanRoot (tmp </> "docs")
      createDirectoryIfMissing True (tmp </> ".fisher")
      writeManifest tmp (kindManifest "rocq" "docs")
      loadProject tmp `shouldThrow`
        manifestError (T.pack (tmp </> ".fisher" </> "project.yaml")
          <> " declares root='docs', but that directory is not a Rocq project root.")
      writeManifest tmp (kindManifest "isabelle" "docs")
      loadProject tmp `shouldThrow`
        manifestError (T.pack (tmp </> ".fisher" </> "project.yaml")
          <> " declares root='docs', but that directory is not an Isabelle project root.")

    it "writes a Lean manifest byte for byte as before" $ withTmp $ \tmp -> do
      let root = tmp </> "d"
      writeLeanRoot root
      p <- initializeProject root defaultInitOptions
      written <- readFile (projectManifestPath p)
      withoutCreatedAt written `shouldBe`
        [ "schema_version: 1", "name: d", "kind: lean4", "lean_root: .", "source:", "  mode: init"
        , "  template_source: ''", "blueprint:", "  markers:", "  - lakefile.lean", "paths:"
        , "  runtime: .fisher/runtime", "  cache: .fisher/cache", "  workflows: .fisher/workflows" ]
      projectProver p `shouldBe` Lean4

    it "initializes a Rocq project with kind and root" $ withTmp $ \tmp -> do
      let root = tmp </> "r"
      writeRocqRoot root
      p <- initializeProject root defaultInitOptions
      written <- readFile (projectManifestPath p)
      withoutCreatedAt written `shouldBe`
        [ "schema_version: 1", "name: r", "kind: rocq", "root: .", "source:", "  mode: init"
        , "  template_source: ''", "blueprint:", "  markers: []", "paths:"
        , "  runtime: .fisher/runtime", "  cache: .fisher/cache", "  workflows: .fisher/workflows" ]
      projectProver p `shouldBe` Rocq
      projectProverRoot p `shouldBe` root
      loadProject root `shouldReturn` p

    it "initializes an Isabelle project" $ withTmp $ \tmp -> do
      let root = tmp </> "i"
      writeIsabelleRoot root
      p <- initializeProject root defaultInitOptions
      projectKind p `shouldBe` "isabelle"
      projectProverRoot p `shouldBe` root

    it "refuses init where two kinds tie, unless a prover is named" $ withTmp $ \tmp -> do
      let root = tmp </> "both"
      writeLeanRoot root
      writeRocqRoot root
      initializeProject root defaultInitOptions `shouldThrow` \case
        ProjectCommandFailed m -> m == T.pack root <> " holds both a Lean 4 and a Rocq project; pass --prover lean4|rocq|isabelle."
        _ -> False
      p <- initializeProject root defaultInitOptions { initProver = Just Rocq }
      projectProver p `shouldBe` Rocq

    it "refuses init with a named prover that has no root here" $ withTmp $ \tmp -> do
      writeLeanRoot (tmp </> "l")
      initializeProject (tmp </> "l") defaultInitOptions { initProver = Just Isabelle } `shouldThrow` \case
        ProjectCommandFailed m -> "Isabelle project root not found" `T.isInfixOf` m
        _ -> False

    it "/project init detects a Rocq project and status names its root" $ withTmp $ \tmp -> do
      let root = tmp </> "R"
      writeRocqRoot root
      s <- session root
      out <- handleProjectTokens s ["init"]
      take 1 out `shouldBe` ["(^_^)b Initialized Fisher project: R"]
      status <- handleProjectTokens s ["status"]
      status `shouldContain` ["  Rocq root: " <> T.pack root]

    it "/project status keeps the Lean root line for Lean" $ withTmp $ \tmp -> do
      let root = tmp </> "L"
      writeLeanRoot root
      s <- session root
      _ <- handleProjectTokens s ["init"]
      status <- handleProjectTokens s ["status"]
      status `shouldContain` ["  Lean root: " <> T.pack root]

    it "/project init takes --prover and refuses a tie without it" $ withTmp $ \tmp -> do
      let root = tmp </> "B"
      writeLeanRoot root
      writeIsabelleRoot root
      s <- session root
      tie <- handleProjectTokens s ["init"]
      take 1 tie `shouldBe` ["(>_<) " <> T.pack root <> " holds both a Lean 4 and an Isabelle project; pass --prover lean4|rocq|isabelle."]
      out <- handleProjectTokens s ["init", "--prover", "isabelle"]
      take 1 out `shouldBe` ["(^_^)b Initialized Fisher project: B"]
      bad <- handleProjectTokens (s { sessionCwd = tmp }) ["init", "--prover", "coq"]
      take 1 bad `shouldBe` ["(>_<) Unknown prover `coq`; expected lean4, rocq or isabelle."]

    it "/project init without a project keeps the Lean message" $ withTmp $ \tmp -> do
      s <- session tmp
      out <- handleProjectTokens s ["init"]
      take 1 out `shouldBe` ["(>_<) Lean project root not found. `/project init` works inside an existing Lean 4 repository."]

    it "/project init and convert load an existing manifest despite a marker tie" $ withTmp $ \tmp -> do
      let root = tmp </> "Tied"
      writeLeanRoot root
      p <- initializeProject root defaultInitOptions { initName = Just "Tied" }
      writeIsabelleRoot root
      createDirectoryIfMissing True (root </> "Sub")
      s <- session (root </> "Sub")
      out <- handleProjectTokens s ["init"]
      take 1 out `shouldBe` ["(^_^)b Initialized Fisher project: Tied"]
      conv <- handleProjectTokens s ["convert", T.pack root]
      take 1 conv `shouldBe` ["(^_^)b Registered existing repository as a Fisher project: Tied"]
      loadProject root `shouldReturn` p
