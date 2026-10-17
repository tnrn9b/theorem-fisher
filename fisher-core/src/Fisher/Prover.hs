-- | The provers a Fisher project can hold: Lean 4, Rocq and Isabelle. One
-- 'Prover' value per kind carries what the rest of Fisher needs to know
-- about it (its project markers, source files, MCP server and binaries), so
-- that a later prover adds a value rather than case analyses.
module Fisher.Prover
  ( ProverKind (..)
  , Prover (..)
  , Marker (..)
  , ServerSpec (..)
  , Binary (..)
  , prover
  , proverByName
  , proverTitle
  , withArticle
  , rocqCoqcTools
  , pvToolsFor
  , petMissingWarning
  , binaryFix
  , findRocqCompiler
  , findIsabelle
  , versionBelow
  , markerKinds
  , isProverRoot
  , detectProver
  , findProverRoot
  , tieMessage
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Directory (canonicalizePath, doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))

import Fisher.Paths (expandUser)
import Fisher.Which (whichIn)
import Fisher.Prover.Holes (Hole, holes)
import Fisher.Prover.Kind (ProverKind (..))

-- | What marks a directory as a project root of a kind.
data Marker
  = FileMarker FilePath
  -- ^ a file of this name
  | DuneMarker
  -- ^ a @dune-project@ with a @(using coq ...)@ or @(using rocq ...)@ stanza
  deriving (Eq, Show)

-- | The prover's MCP server: the name it is registered under, the command
-- that launches it by default, the environment variable that overrides the
-- package spec, and the tools Fisher's native toolset offers from it.
data ServerSpec = ServerSpec
  { svName :: Text
  , svDefault :: [Text]
  , svOverride :: String
  , svTools :: [Text]
  }
  deriving (Eq, Show)

-- | A program doctor checks: its name, the arguments that print its
-- version, and whether the prover's workflows cannot run without it.
data Binary = Binary
  { binName :: Text
  , binVersionArgs :: [Text]
  , binRequired :: Bool
  }
  deriving (Eq, Show)

data Prover = Prover
  { pvKind :: ProverKind
  , pvName :: Text
  , pvMarkers :: [Marker]
  , pvSourceExt :: [String]
  , pvServer :: ServerSpec
  , pvBinaries :: [Binary]
  , pvHoles :: Text -> [Hole]
  -- ^ the unproved-goal markers (@sorry@, @Admitted.@, ...) in a source text
  }

prover :: ProverKind -> Prover
prover = \case
  Lean4 ->
    Prover
      { pvKind = Lean4
      , pvName = "lean4"
      , pvMarkers = [FileMarker "lakefile.lean", FileMarker "lakefile.toml"]
      , pvSourceExt = [".lean"]
      , pvServer =
          -- Descriptive only: the Lean workflow builds its server command in
          -- fisher-workflow (Settings.resolveUvRunner), as before.
          ServerSpec
            { svName = "lean-lsp"
            , svDefault = ["uvx", "--from", "lean-lsp-mcp", "lean-lsp-mcp"]
            , svOverride = "GAUSS_AUTOFORMALIZE_LEAN_LSP_MCP_SPEC"
            , svTools = []
            }
      , pvBinaries = [Binary "lake" ["--version"] True]
      , pvHoles = holes Lean4
      }
  Rocq ->
    Prover
      { pvKind = Rocq
      , pvName = "rocq"
      , pvMarkers = [FileMarker "_RocqProject", FileMarker "_CoqProject", DuneMarker]
      , pvSourceExt = [".v"]
      , pvServer =
          ServerSpec
            { svName = "rocq"
            -- pinned: the commit whose 13 tools (12 here, rocq_switch
            -- left out) were checked against svTools
            , svDefault = ["uvx", "--from", "git+https://github.com/LLM4Rocq/rocq-mcp@6983113d0844c0b7f987c79dab13988445109bfb", "rocq-mcp"]
            , svOverride = "GAUSS_AUTOFORMALIZE_ROCQ_MCP_SPEC"
            , svTools =
                [ "rocq_compile", "rocq_compile_file", "rocq_verify", "rocq_query", "rocq_assumptions"
                , "rocq_toc", "rocq_notations", "rocq_start", "rocq_step_multi", "rocq_check"
                , "rocq_diag", "rocq_health" ]
            }
      , pvBinaries =
          -- coqc or rocq, either one ('findRocqCompiler'): Rocq-MCP runs
          -- coqc, or rocq through the coqc wrapper Fisher stages
          [ Binary "coqc" ["--version"] False
          , Binary "rocq" ["--version"] False
          , Binary "pet" ["--version"] False
          , Binary "uvx" ["--version"] True
          -- uvx brings its own Python: python3 is checked, not required
          , Binary "python3" ["--version"] False ]
      , pvHoles = holes Rocq
      }
  Isabelle ->
    Prover
      { pvKind = Isabelle
      , pvName = "isabelle"
      , pvMarkers = [FileMarker "ROOT", FileMarker "ROOTS"]
      , pvSourceExt = [".thy"]
      , pvServer =
          ServerSpec
            { svName = "isabelle"
            , svDefault = ["uvx", "--from", "isabelle-mcp==0.6.0", "isabelle-mcp"]
            , svOverride = "GAUSS_AUTOFORMALIZE_ISABELLE_MCP_SPEC"
            , svTools =
                [ "isabelle_launch", "isabelle_terminate", "isabelle_evaluate_to", "isabelle_evaluation_status"
                , "isabelle_cancel_evaluation", "isabelle_hover", "isabelle_definition", "isabelle_local_occurrences"
                , "isabelle_goal", "isabelle_find_theorems", "isabelle_command_output", "isabelle_command_status"
                , "isabelle_session_info" ]
            }
      , pvBinaries =
          [ Binary "isabelle" ["version"] True
          , Binary "uvx" ["--version"] True
          , Binary "python3" ["--version"] False ]
      , pvHoles = holes Isabelle
      }

-- | The kind a manifest's @kind@ (or @--prover@) names.
proverByName :: Text -> Maybe ProverKind
proverByName n = case [k | k <- [minBound .. maxBound], pvName (prover k) == n] of
  (k : _) -> Just k
  [] -> Nothing

-- | The kind as messages name it: @Lean 4@, @Rocq@, @Isabelle@.
proverTitle :: ProverKind -> Text
proverTitle = \case
  Lean4 -> "Lean 4"
  Rocq -> "Rocq"
  Isabelle -> "Isabelle"

-- | The title with its article: @a Rocq@, @an Isabelle@.
withArticle :: ProverKind -> Text
withArticle k =
  let t = proverTitle k
   in (if T.take 1 t `elem` ["A", "E", "I", "O", "U"] then "an " else "a ") <> t

-- | The Rocq-MCP tools that work with @coqc@ alone, without coq-lsp's @pet@.
rocqCoqcTools :: [Text]
rocqCoqcTools = ["rocq_compile", "rocq_compile_file", "rocq_verify", "rocq_diag", "rocq_health"]

-- | The tools Fisher's native toolset offers from the prover's server; the
-- flag says whether coq-lsp's @pet@ is on PATH. Without it Rocq-MCP's
-- interactive tools fail, so only the coqc ones are offered.
pvToolsFor :: ProverKind -> Bool -> [Text]
pvToolsFor Rocq False = rocqCoqcTools
pvToolsFor k _ = svTools (pvServer (prover k))

-- | The one warning a Rocq session without @pet@ prints.
petMissingWarning :: Text
petMissingWarning = "pet not found: the interactive Rocq tools are unavailable"

-- | The command or step that installs a missing binary (or, for
-- @isabelle-mcp@, registers Isabelle-MCP's component).
binaryFix :: ProverKind -> Text -> Text
binaryFix kind name = case (kind, name) of
  (_, "uvx") -> "install uv"
  (_, "uv") -> "install uv"
  (_, "python3") -> "install Python 3.12"
  (_, "python") -> "install Python 3.12"
  (Lean4, _) -> "install elan"
  (Rocq, _) -> "opam install rocq-prover coq-lsp"
  (Isabelle, "isabelle-mcp") -> "isabelle-mcp install"
  (Isabelle, _) -> "install Isabelle2025-2 from isabelle.in.tum.de"

-- | What Rocq-MCP compiles with, on this PATH: @coqc@ when it is there,
-- else @rocq@ (for which Fisher stages a @coqc@ wrapper that runs
-- @rocq compile@). The name found and its path.
findRocqCompiler :: [(String, String)] -> IO (Maybe (Text, FilePath))
findRocqCompiler env = go ["coqc", "rocq"]
  where
    path = maybe "" id (lookup "PATH" env)
    go [] = pure Nothing
    go (n : ns) = whichIn path (T.unpack n) >>= maybe (go ns) (\p -> pure (Just (n, p)))

-- | Isabelle on PATH, else under @ISABELLE_HOME@, else the 2025-2 app in
-- @~/Applications@ or @/Applications@: its @bin/isabelle@, and whether
-- PATH found it. The launch check, @fisher check@ and doctor all look here.
findIsabelle :: [(String, String)] -> IO (Maybe (FilePath, Bool))
findIsabelle env =
  whichIn (maybe "" id (lookup "PATH" env)) "isabelle" >>= \case
    Just p -> pure (Just (p, True))
    Nothing -> firstIn candidates
  where
    app = "Isabelle2025-2.app"
    candidates =
      [h | Just h <- [lookup "ISABELLE_HOME" env], not (null h)]
        <> [h </> "Applications" </> app | Just h <- [lookup "HOME" env], not (null h)]
        <> ["/Applications" </> app]
    firstIn [] = pure Nothing
    firstIn (d : ds) =
      whichIn (d </> "bin") "isabelle" >>= \case
        Just p -> pure (Just (p, False))
        Nothing -> firstIn ds

-- | Whether a version is below another, the shorter padded with zeros
-- (@9@ is @9.0@, not below it).
versionBelow :: [Int] -> [Int] -> Bool
versionBelow a b =
  let n = max (length a) (length b)
      pad v = v <> replicate (n - length v) 0
   in pad a < pad b

-- | Is this marker present in the directory. Lean's markers test any path,
-- as before; the others want a regular file, so that a directory @root@ on
-- a case-insensitive file system is not taken for an Isabelle @ROOT@.
markerPresent :: ProverKind -> FilePath -> Marker -> IO Bool
markerPresent kind dir = \case
  FileMarker f
    | kind == Lean4 -> doesPathExist (dir </> f)
    | otherwise -> doesFileExist (dir </> f)
  DuneMarker -> do
    let f = dir </> "dune-project"
    yes <- doesFileExist f
    if not yes
      then pure False
      else either (\(_ :: IOException) -> False) usesRocq <$> try (BS.readFile f)

-- | A @dune-project@ line @(using coq ...)@ or @(using rocq ...)@.
usesRocq :: BS.ByteString -> Bool
usesRocq = any stanza . T.lines . TE.decodeUtf8With TE.lenientDecode
  where
    stanza l = case T.uncons (T.stripStart l) of
      Just ('(', rest) -> case T.words rest of
        ("using" : lang : _) -> T.dropWhileEnd (\c -> c == ')' || isSpace c) lang `elem` ["coq", "rocq"]
        _ -> False
      _ -> False

isProverRoot :: ProverKind -> FilePath -> IO Bool
isProverRoot kind dir = or <$> mapM (markerPresent kind dir) (pvMarkers (prover kind))

-- | The kinds whose markers the directory holds, in 'ProverKind' order.
markerKinds :: FilePath -> IO [ProverKind]
markerKinds dir = filterM (`isProverRoot` dir) [minBound .. maxBound]

-- | @<dir> holds both a <kind> and a <kind> project; ...@
tieMessage :: FilePath -> ProverKind -> ProverKind -> Text
tieMessage dir a b =
  T.pack dir <> " holds both " <> withArticle a <> " and " <> withArticle b
    <> " project; pass --prover lean4|rocq|isabelle."

resolve :: FilePath -> IO FilePath
resolve p = expandUser p >>= canonicalizePath

selfAndParents :: FilePath -> [FilePath]
selfAndParents p =
  let up = takeDirectory p
   in if up == p then [p] else p : selfAndParents up

-- | The nearest directory at or above @start@ holding any kind's marker,
-- with that kind. Markers of two kinds in that directory are an error.
detectProver :: FilePath -> IO (Either Text (Maybe (ProverKind, FilePath)))
detectProver start = resolve start >>= go . selfAndParents
  where
    go [] = pure (Right Nothing)
    go (d : ds) =
      markerKinds d >>= \case
        [] -> go ds
        [k] -> pure (Right (Just (k, d)))
        (a : b : _) -> pure (Left (tieMessage d a b))

-- | The nearest directory at or above @start@ holding a marker of this
-- kind, whatever else it holds.
findProverRoot :: ProverKind -> FilePath -> IO (Maybe FilePath)
findProverRoot kind start = resolve start >>= go . selfAndParents
  where
    go [] = pure Nothing
    go (d : ds) = isProverRoot kind d >>= \yes -> if yes then pure (Just d) else go ds
