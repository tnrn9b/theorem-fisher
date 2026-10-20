{-# LANGUAGE TemplateHaskell #-}
-- | The system prompt (@agent/prompt_builder.py@ and
-- @AIAgent._build_system_prompt@): the identity, tool guidance, the
-- caller's system message, the memory snapshot, the project's context
-- files (AGENTS.md, .cursorrules, .cursor/rules, SOUL.md) after an
-- injection scan and truncation, the start time and the platform hint.
module Fisher.Agent.Prompt
  ( PromptInput (..)
  , buildSystemPrompt
  , buildContextFilesPrompt
  , ensureFisherHomeLayout
  , scanContextContent
  , truncateContent
  , MemorySnapshot (..)
  , loadMemorySnapshot
  , renderMemoryBlock
  , memoryPct
  , commasI
  , defaultAgentIdentity
  , memoryGuidance
  , sessionSearchGuidance
  , skillsGuidance
  , platformHints
  , defaultSoulMd
  , threatPatterns
  , timestampLine
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.List (isSuffixOf, sort, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (LocalTime, defaultTimeLocale, formatTime)
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.Posix.Files (setFileMode)

import Fisher.LLM.Identity (defaultAgentIdentity)
import qualified Fisher.Py.Re as Re
import Fisher.Py.Value
import Fisher.Tools.Py (pyStrip)

decode :: BS.ByteString -> Text
decode = TE.decodeUtf8With TEE.lenientDecode

memoryGuidance, sessionSearchGuidance, skillsGuidance, defaultSoulMd :: Text
memoryGuidance = decode $(makeRelativeToProject "data/memory-guidance.txt" >>= embedFile)
sessionSearchGuidance = decode $(makeRelativeToProject "data/session-search-guidance.txt" >>= embedFile)
skillsGuidance = decode $(makeRelativeToProject "data/skills-guidance.txt" >>= embedFile)
defaultSoulMd = decode $(makeRelativeToProject "data/default-soul.md" >>= embedFile)

platformHints :: [(Text, Text)]
platformHints = case loads $(makeRelativeToProject "data/platform-hints.json" >>= embedFile) of
  Right (PDict kvs) -> [(k, v) | (k, PStr v) <- kvs]
  _ -> []

-- | @_CONTEXT_THREAT_PATTERNS@: (pattern, finding id), matched ignoring case.
threatPatterns :: [(Re.Regex, Text)]
threatPatterns = case loads $(makeRelativeToProject "data/threat-patterns.json" >>= embedFile) of
  Right (PList ps) -> [(Re.compileOrDie [Re.IgnoreCase] p, i) | PList [PStr p, PStr i] <- ps]
  _ -> []

-- | @_CONTEXT_INVISIBLE_CHARS@, in code-point order (upstream iterates a
-- set, so its order changes from run to run).
invisibleChars :: [Char]
invisibleChars = ['\x200b', '\x200c', '\x200d', '\x202a', '\x202b', '\x202c', '\x202d', '\x202e', '\x2060', '\xfeff']

-- | @_scan_context_content(content, filename)@: the content, or the
-- notice that replaces it, and the findings (upstream logs them).
scanContextContent :: Text -> Text -> (Text, [Text])
scanContextContent content filename =
  let inv = ["invisible unicode U+" <> T.justifyRight 4 '0' (T.toUpper (T.pack (showHex (fromEnum c) ""))) | c <- invisibleChars, T.any (== c) content]
      threats = [i | (rx, i) <- threatPatterns, Just _ <- [Re.search rx content]]
      findings = inv <> threats
   in if null findings
        then (content, [])
        else ("[BLOCKED: " <> filename <> " contained potential prompt injection (" <> T.intercalate ", " findings <> "). Content not loaded.]", findings)

-- | @_truncate_content(content, filename, max_chars=20000)@
truncateContent :: Text -> Text -> Text
truncateContent content filename
  | T.length content <= maxChars = content
  | otherwise =
      T.take headN content
        <> "\n\n[...truncated " <> filename <> ": kept " <> tshow headN <> "+" <> tshow tailN <> " of " <> tshow (T.length content)
        <> " chars. Use file tools to read the full file.]\n\n"
        <> T.takeEnd tailN content
  where
    maxChars = 20000
    headN = 14000
    tailN = 4000

-- | Upstream's home setup at start: the home and its @cron@, @sessions@, @logs@ and
-- @memories@ directories (mode 0700), and SOUL.md seeded (0600).
ensureFisherHomeLayout :: FilePath -> IO ()
ensureFisherHomeLayout home = do
  createDirectoryIfMissing True home
  secure home
  mapM_ (\d -> createDirectoryIfMissing True (home </> d) >> secure (home </> d)) ["cron", "sessions", "logs", "memories"]
  let soul = home </> "SOUL.md"
  exists <- doesFileExist soul
  unless exists $ do
    BS.writeFile soul (TE.encodeUtf8 defaultSoulMd)
    _ <- try @SomeException (setFileMode soul 0o600)
    pure ()
  where
    secure d = () <$ try @SomeException (setFileMode d 0o700)

readStripped :: FilePath -> IO (Maybe Text)
readStripped p = do
  r <- try @SomeException (BS.readFile p)
  pure $ case r of
    Right b -> Just (pyStrip (decode b))
    Left _ -> Nothing

-- | @build_context_files_prompt(cwd)@. Within a directory, entries are
-- visited in name order (upstream follows the order the filesystem lists
-- them in).
buildContextFilesPrompt :: (Text -> IO ()) -> FilePath -> FilePath -> IO Text
buildContextFilesPrompt warn fisherHome cwd0 = do
  cwd <- canonicalizePath cwd0
  top <- firstExisting [cwd </> "AGENTS.md", cwd </> "agents.md"]
  agents <- case top of
    Nothing -> pure Nothing
    Just _ -> do
      files <- walk cwd
      let ordered = sortOn (length . splitDirectories) files
      parts <- forM ordered $ \p -> do
        c <- readStripped p
        case c of
          Just content | not (T.null content) -> do
            let rel = T.pack (makeRelative cwd p)
            scanned <- scan content rel
            pure ("## " <> rel <> "\n\n" <> scanned <> "\n\n")
          _ -> pure ""
      let total = T.concat parts
      pure (if T.null total then Nothing else Just (truncateContent total "AGENTS.md"))
  crFile <- doesPathExist (cwd </> ".cursorrules")
  cr1 <-
    if crFile
      then do
        c <- readStripped (cwd </> ".cursorrules")
        case c of
          Just content | not (T.null content) -> do
            s <- scan content ".cursorrules"
            pure ("## .cursorrules\n\n" <> s <> "\n\n")
          _ -> pure ""
      else pure ""
  let rulesDir = cwd </> ".cursor" </> "rules"
  isDir <- doesDirectoryExist rulesDir
  cr2 <-
    if not isDir
      then pure ""
      else do
        names <- sort . filter (".mdc" `isSuffixOf`) <$> listDirectory rulesDir
        fmap T.concat . forM names $ \n -> do
          isFile <- doesFileExist (rulesDir </> n)
          c <- if isFile then readStripped (rulesDir </> n) else pure Nothing
          case c of
            Just content | not (T.null content) -> do
              let label = ".cursor/rules/" <> T.pack n
              s <- scan content label
              pure ("## " <> label <> "\n\n" <> s <> "\n\n")
            _ -> pure ""
  let cursor = cr1 <> cr2
  _ <- try @SomeException (ensureFisherHomeLayout fisherHome)
  soulOk <- doesPathExist (fisherHome </> "SOUL.md")
  soul <-
    if not soulOk
      then pure Nothing
      else do
        c <- readStripped (fisherHome </> "SOUL.md")
        case c of
          Just content | not (T.null content) -> do
            s <- scan content "SOUL.md"
            pure (Just (truncateContent s "SOUL.md"))
          _ -> pure Nothing
  let sections = catMaybes [agents, if T.null cursor then Nothing else Just (truncateContent cursor ".cursorrules"), soul]
  pure $
    if null sections
      then ""
      else "# Project Context\n\nThe following project context files have been loaded and should be followed:\n\n" <> T.intercalate "\n" sections
  where
    scan content name = do
      let (out, findings) = scanContextContent content name
      unless (null findings) $ warn ("Context file " <> name <> " blocked: " <> T.intercalate ", " findings)
      pure out
    firstExisting = \case
      [] -> pure Nothing
      (p : ps) -> do
        e <- doesPathExist p
        if e then pure (Just p) else firstExisting ps
    walk dir = do
      r <- try @SomeException (listDirectory dir)
      case r of
        Left _ -> pure []
        Right names0 -> do
          let names = sort names0
          kinds <- forM names $ \n -> do
            isD <- doesDirectoryExist (dir </> n)
            isL <- pathIsSymbolicLink (dir </> n)
            pure (n, isD, isL)
          let files = [dir </> n | (n, isD, _) <- kinds, not isD, map toLower n == "agents.md"]
              dirs = [n | (n, isD, isL) <- kinds, isD, not isL, not ("." `isPrefixOfS` n), n `notElem` ["node_modules", "__pycache__", "venv", ".venv"]]
          sub <- concat <$> mapM (walk . (dir </>)) dirs
          pure (files <> sub)
    isPrefixOfS p n = take (length p) n == p

-- | The memory snapshot frozen into the prompt: the rendered MEMORY.md and
-- USER.md blocks (empty when there are no entries).
data MemorySnapshot = MemorySnapshot
  { msMemory :: Text
  , msUser :: Text
  }
  deriving (Eq, Show)

-- | @MemoryStore(memory_char_limit, user_char_limit).load_from_disk()@
loadMemorySnapshot :: FilePath -> Int -> Int -> IO MemorySnapshot
loadMemorySnapshot memDir memLimit userLimit = do
  createDirectoryIfMissing True memDir
  mem <- entries (memDir </> "MEMORY.md")
  usr <- entries (memDir </> "USER.md")
  pure (MemorySnapshot (renderMemoryBlock "memory" memLimit mem) (renderMemoryBlock "user" userLimit usr))
  where
    entries p = do
      e <- doesFileExist p
      if not e
        then pure []
        else do
          r <- try @SomeException (BS.readFile p)
          case r of
            Left _ -> pure []
            Right b ->
              let raw = decode b
               in if T.null (pyStrip raw) then pure [] else pure (dedupe [x | x <- map pyStrip (T.splitOn "\n§\n" raw), not (T.null x)])
    dedupe = foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) []

-- | @int((current / limit) * 100) if limit > 0 else 0@ (in floating point,
-- as upstream: 29/100 is 28%).
memoryPct :: Int -> Int -> Int
memoryPct current limit
  | limit > 0 = truncate ((fromIntegral current / fromIntegral limit) * 100 :: Double)
  | otherwise = 0

-- | @_render_block(target, entries)@
renderMemoryBlock :: Text -> Int -> [Text] -> Text
renderMemoryBlock _ _ [] = ""
renderMemoryBlock target limit es =
  let content = T.intercalate "\n§\n" es
      current = T.length content
      pct = memoryPct current limit
      usage = tshow pct <> "% — " <> commasI current <> "/" <> commasI limit <> " chars]"
      header =
        if target == "user"
          then "USER PROFILE (who the user is) [" <> usage
          else "MEMORY (your personal notes) [" <> usage
      sep = T.replicate 46 "═"
   in sep <> "\n" <> header <> "\n" <> sep <> "\n" <> content

-- | What the agent knows when it builds its prompt.
data PromptInput = PromptInput
  { piToolNames :: [Text]
  , piSystemMessage :: Maybe Text
  , piMemory :: Maybe (Bool, Bool, MemorySnapshot)
  -- ^ memory enabled, user profile enabled, the snapshot (when a store exists)
  , piSkipContextFiles :: Bool
  , piFisherHome :: FilePath
  , piCwd :: FilePath
  , piNow :: LocalTime
  , piSessionId :: Maybe Text
  -- ^ set when @pass_session_id@
  , piPlatform :: Maybe Text
  }

-- | @Conversation started: Friday, October 02, 2026 04:50 AM@
timestampLine :: LocalTime -> Text
timestampLine t = "Conversation started: " <> T.pack (formatTime defaultTimeLocale "%A, %B %d, %Y %I:%M %p" t)

-- | @_build_system_prompt(system_message)@
buildSystemPrompt :: (Text -> IO ()) -> PromptInput -> IO Text
buildSystemPrompt warn pin = do
  ctx <- if piSkipContextFiles pin then pure "" else buildContextFilesPrompt warn (piFisherHome pin) (piCwd pin)
  let names = piToolNames pin
      guidance =
        [memoryGuidance | "memory" `elem` names]
          <> [sessionSearchGuidance | "session_search" `elem` names]
          <> [skillsGuidance | "skill_manage" `elem` names]
      memParts = case piMemory pin of
        Just (memOn, userOn, snap) ->
          [msMemory snap | memOn, not (T.null (msMemory snap))] <> [msUser snap | userOn, not (T.null (msUser snap))]
        Nothing -> []
      ts = timestampLine (piNow pin) <> maybe "" ("\nSession ID: " <>) (piSessionId pin)
      hint = lookup (T.strip (T.toLower (maybe "" id (piPlatform pin)))) platformHints
      parts =
        [defaultAgentIdentity]
          <> [T.unwords guidance | not (null guidance)]
          <> maybe [] pure (piSystemMessage pin)
          <> memParts
          <> [ctx | not (T.null ctx)]
          <> [ts]
          <> maybe [] pure hint
  pure (T.intercalate "\n\n" parts)

tshow :: Int -> Text
tshow = T.pack . show

commasI :: Int -> Text
commasI n = T.reverse (T.intercalate "," (T.chunksOf 3 (T.reverse (T.pack (show n)))))
