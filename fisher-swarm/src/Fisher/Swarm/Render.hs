-- | @/swarm@'s views: the task table, one task's detail, the banner summary
-- and the status-bar fragment. Upstream draws these with Rich; Fisher draws
-- the same columns as plain aligned text.
module Fisher.Swarm.Render
  ( formatElapsed
  , statusIndicator
  , renderTable
  , renderDetail
  , summaryLine
  , statusBarFragment
  ) where

import Data.List (transpose)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Swarm.Event (Progress (..))
import Fisher.Swarm.Types

-- | @_format_elapsed@: seconds, then minutes, then hours and minutes.
formatElapsed :: Double -> Text
formatElapsed s
  | s < 60 = T.pack (show (truncate s :: Integer)) <> "s"
  | s < 3600 = T.pack (show (floor (s / 60) :: Integer)) <> "m"
  | otherwise =
      let h = floor (s / 3600) :: Integer
          m = floor ((s - fromIntegral h * 3600) / 60) :: Integer
       in T.pack (show h) <> "h" <> T.pack (show m) <> "m"

statusIndicator :: Status -> Text
statusIndicator = \case
  Complete -> "● done"
  Running -> "● active"
  Queued -> "○ queued"
  Failed -> "● failed"
  Cancelled -> "● cancel"

elapsedOf :: Double -> TaskView -> Text
elapsedOf now v = case tvStart v of
  Nothing -> "—"
  Just s -> formatElapsed (maybe now id (tvEnd v) - s)

projectOf :: TaskView -> Text
projectOf v
  | not (T.null (tiProjectName (tvInfo v))) = tiProjectName (tvInfo v)
  | not (null (tiProjectRoot (tvInfo v))) = T.pack (reverse (takeWhile (/= '/') (reverse (tiProjectRoot (tvInfo v)))))
  | otherwise = "—"

columns :: [[Text]] -> [Text]
columns rows =
  let widths = map (maximum . map T.length) (transpose rows)
   in [T.stripEnd (T.intercalate "  " (zipWith (\w c -> T.justifyLeft w ' ' c) widths r)) | r <- rows]

-- | The task table, as at @/swarm@. @now@ is seconds since the epoch.
renderTable :: Double -> [TaskView] -> [Text]
renderTable now views =
  "Fisher Workflow Swarm" :
  columns
    ( ["ID", "Workflow", "Project", "Status", "Time", "Lean", "Progress"]
    : [ [ tiTaskId i
        , let w = if T.null (tiWorkflowKind i) then "workflow" else tiWorkflowKind i
           in T.take 26 (if T.null (tiDescription i) then w else w <> ": " <> tiDescription i)
        , T.take 18 (projectOf v)
        , statusIndicator (tvStatus v)
        , elapsedOf now v
        , maybe "—" id (pLean (tvProgress v))
        , pProgress (tvProgress v) <> (if tvHasPty v && tvStatus v == Running then " › attach" else "") ]
      | v <- views, let i = tvInfo v ] )

renderDetail :: Double -> TaskView -> [Text]
renderDetail now v =
  let i = tvInfo v
      p = tvProgress v
      row k val = [(k, val)]
      opt k val = if T.null val then [] else row k val
   in ("Task " <> tiTaskId i) :
      columns
        [ [k, val]
        | (k, val) <-
            row "ID" (tiTaskId i) <> row "Description" (tiDescription i) <> row "Theorem" (tiTheorem i)
              <> opt "Workflow" (tiWorkflowKind i) <> opt "Command" (tiWorkflowCommand i) <> opt "Backend" (tiBackendName i)
              <> (if not (T.null (tiProjectName i)) || not (null (tiProjectRoot i)) then row "Project" (projectOf v) else [])
              <> opt "Project Root" (T.pack (tiProjectRoot i)) <> opt "Working Dir" (T.pack (tiWorkingDir i))
              <> row "Status" (statusIndicator (tvStatus v)) <> row "Elapsed" (elapsedOf now v)
              <> row "Lean" (maybe "—" id (pLean p)) <> row "Progress" (pProgress p)
              <> opt "Session" (tiSessionId i)
              <> (if tvHasPty v && tvStatus v == Running then row "Attach" ("/swarm attach " <> tiTaskId i <> "  (Ctrl-] to detach)") else [])
              <> maybe [] (row "Result") (pResult p) <> maybe [] (row "Error") (pError p) ]

counts :: [TaskView] -> Map.Map Status Int
counts vs = Map.fromListWith (+) [(tvStatus v, 1) | v <- vs]

-- | @Swarm: 2 agents running · 1 complete@, or nothing.
summaryLine :: [TaskView] -> Maybe Text
summaryLine vs =
  let c = counts vs
      n s = Map.findWithDefault 0 s c
      parts =
        [T.pack (show (n Running)) <> (if n Running == 1 then " agent running" else " agents running") | n Running > 0]
          <> [T.pack (show (n Complete)) <> " complete" | n Complete > 0]
          <> [T.pack (show (n Queued)) <> " queued" | n Queued > 0]
          <> [T.pack (show (n Failed)) <> " failed" | n Failed > 0]
   in if null parts then Nothing else Just ("Swarm: " <> T.intercalate " · " parts)

-- | @af:running/total@ while anything runs, @af:total@ otherwise.
statusBarFragment :: [TaskView] -> Maybe Text
statusBarFragment vs
  | null vs = Nothing
  | running > 0 = Just ("af:" <> T.pack (show running) <> "/" <> T.pack (show (length vs)))
  | otherwise = Just ("af:" <> T.pack (show (length vs)))
  where
    running = Map.findWithDefault 0 Running (counts vs)
