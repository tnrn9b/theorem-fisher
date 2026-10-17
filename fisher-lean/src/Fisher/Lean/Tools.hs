-- | The Lean tools C1 ports from lean-lsp-mcp 0.31.0, by name.
module Fisher.Lean.Tools
  ( leanTools
  , findTool
  , callTool
  ) where

import Data.List (find)
import Data.Text (Text)

import Fisher.Lean.Session (Session)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Analysis
import Fisher.Lean.Tools.Build
import Fisher.Lean.Tools.Diagnostics
import Fisher.Lean.Tools.Goals
import Fisher.Lean.Tools.Navigation
import Fisher.Lean.Tools.Outline
import Fisher.Lean.Tools.Profile
import Fisher.Lean.Tools.Search
import Fisher.Py.Value (PyVal)

leanTools :: [Tool]
leanTools =
  [ goalTool, termGoalTool, diagnosticsTool, codeActionsTool, outlineTool
  , hoverTool, completionsTool, declarationFileTool, referencesTool
  , widgetsTool, widgetSourceTool
  , multiAttemptTool, runCodeTool, verifyTool, minimalHypothesesTool
  , profileTool, buildTool
  , localSearchTool, leanSearchTool, loogleTool, leanFinderTool, hammerPremiseTool ]

findTool :: Text -> Maybe Tool
findTool n = find ((== n) . toolName) leanTools

callTool :: Session -> Text -> PyVal -> IO (Maybe ToolResult)
callTool s n a = maybe (pure Nothing) (\t -> Just <$> runTool t s a) (findTool n)
