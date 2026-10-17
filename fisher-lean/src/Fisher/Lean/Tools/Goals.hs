-- | @lean_goal@ and @lean_term_goal@ (lean-lsp-mcp @tools/goals.py@).
module Fisher.Lean.Tools.Goals
  ( goalTool
  , termGoalTool
  , goalToStructured
  , lineInFile
  ) where

import Control.Exception (throwIO, try)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Client
import Fisher.Lean.Error
import Fisher.Lean.Session
import Fisher.Lean.Text (splitLines)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common (fileField, tshow)
import Fisher.Py.Value

-- | The file's line (1-based) after syncing it from disk, or the range
-- error.
lineInFile :: Session -> Text -> Int -> IO (Client, [Text], Text)
lineInFile s rel line = do
  c <- getClient s
  openSynced s rel False
  ls <- splitLines <$> content c rel
  if line < 1 || line > length ls
    then toolError ("Line " <> tshow line <> " out of range (file has " <> tshow (length ls) <> " lines)")
    else pure (c, ls, ls !! (line - 1))

goalTool :: Tool
goalTool =
  Tool "lean_goal" "goalArguments"
    [ fileField, ge "1" (field "line" TInt), ge "1" (nullable "column" TInt PNull)
    , opt "format" (TLit ["text", "structured"]) (PStr "text"), ge "1" (nullable "timeout_s" TFloat PNull) ]
    run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      let line = argInt a "line"
      (c, _, ctx) <- lineInFile s rel line
      limit <- maxOutputChars s
      let structured = argText a "format" == "structured"
          render = map (\g -> if structured then goalToStructured g else PStr (boundOutput limit g))
          model goals before after status =
            Model (PDict [("line_context", PStr ctx), ("goals", goals), ("goals_before", before), ("goals_after", after), ("status", status)])
      r <- try (barrier c rel (argMaybeDouble a "timeout_s"))
      case r of
        Left (RequestTimeout _) -> pure (model (PList []) PNull PNull (PStr "still_elaborating"))
        Left e -> throwIO e
        Right () -> case argMaybeInt a "column" of
          Nothing -> do
            let start = maybe 0 id (T.findIndex (not . isSpace) ctx)
            g1 <- goal c rel (line - 1) start False
            g2 <- goal c rel (line - 1) (T.length ctx) False
            let strs gr = if grStatus gr == "goals" then grGoals gr else []
            pure (model PNull (PList (render (strs g1))) (PList (render (strs g2))) PNull)
          Just col -> do
            gr <- goal c rel (line - 1) (col - 1) False
            let status = if grStatus gr == "no_goal" then "no_goal_at_position" else grStatus gr
            pure (model (PList (if grStatus gr == "goals" then render (grGoals gr) else [])) PNull PNull (PStr status))

termGoalTool :: Tool
termGoalTool =
  Tool "lean_term_goal" "term_goalArguments"
    [fileField, ge "1" (field "line" TInt), ge "1" (nullable "column" TInt PNull)]
    run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      let line = argInt a "line"
      (c, _, ctx) <- lineInFile s rel line
      let col = maybe (max (T.length ctx) 1) id (argMaybeInt a "column")
      r <- termGoal c rel (line - 1) (col - 1) True
      let expected = case get "goal" r of
            Just (PStr g) | not (T.null g) -> PStr (T.replace "\n```" "" (T.replace "```lean\n" "" g))
            Just v | truthy v -> PStr (pyStrOf v)
            _ -> PNull
      pure (Model (PDict [("line_context", PStr ctx), ("expected_type", expected)]))

-- | @goal_to_structured(goal)@.
goalToStructured :: Text -> PyVal
goalToStructured g0 =
  let g = T.strip g0
      mk ctx goal' status = PDict [("context", PList ctx), ("goal", goal'), ("status", PStr status), ("pretty", PStr g)]
   in if T.null g || T.toLower g == "no goals"
        then mk [] PNull "complete"
        else case T.breakOn "⊢" g of
          (_, rest) | T.null rest -> mk [] (PStr g) "unknown"
          (ctxText, rest) ->
            let target = T.drop 1 rest
                step (acc, cur) line =
                  let stripped = T.strip line
                   in if T.null stripped
                        then (acc, cur)
                        else
                          if ":" `T.isInfixOf` stripped && not (" " `T.isPrefixOf` line)
                            then
                              let (name, ty) = T.breakOn ":" stripped
                               in (flush acc cur, Just (T.strip name, [T.strip (T.drop 1 ty)]))
                            else case cur of
                              Just (n, tys) -> (acc, Just (n, tys <> [stripped]))
                              Nothing -> (acc, cur)
                flush acc = \case
                  Just (n, tys) -> acc <> [PDict [("name", PStr n), ("type", PStr (T.strip (T.unwords (map T.strip tys))))]]
                  Nothing -> acc
                (entries, lastCur) = foldl step ([], Nothing) (splitLines ctxText)
             in mk (flush entries lastCur) (PStr (T.strip target)) "open"
