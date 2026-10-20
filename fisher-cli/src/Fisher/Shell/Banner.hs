-- | The welcome banner (upstream @cli/banner.py@'s @build_welcome_banner@ and
-- @cli.py@'s @_build_compact_banner@), drawn with Fisher.Rich so it lays
-- out as upstream's does at every width.
module Fisher.Shell.Banner
  ( BannerInput (..)
  , welcomeBanner
  , compactBanner
  , shortenMiddle
  , chatLines
  ) where

import Data.Char (isAscii)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Rich.Render
import Fisher.Rich.Style (parseStyle)
import Fisher.Rich.Text
import Fisher.Shell.Data
import Fisher.Skin

data BannerInput = BannerInput
  { biWidth :: Int
  , biUnicode :: Bool
  , biSkin :: Skin
  , biModel :: Text
  , biCwd :: Text
  , biProject :: Maybe Text
  , biSession :: Maybe Text
  , biSwarmLine :: Maybe Text
  , biBehind :: Maybe Int
  }

-- | @_shorten_middle@.
shortenMiddle :: Text -> Int -> Text
shortenMiddle t maxLen
  | maxLen <= 0 || T.length t <= maxLen = t
  | maxLen <= 7 = T.take maxLen t
  | otherwise =
      let headN = max 2 ((maxLen - 3) `div` 2)
          tailN = max 2 (maxLen - headN - 3)
       in T.take headN t <> "..." <> T.takeEnd tailN t

-- | @_markup_is_ascii@: the text outside @[...]@ tags is ASCII.
markupIsAscii :: Text -> Bool
markupIsAscii = T.all isAscii . stripTags
  where
    stripTags s = case T.breakOn "[" s of
      (a, b) | T.null b -> a
      (a, b) -> case T.breakOn "]" (T.drop 1 b) of
        (_, c) | T.null c -> a <> b
        (_, c) -> a <> stripTags (T.drop 1 c)

-- | @_select_banner_art@: (split layout?, logo, hero).
selectArt :: Int -> Skin -> Bool -> (Bool, Text, Text)
selectArt width skin unicode =
  let pick custom fallback = if not (T.null custom) && (unicode || markupIsAscii custom) then custom else fallback
      fullLogo = pick (skBannerLogo skin) (bannerArt (if unicode then "logo" else "logo_ascii"))
      compactLogo = pick (skBannerLogoCompact skin) (bannerArt (if unicode then "compact" else "compact_ascii"))
      fullHero = pick (skBannerHero skin) (bannerArt (if unicode then "hero" else "hero_ascii"))
      compactHero = pick (skBannerHeroCompact skin) ""
   in if width >= 124
        then (True, fullLogo, fullHero)
        else if width >= 88
          then (False, fullLogo, if T.null fullHero then compactHero else fullHero)
          else if width >= 34 then (False, compactLogo, "") else (False, "", "")

-- | The banner's lines, as @ChatConsole@ prints them.
welcomeBanner :: BannerInput -> [Line]
welcomeBanner bi =
  let w = biWidth bi
      skin = biSkin bi
      simplified = w < 72
      uni = biUnicode bi
      sep = if uni then "·" else "|"
      dash = if uni then "—" else "-"
      warn = if uni then "⚠" else "!"
      accent = skinColor skin "banner_accent" "#FFBF00"
      dimC = skinColor skin "banner_dim" "#B8860B"
      textC = skinColor skin "banner_text" "#FFF8DC"
      sessionC = skinColor skin "session_border" "#8B8682"
      (split, logo, hero) = selectArt w skin uni
      dimT t = "[dim " <> dimC <> "]" <> t <> "[/]"
      modelShort0 = if "/" `T.isInfixOf` biModel bi then last (T.splitOn "/" (biModel bi)) else biModel bi
      modelMax = if simplified then 18 else 28
      modelShort = if T.length modelShort0 > modelMax then T.take (modelMax - 3) modelShort0 <> "..." else modelShort0
      left =
        [hero | not (T.null hero)] <> ["" | not (T.null hero)]
          <> ( if simplified
                 then ["[" <> accent <> "]" <> modelShort <> "[/] " <> dimT sep <> " " <> dimT "Math Inc.", dimT (shortenMiddle (biCwd bi) (max 18 (w - 10)))]
                 else ["[" <> accent <> "]" <> modelShort <> "[/] " <> dimT sep <> " " <> dimT "Math Inc.", dimT (biCwd bi)] )
          <> maybe [] (\p -> [dimT ("Project: " <> p)]) (biProject bi)
          <> catMaybes [fmap (\s -> "[dim " <> sessionC <> "]Session: " <> s <> "[/]") (if simplified then Nothing else biSession bi)]
      item c d = "[" <> textC <> "]`" <> c <> "`[/] " <> dimT d
      items
        | simplified =
            [ item "/chat" "turn on onboarding mode and see the first steps", item "/project" "select or create a Fisher project"
            , item "/prove" "guided Lean workflow", item "/review" "review, checkpoint, refactor, golf"
            , item "/draft" "draft Lean declaration skeletons", item "/autoprove" "autonomous Lean workflow"
            , item "/formalize" "interactive draft plus prove", item "/autoformalize" "autonomous draft plus autoprove"
            , item "/swarm" "track, attach, or cancel workflow agents", item "/help" "commands and diagnostics" ]
        | otherwise =
            [ item "/chat" (dash <> " turn on onboarding mode and show the first steps")
            , item "/project" (dash <> " create, convert, inspect, or switch the active project")
            , item "/prove" (dash <> " spawn a guided managed proving agent")
            , item "/review" (dash <> " review, checkpoint, refactor, or golf Lean proofs")
            , item "/draft" (dash <> " draft Lean declaration skeletons")
            , item "/autoprove" (dash <> " spawn an autonomous managed proving agent")
            , item "/formalize" (dash <> " spawn an interactive managed formalization agent")
            , item "/autoformalize" (dash <> " spawn an autonomous managed formalization agent")
            , item "/swarm" (dash <> " track, attach, or cancel workflow agents")
            , item "/help" (dash <> " session and diagnostics commands")
            , dimT "You can ignore MCP and plugin internals at first; Theorem Fisher stages the default Lean workflow setup for you." ]
      summary = T.intercalate (" " <> sep <> " ")
        ["/chat", "/project", "/prove", "/review", "/draft", "/autoprove", "/formalize", "/autoformalize", "/swarm", "/help", "/help for commands"]
      behind = case biBehind bi of
        Just n | n > 0 ->
          ["[bold yellow]" <> warn <> " " <> T.pack (show n) <> " " <> (if n == 1 then "commit" else "commits") <> " behind[/][dim yellow] "
             <> dash <> " run [bold]fisher update[/bold] to update[/]"]
        _ -> []
      right =
        ["[bold " <> accent <> "]Start Here[/]"] <> items <> maybe [] pure (biSwarmLine bi) <> [""] <> [dimT summary] <> behind
      leftCell = markup (T.intercalate "\n" left)
      rightCell = markup (T.intercalate "\n" right)
      layout
        | split = grid (0, 2, 0, 2) [column "left", column "right"] [[leftCell, rightCell]]
        | otherwise = grid (0, 1, 0, 1) [column "content"] [[leftCell], [markup ""], [rightCell]]
      agent = skinBranding skin "agent_name" "Fisher"
      titleC = skinColor skin "banner_title" "#FFD700"
      borderC = skinColor skin "banner_border" "#CD7F32"
      outer =
        (panel (RTable layout))
          { pTitle = Just ("[bold " <> titleC <> "]" <> agent <> " v" <> upstreamVersion <> " (" <> upstreamReleaseDate <> ")[/]")
          , pBorderStyle = parseStyle borderC
          , pPadding = (0, 2, 0, 2)
          , pBox = if uni then boxRounded else boxAscii }
      printed j r = chatLines (printLines w j r)
   in [[]]
        <> (if T.null logo then [] else printed (Just JCenter) (markup logo) <> [[]])
        <> printed Nothing (RPanel outer)

-- | @_build_compact_banner@, for @display.compact@ and tiny terminals.
compactBanner :: Int -> Bool -> Skin -> [Line]
compactBanner w uni skin =
  let accent = skinColor skin "banner_accent" "#FFBF00"
      dimC = skinColor skin "banner_dim" "#B8860B"
      logo
        | not (T.null (skBannerLogoCompact skin)) = skBannerLogoCompact skin
        | otherwise = "[bold " <> accent <> "]FISHER[/] [dim " <> dimC <> "]" <> (if uni then "·" else "-") <> " Lean Autoformalization[/]"
      body = "\n" <> logo <> "\n[dim " <> dimC <> "]Managed Claude Code Lean handoff[/]\n"
   in chatLines (printLines w Nothing (markup body))

-- | What @ChatConsole.print@ writes: the rendering less its trailing
-- newlines, so at least one (possibly empty) line.
chatLines :: [Line] -> [Line]
chatLines ls = case reverse (dropWhile (T.null . linePlain) (reverse ls)) of
  [] -> [[]]
  ls' -> ls'
