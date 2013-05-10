import Data.Ratio ((%))
import System.IO

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

bgDark = "#2b2b2b"
faded  = "#dfaf8f"
orange = "#ffa500"

statusBarFont = "Source Code Pro-9:semibold"

statusBarCmd  = 
    "conky | dzen2 -e - -x 1420 -h 24 -w 500 -ta r -fg '" ++
    orange  ++
    "' -bg '" ++
    bgDark  ++
    "' -fn '" ++ statusBarFont ++ "'"

switcherBarCmd = 
    "dzen2 -h 24 -w 1000 -ta l -fg '" ++
    orange  ++
    "' -bg '" ++
    bgDark  ++
   "' -fn '" ++ statusBarFont ++ "'"

spotifyBarCmd = "/home/jan/bin/dzspotify | dzen2 -x 1000 -h 24 -w 420 -ta c -fg '" ++
    orange  ++
    "' -bg '" ++
    bgDark  ++
    "' -fn '" ++ statusBarFont ++ "'"

spaces = ["dev", "term", "web", "chat", "music"]

main = do
     leftBarPipe   <- spawnPipe switcherBarCmd
     rightBarPipe  <- spawnPipe statusBarCmd
     centerBarPipe <- spawn spotifyBarCmd
     xmonad $ gnomeConfig {
              modMask            = mod4Mask
            , normalBorderColor  = faded
            , focusedBorderColor = orange
            , workspaces         = spaces
            , manageHook         = applicationRules <+> manageDocks
            , layoutHook         = layout
            , logHook            = logHandler leftBarPipe
     } `additionalKeys` keyOverrides

-- application rules

applicationRules = composeAll [
          resource  =? "notify-osd" --> doIgnore
        , className =? "Emacs"      --> doF(W.shift "dev")
        , className =? "Pidgin"     --> doF(W.shift "chat")
        , className =? "Empathy"    --> doF(W.shift "chat")
        , className =? "Spotify"    --> doF(W.shift "music")
    ]

tiledLayout   = ResizableTall 1 0.03 0.75 []
defaultLayout = avoidStruts $ (tiledLayout ||| Full)
chatLayout    = reflectHoriz $ withIM (2%12) (Or (ClassName "Pidgin") (ClassName "Empathy")) Grid ||| Full

layout = avoidStruts $ onWorkspace "chat" chatLayout $ defaultLayout

logHandler bar = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           = dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           = dzenColor "white" "#1B1D1E" . pad
      , ppHidden            = dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   = dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            = dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             = " "
      , ppSep               = "  |  "
      , ppTitle             = (" " ++) . dzenColor "white" bgDark . dzenEscape
      , ppOutput            = hPutStrLn bar
    }

applicationMenu :: XPConfig
applicationMenu = 
  defaultXPConfig {
      font                  = "xft: " ++ statusBarFont
    , bgColor               = bgDark
    , fgColor               = orange
    , bgHLight              = orange
    , fgHLight              = bgDark
    , promptBorderWidth     = 0
    , height                = 24
    , historyFilter         = deleteConsecutive
    }

keyOverrides =
  [ ((mod4Mask,               xK_r), runOrRaisePrompt applicationMenu)
  , ((mod4Mask,               xK_q), spawn "/usr/bin/xmonad --recompile && /usr/bin/xmonad --restart")
  , ((mod4Mask .|. shiftMask, xK_q), spawn "gnome-session-quit --logout")]
