import Data.Ratio ((%))
import System.Posix.Unistd

import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run

myManageHook = composeOne
               [ resource =? "Do" -?> doIgnore
               , className =? "Xmessage" -?> doCenterFloat
               , isFullscreen -?> doFullFloat
               ]

myLayoutHook = smartBorders $ toggleLayouts Full $ layouts
    where
        layouts = mouseResizableTile ||| mouseResizableTileMirrored ||| (im $ Grid)
        im = withIM (1%6) (Or (Or (Title "Buddy List") (Title "Contact List")) (ClassName "psi"))

myXPConfig = amberXPConfig
             { borderColor = "#222222"
             , showCompletionOnTab = True
             }

myKeys = [ ("M-S-l", spawn "gnome-screensaver-command -l")
         -- Cycling between workspaces
         , ("M-[", moveTo Prev HiddenWS)
         , ("M-]", moveTo Next HiddenWS)
         , ("M-S-[", shiftTo Prev HiddenWS)
         , ("M-S-]", shiftTo Next HiddenWS)
         -- Resize slave windows
         , ("M-u", sendMessage $ ShrinkSlave)
         , ("M-i", sendMessage $ ExpandSlave)
         -- Toggle fullscreen
         , ("M-f", sendMessage $ ToggleLayout)
         -- Focus window with urgency flag
         , ("M-0", focusUrgent)
         -- Switch to previous workspace
         , ("M-o", toggleWS)
         -- Shell prompt
         , ("M-p", shellPrompt myXPConfig)
         , ("M-S-p", spawn "gmrun")
         --, ("M-w", windowPromptGoto myXPConfig)
         --, ("M-S-w", windowPromptBring myXPConfig)
         -- Terminals
         , ("M-x", spawn "xterm")
         , ("M-S-x", spawn "gnome-terminal")
         -- Multimedia keys
         , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 2dB-")
         , ("<XF86AudioMute>" , spawn "amixer -q set Master toggle")
         , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 2dB+")
         , ("<XF86Display>", spawn "xrandr --auto")
         ]
         ++
         -- Use view instead of greedyView for all workspaces
         [ (otherModMasks ++ "M-" ++ [key], action tag)
         | (tag, key)  <- zip ["1","2","3","4","5","6","7","8","9"] "123456789"
         , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                      , ("S-", windows . W.shift)]
    ]

myLogHook proc = dynamicLogWithPP $ defaultPP
                 { ppOutput = hPutStrLn proc
                 , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                 , ppTitle = xmobarColor "green" "" . shorten 50
                 , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                 , ppSep = " | "
                 }

main = do
    xmobarProc <- spawnPipe "xmobar"
    xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ gnomeConfig
        { modMask = mod4Mask
        , terminal = "xterm"
        , borderWidth = 1
        , normalBorderColor = "#222222"
        , focusedBorderColor = "#3388aa"
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , layoutHook = desktopLayoutModifiers myLayoutHook
        , logHook = myLogHook xmobarProc >> updatePointer (Relative (95/100) (95/100))
        }
        `additionalKeysP` myKeys
