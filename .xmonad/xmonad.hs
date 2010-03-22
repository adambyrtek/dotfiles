import Data.Ratio ((%))
import System.Posix.Unistd

import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run

myManageHook = composeAll [ manageDocks
               , resource =? "Do" --> doIgnore
               , className =? "Xmessage" --> doFloat
               , className =? "Gimp" --> doFloat
               ]

myLayout = avoidStruts $ smartBorders $ (im $ tiled ||| Mirror tiled ||| Grid) ||| Full
    where 
        tiled = ResizableTall 1 (3/100) (1/2) [1]
        im = withIM (1%6) (Title "Buddy List") 

myKeys = [ ("M-S-l", spawn "gnome-screensaver-command -l")
         , ("M-p", spawn "gmrun")
         -- Cycling between workspaces
         , ("M-[", prevWS)
         , ("M-]", nextWS)
         , ("M-S-[", shiftToPrev)
         , ("M-S-]", shiftToNext)
         -- Resize resizable layouts
         , ("M-C-k", sendMessage $ MirrorExpand)
         , ("M-C-j", sendMessage $ MirrorShrink)
         , ("M-C-h", sendMessage $ Shrink)
         , ("M-C-l", sendMessage $ Expand)
         -- Maximize
         , ("M-f", sendMessage $ JumpToLayout "Full")
         -- Focus window with urgency flag
         , ("M-u", focusUrgent)
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
    host <- fmap nodeName getSystemID
    xmobarProc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook gnomeConfig
        { modMask = mod4Mask
        --, terminal = "xterm"
        , focusedBorderColor = "#ffff00"
        , manageHook = myManageHook <+> manageHook gnomeConfig 
        , layoutHook = myLayout
        , logHook = myLogHook xmobarProc
        }
        `additionalKeysP` myKeys
