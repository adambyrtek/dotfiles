import Data.Ratio ((%))
import System.Posix.Unistd

import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.ManageHook
import XMonad.Util.EZConfig
import XMonad.Util.Run

myManageHook = composeAll [ manageDocks
               , resource =? "Do" --> doIgnore
               , className =? "Xmessage" --> doFloat
               , className =? "Gimp" --> doFloat
               ]

myLayout = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| simpleTabbed ||| gridIM (1%6) (Title "Buddy List")
    where 
        tiled = ResizableTall 1 (3/100) (1/2) [1]

myKeys = [ ("M-S-l", spawn "gnome-screensaver-command -l")
         , ("M-p", spawn "gmrun")
         , ("M-[", prevWS)
         , ("M-]", nextWS)
         , ("M-S-[", shiftToPrev)
         , ("M-S-]", shiftToNext)
         , ("M-C-k", sendMessage $ MirrorExpand)
         , ("M-C-j", sendMessage $ MirrorShrink)
         , ("M-C-h", sendMessage $ Shrink)
         , ("M-C-l", sendMessage $ Expand)
         --, ("M-f", sendMessage $ JumpToLayout "Full")
         , ("M-u", focusUrgent)
         ]

main = do
    host <- fmap nodeName getSystemID
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook gnomeConfig
        { modMask = mod4Mask
        --, terminal = "xterm"
        , focusedBorderColor = "#ffff00"
        , manageHook = myManageHook <+> manageHook gnomeConfig 
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP $ defaultPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                        , ppSep = " | "
                        }
        }
        `additionalKeysP` myKeys
