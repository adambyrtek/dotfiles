import Data.Ratio ((%))

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.ManageHook
import XMonad.Util.EZConfig

myManageHook :: [ManageHook]
myManageHook = [ resource =? "Do" --> doIgnore ]

myLayout = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full ||| gridIM (1%7) (ClassName "psi")
    where 
        tiled = Tall nmaster delta ratio
        nmaster = 1
        delta = 3/100
        ratio = 1/2

main = xmonad $ gnomeConfig
    { modMask = mod4Mask
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    , layoutHook = myLayout
    }
    `additionalKeysP`
        [ ("M-S-l", spawn "gnome-screensaver-command -l")
        , ("M-p", spawn "gmrun")
        , ("M-[", prevWS)
        , ("M-]", nextWS)
        , ("M-S-[", shiftToPrev)
        , ("M-S-]", shiftToNext)
        ]
