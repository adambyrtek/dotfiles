import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Util.EZConfig

myManageHook :: [ManageHook]
myManageHook =
    [ resource =? "Do" --> doIgnore ]

main = xmonad $ gnomeConfig
    { modMask = mod4Mask
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    }
    `additionalKeysP`
        [ ("M-S-l", spawn "gnome-screensaver-command -l")
        , ("M-p", spawn "gmrun")
        , ("M-[", prevWS)
        , ("M-]", nextWS)
        , ("M-S-[", shiftToPrev)
        , ("M-S-]", shiftToNext)
        ]
