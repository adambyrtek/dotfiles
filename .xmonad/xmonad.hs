import Data.Ratio ((%))

import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run

myManageHook = composeOne
    [ appName =? "xmessage" -?> doCenterFloat
    , appName =? "update-manager" -?> doCenterFloat
    , appName =? "guake" -?> doFloat
    , className =? "Xfce4-notifyd" -?> doF W.focusDown
    , isFullscreen -?> doFullFloat
    ]

myLayoutHook = smartSpacing 10 $ smartBorders (toggleLayouts Full $ avoidStruts layouts)
    where
        layouts = mouseResizableTile ||| mouseResizableTileMirrored ||| (im $ Grid)
        im = withIM (1%6)
            (Or (Title "Buddy List") (Title "Contact List"))

myXPConfig = defaultXPConfig
    { font = "xft:Droid Sans Mono-10"
    , height = 20
    , fgColor = "#eee8d5" -- Solarized base2
    , bgColor = "#073642" -- Solarized base02
    , fgHLight = "#cb4b16" -- Solarized orange
    , bgHLight = "#073642" -- Solarized base02
    , promptBorderWidth = 0
    , showCompletionOnTab = True
    , position = Top
    }

myKeys conf =
    -- Cycle between workspaces
    [ ("M-[", moveTo Prev HiddenWS)
    , ("M-]", moveTo Next HiddenWS)
    , ("M-S-[", shiftTo Prev HiddenWS)
    , ("M-S-]", shiftTo Next HiddenWS)
    -- Toggle fullscreen
    , ("M-f", sendMessage $ ToggleLayout)
    -- Focus window with urgency flag
    , ("M-u", focusUrgent)
    -- Switch to previous workspace
    , ("M-o", toggleWS)
    -- Prompts
    , ("M-p", shellPrompt myXPConfig)
    , ("M-S-p", spawn "xfce4-appfinder")
    -- GNOME screen lock
    , ("M-S-l", spawn "gnome-screensaver-command -l")
    ]
    ++
    -- Use view instead of greedyView for all workspaces
    [ (modMasks ++ [key], action tag)
    | (tag, key) <- zip (workspaces conf) "123456789"
    , (modMasks, action) <- [ ("M-", windows . W.view) -- was W.greedyView
                            , ("M-S-", windows . W.shift) ]
    ]

myMouse conf =
    -- Switch workspaces with a scroll wheel
    [ ((modMask conf, button4), \_ -> moveTo Prev HiddenWS)
    , ((modMask conf, button5), \_ -> moveTo Next HiddenWS)
    ]

myConfig = ewmh $ withUrgencyHookC NoUrgencyHook urgencyConfig { suppressWhen = Focused }
                $ desktopConfig
    { modMask = mod4Mask
    , terminal = "gnome-terminal"
    , borderWidth = 2
    , normalBorderColor = "#002b36" -- Solarized base03
    , focusedBorderColor = "#859900" -- Solarized green
    , manageHook = insertPosition End Newer <+> myManageHook <+> manageHook desktopConfig
    , layoutHook = myLayoutHook
    , logHook = logHook desktopConfig
    , startupHook = startupHook desktopConfig
    , focusFollowsMouse = False
    }
    `additionalKeysP` myKeys myConfig
    `additionalMouseBindings` myMouse myConfig

main = do
    xmonad $ myConfig
