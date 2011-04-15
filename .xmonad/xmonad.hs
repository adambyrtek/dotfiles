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
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run

myManageHook = composeOne
    [ appName =? "Do" -?> doIgnore
    , appName =? "xmessage" -?> doCenterFloat
    , appName =? "gcalctool" -?> doCenterFloat
    , title =? "Chromium Options" -?> doCenterFloat
    , title =? "Google Chrome Preferences" -?> doCenterFloat
    , appName =? "update-manager" -?> doCenterFloat
    , isFullscreen -?> doFullFloat
    ]

myLayoutHook = smartBorders $ toggleLayouts Full $ layouts
    where
        layouts = mouseResizableTile ||| mouseResizableTileMirrored ||| (im $ Grid)
        im = withIM (1%6)
            (Or (Title "Buddy List") (Title "Contact List"))

myXPConfig = defaultXPConfig
    { font = "xft:Droid Sans Mono-10"
    , height = 20
    , fgColor = "#eee8d5" -- Solarized base2
    , bgColor = "#073642" -- Solarized base02
    , fgHLight = "#073642" -- Solarized base02
    , bgHLight = "#cb4b16" -- Solarized orange
    , promptBorderWidth = 0
    , showCompletionOnTab = True
    , position = Top
    }

myKeys conf =
    -- GNOME screen lock
    [ ("M-S-l", spawn "gnome-screensaver-command -l")
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
    , ("M-\\", focusUrgent)
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

myLogHook proc = dynamicLogWithPP $ defaultPP
    -- xmobar log hook, not used anymore
    { ppOutput = hPutStrLn proc
    , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
    , ppTitle = xmobarColor "green" "" . shorten 50
    , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
    , ppSep = " | "
    }

{-data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)-}

{-instance UrgencyHook LibNotifyUrgencyHook where-}
    {-urgencyHook LibNotifyUrgencyHook w = do-}
        {-name <- getName w-}
        {-ws <- gets windowset-}
        {-whenJust (W.findTag w ws) (flash name)-}
      {-where flash name index =-}
                {-safeSpawn "notify-send" (show name ++ " requests attention on workspace " ++ index)-}

myConfig = ewmh $ withUrgencyHook NoUrgencyHook $ gnomeConfig
    { modMask = mod4Mask
    , terminal = "xterm"
    , borderWidth = 2
    , normalBorderColor = "#002b36" -- Solarized base03
    , focusedBorderColor = "#859900" -- Solarized green
    , manageHook = myManageHook <+> manageHook gnomeConfig
    , layoutHook = desktopLayoutModifiers myLayoutHook
    }
    `additionalKeysP` myKeys myConfig
    `additionalMouseBindings` myMouse myConfig

main = do
    xmonad $ myConfig
