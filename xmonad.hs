import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Main function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Key binding to toggle the gap for the bar
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Command to launch the bar
myBar = "xmobar"

-- Custom PP, determines what is being written to the bar
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- ModMask
myModMask = mod4Mask

-- Terminal
myTerminal = "urxvt"

-- Borders
myBorderWidth = 1
myFocusedBorderColor = "#0088CC"

-- Keybindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- %! Launch terminal
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)

    -- %! Close the focused window
    , ((modMask,               xK_q     ), kill)

    -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- %! Quit xmonad
    --, ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- %! Restart xmonad
    , ((modMask .|. shiftMask, xK_q     ), spawn "xmonad --recompile && xmonad --restart")
    ]

-- Manage hook
myManageHook = composeAll
    [ role =? "Preferences" --> doFloat
    , role =? "pop-up" --> doFloat
    , className =? "Pidgin" --> doFloat
    , className =? "Lxappereance" --> doFloat
    , className =? "Skype" --> doFloat
    , className =? "Vlc" --> doFloat
    , manageDocks
    ]
    where role = stringProperty "WM_WINDOW_ROLE"

-- Main config
myConfig = defaultConfig { terminal = myTerminal
                           , modMask = myModMask
                           , borderWidth = myBorderWidth
                           , focusedBorderColor = myFocusedBorderColor
                           , manageHook=myManageHook <+> manageHook defaultConfig
                           , layoutHook=avoidStruts $ layoutHook defaultConfig
                           , keys = \c -> myKeys c `M.union` keys defaultConfig c
                           }
