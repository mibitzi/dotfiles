import XMonad
import Data.Default
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Util.Run
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.StackSet
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops
import XMonad.Prompt
import XMonad.Prompt.Shell

import System.Exit

import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- ModMask
myModMask = mod4Mask

-- Terminal
myTerminal = "termite"

-- Borders
myBorderWidth = 1
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#0088CC"

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int] ++ ["0", "'", "^"]

-- Browser
myBrowser = "google-chrome-stable"

-- Keybindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Launch terminal
    [ ((modMask,               xK_Return), spawnHere $ XMonad.terminal conf)

    -- Close the focused window
    , ((modMask,               xK_q     ), kill)

    -- Rotate through the available layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)

    -- Move focus
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- floating layer support
    , ((modMask .|. shiftMask, xK_space ), withFocused $ windows . W.sink)

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- quit/restart xmonad
    --, ((modMask .|. controlMask, xK_q   ), io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask, xK_q     ), spawn "xmonad --recompile && xmonad --restart")

    -- Rename workspace
    , ((modMask,               xK_n     ), renameWorkspace def)

    -- Shell prompt
    , ((modMask,               xK_p     ), shellPrompt def)

    -- Media keys
    , ((0, xF86XK_AudioLowerVolume      ), spawn "pulseaudio-ctl down 2")
    , ((0, xF86XK_AudioRaiseVolume      ), spawn "pulseaudio-ctl up 2")
    , ((0, xF86XK_AudioMute             ), spawn "pulseaudio-ctl mute")

    -- xF86XK_AudioMicMute
    , ((0, 0x1008FFB2                   ), spawn "pulseaudio-ctl mute-input")

    , ((0, xF86XK_MonBrightnessDown     ), spawn "xblacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp       ), spawn "xblacklight -inc 10")

    -- toggle xmobar
    , ((modMask, xK_b                   ), sendMessage ToggleStruts)

    -- Search
    , ((modMask, xK_s), S.promptSearchBrowser def myBrowser S.google)
    ]
    ++
    -- Workspaces
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1
                                                 , xK_2
                                                 , xK_3
                                                 , xK_4
                                                 , xK_5
                                                 , xK_6
                                                 , xK_7
                                                 , xK_8
                                                 , xK_9
                                                 , xK_0
                                                 , xK_apostrophe
                                                 , xK_asciicircum]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_r, xK_w, xK_t] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Layout hook
myLayout = smartBorders . avoidStruts . smartSpacing 3 $ tall ||| noBorders Full
    where tall = Tall 1 (3/100) (1/2)

-- Manage hook
myManageHook = manageDocks <+> composeOne
    [ isFullscreen -?> doFullFloat
    , isDialog -?> doCenterFloat
    , role =? "Preferences" -?> doFloat
    , className =? "Pidgin" -?> doFloat
    , className =? "Lxappereance" -?> doCenterFloat
    , className =? "Skype" -?> doFloat
    , className =? "Vlc" -?> doCenterFloat
    , className =? "RoboSim" -?> doCenterFloat
    , title =? "Volume Control" -?> doCenterFloat
    --, placeHook $ smart (0.5, 0.5)
    , transience
    , pure True -?> insertPosition Above Newer
    ]
    where role = stringProperty "WM_WINDOW_ROLE"

-- Log hook
myLogHook handle = workspaceNamesPP def
    { ppOutput = hPutStrLn handle
    , ppCurrent = \wsID -> "<fc=#FFAF00>[" ++ wsID ++ "]</fc>"
    , ppUrgent = \wsID -> "<fc=#FF0000>" ++ wsID ++ "</fc>"
    , ppSep = " | "
    , ppTitle = \wTitle -> "<fc=#92FF00>" ++ shorten 50 wTitle ++ "</fc>"
    } >>= dynamicLogWithPP

-- Main config
main :: IO()
main = do
    xmobarPipe <- spawnPipe "xmobar"
    xmonad $ docks def { terminal = myTerminal
                           , modMask = myModMask
                           , font = "xft:SourceCodeProSemibold-8"
                           , borderWidth = myBorderWidth
                           , normalBorderColor = myNormalBorderColor
                           , focusedBorderColor = myFocusedBorderColor
                           , XMonad.workspaces = myWorkspaces
                           , manageHook=myManageHook <+> manageHook def
                           , layoutHook=myLayout
                           , logHook = myLogHook xmobarPipe
                           , keys = \c -> myKeys c
                           }
