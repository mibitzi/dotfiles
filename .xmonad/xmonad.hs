import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Util.Run
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt
import XMonad.Layout.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.StackSet
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops

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
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#0088CC"

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int] ++ ["0", "'", "^"]

-- Browser
myBrowser = "google-chrome-stable"
myJiraSearchEngine = S.searchEngine "Jira" "http://jira.futuretek.ch/browse/"
myPhpSearchEngine = S.searchEngine "PHP" "http://php.net/"

mySearchEngineMap method = M.fromList $
    [ ((0, xK_g), method S.google)
    , ((0, xK_j), method myJiraSearchEngine)
    , ((0, xK_p), method myPhpSearchEngine)
    ]

-- Keybindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Launch terminal
    [ ((modMask,               xK_Return), spawnHere $ XMonad.terminal conf)

    -- Close the focused window
    , ((modMask,               xK_q     ), kill)
    , ((modMask .|. shiftMask, xK_c     ), return ())

    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Quit xmonad
    --, ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_q     ), spawn "xmonad --recompile && xmonad --restart")

    -- Prev/Next workspace
    , ((modMask,               xK_i     ), moveTo Prev NonEmptyWS)
    , ((modMask,               xK_o     ), moveTo Next NonEmptyWS)

    -- Rename workspace
    , ((modMask,               xK_n     ), renameWorkspace defaultXPConfig)

    -- Media keys
    , ((0, xF86XK_AudioLowerVolume      ), spawn "pulseaudio-ctl down 2")
    , ((0, xF86XK_AudioRaiseVolume      ), spawn "pulseaudio-ctl up 2")
    , ((0, xF86XK_AudioMute             ), spawn "pulseaudio-ctl mute")

    -- xF86XK_AudioMicMute
    , ((0, 0x1008FFB2                   ), spawn "pulseaudio-ctl mute-input")

    , ((0, xF86XK_MonBrightnessDown     ), spawn "xblacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp       ), spawn "xblacklight -inc 10")

    , ((modMask, xK_b                   ), sendMessage ToggleStruts)

    -- Search commands
    , ((modMask, xK_s), SM.submap $ mySearchEngineMap $ S.promptSearchBrowser defaultXPConfig myBrowser)
    , ((modMask .|. shiftMask, xK_s), SM.submap $ mySearchEngineMap $ S.selectSearchBrowser myBrowser)
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

-- Layout hook
myLayout = smartBorders . avoidStruts $ tall ||| Mirror tall ||| noBorders Full
    where tall = Tall 1 (3/100) (1/2)

-- Manage hook
myManageHook = composeAll
    [ manageDocks
    , isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    , role =? "Preferences" --> doFloat
    , className =? "Pidgin" --> doFloat
    , className =? "Lxappereance" --> doFloat
    , className =? "Skype" --> doFloat
    , className =? "Vlc" --> doFloat
    , title =? "Volume Control" --> doFloat
    , placeHook $ smart (0.5, 0.5)
    -- , insertPosition End Newer
    ]
    where role = stringProperty "WM_WINDOW_ROLE"

-- Log hook
myLogHook handle = workspaceNamesPP defaultPP
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
    xmonad $ ewmh defaultConfig { terminal = myTerminal
                           , modMask = myModMask
                           , borderWidth = myBorderWidth
                           , normalBorderColor = myNormalBorderColor
                           , focusedBorderColor = myFocusedBorderColor
                           , XMonad.workspaces = myWorkspaces
                           , manageHook=myManageHook <+> manageHook defaultConfig
                           , layoutHook=myLayout
                           , logHook = myLogHook xmobarPipe
                           , keys = \c -> myKeys c `M.union` keys defaultConfig c
                           }
