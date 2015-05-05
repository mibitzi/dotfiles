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
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
import XMonad.Layout.Circle
import XMonad.Layout.Tabbed
import XMonad.Util.Themes
import XMonad.Actions.SpawnOn

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

    -- SubLayouts
    , ((modMask .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modMask .|. controlMask, xK_l), sendMessage $ pullGroup R)
    , ((modMask .|. controlMask, xK_k), sendMessage $ pullGroup U)
    , ((modMask .|. controlMask, xK_j), sendMessage $ pullGroup D)

    , ((modMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    , ((modMask .|. controlMask, xK_period), onGroup W.focusUp')
    , ((modMask .|. controlMask, xK_comma), onGroup W.focusDown')

    , ((modMask, xK_s), SM.submap $ defaultSublMap conf)

    -- Quit xmonad
    --, ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_q     ), spawn "xmonad --recompile && xmonad --restart")

    -- Rename workspace
    , ((modMask,               xK_n     ), renameWorkspace defaultXPConfig)

    -- Search commands
    --, ((modMask, xK_s), SM.submap $ mySearchEngineMap $ S.promptSearchBrowser defaultXPConfig myBrowser)
    --, ((modMask .|. shiftMask, xK_s), SM.submap $ mySearchEngineMap $ S.selectSearchBrowser myBrowser)
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
myLayout = avoidStruts $ windowNavigation $ boringWindows $ subLayout [] row $ row ||| Full
  where row = Tall 1 (3/100) (1/2)

-- Manage hook
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    , role =? "Preferences" --> doFloat
    , className =? "Pidgin" --> doFloat
    , className =? "Lxappereance" --> doFloat
    , className =? "Skype" --> doFloat
    , className =? "Vlc" --> doFloat
    , title =? "Cocos2dx-Linux" --> doFloat
    , manageDocks
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
    xmonad $ defaultConfig { terminal = myTerminal
                           , modMask = myModMask
                           , borderWidth = myBorderWidth
                           , normalBorderColor = myNormalBorderColor
                           , focusedBorderColor = myFocusedBorderColor
                           , workspaces = myWorkspaces
                           , manageHook=myManageHook <+> manageHook defaultConfig
                           , layoutHook=myLayout
                           , logHook = myLogHook xmobarPipe
                           , keys = \c -> myKeys c `M.union` keys defaultConfig c
                           }
