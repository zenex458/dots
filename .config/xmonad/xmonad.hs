import System.Exit
import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.SpawnOn
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Layout.ResizableTile
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Tabbed
import XMonad.Layout.SimpleFloat

main :: IO ()
main = xmonad 
    -- . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP )) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask
    , layoutHook = myLayout
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#999999"
--    , startupHook = myStartupHook
--    , workspaces = myWorkspaces
    , manageHook = myManageHook
    , keys = myKeys
    -- , manageHook = namedScratchpadManageHook scratchpads                       
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- terminal
    [ ((modm, xK_Return), spawn "urxvtc -e tmux"),
      -- normal firefox
      ((modm, xK_c), spawn "firefox"),
      ((modm .|. shiftMask, xK_c), spawn "firefox --private-window"),
      -- private firefox
      ((mod1Mask, xK_c), spawn "firefox -P priv"),
      -- next song
      ((mod1Mask, xK_o), spawn "mpc next"),
      -- previous song
      ((mod1Mask, xK_i), spawn "mpc prev"),
      -- play/pause song
      ((mod1Mask, xK_p), spawn "mpc toggle"),
      -- mute/unmute
      ((mod1Mask, xK_m), spawn "amixer sset Master toggle"),
      -- minus vol
      ((mod1Mask, xK_bracketleft), spawn "amixer sset Master 2%-"),
      -- add vol
      ((mod1Mask, xK_bracketright), spawn "amixer sset Master 2%+"),
      -- lock
      ((mod1Mask .|. controlMask, xK_l), spawn "Menu"),
      -- spawn alsamixer
      ((modm, xK_a), spawn "urxvt -e alsamixer -V all"),
      --spawn emacs without the site-file
      ((modm, xK_u), spawn "emacs"),
      -- brightness up by 2
      ((mod1Mask, xK_Prior), spawn "light -A 2"),
      -- brightness down by 2
      ((mod1Mask, xK_Next), spawn "light -U 2"),
      -- bluelight filter
      ((modm .|. shiftMask, xK_r), spawn "redshift -o -c ~/.config/redshift.conf"),
      -- application launcher
      ((modm, xK_p), spawn "rofi -show combi -modes combi -combi-modes 'window,run'"),
--      ((modm, xK_m), namedScratchpadAction scratchpads "music"),
      -- close focused window
      ((modm, xK_q), kill),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      ((modm, xK_m), sendMessage $ JumpToLayout "Full"), 
      ((modm, xK_t), sendMessage $ JumpToLayout "ResizableTall"),
      ((mod1Mask, xK_t), sendMessage $ JumpToLayout "Tabbed Simplest"),
      ((mod1Mask, xK_f), sendMessage $ JumpToLayout "Simple Float"),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      -- Move focus to the next window
      ((modm, xK_Tab), windows W.focusDown),
      -- Move focus to the next window
      ((modm, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((modm, xK_k), windows W.focusUp),
      -- Move focus to the master window
      --((modm, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((modm .|. shiftMask, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area
      ((modm, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm, xK_l), sendMessage Expand),
      ((mod1Mask, xK_a), sendMessage MirrorShrink),
      ((mod1Mask, xK_z), sendMessage MirrorExpand),
      -- Push window back into tiling
      ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- Quit xmonad
      ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
      -- Restart xmonad
      ((modm .|. controlMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
-- normal
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]


  --      [ ((m .|. modm, k), windows $ onCurrentScreen f i)
  --        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9],
  --          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  --      ]



      ++
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3

      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_z] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]
myLayout = lessBorders (Screen) tiled ||| noBorders Full ||| noBorders simpleTabbed ||| simpleFloat
  where
    tiled    = ResizableTall nmaster delta ratio []
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

myWorkspaces = do
  withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

--myStartupHook :: X()
------myStartupHook = spawnOn "1" "firefox"                   
--myStartupHook = do
--  -- spawnOnOnce "1" "firefox"
--  --spawnOnce "~/.local/bin/notify-log"
--  spawnOnce "xautolock -secure -detectsleep -time 5 -locker 'xsecurelock'"
--  spawnOnce "feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/smou.jpg'"
--  spawnOnce "urxvtd -q -o -f"
--  spawnOnce "xrdb ~/.config/.Xresources"
--  spawnOnce "setxkbmap -option altwin:swap_lalt_lwin,altwin:ctrl_alt_win"
--  --spawnOnce "pcmanfm -d"
--  spawnOnce "dunst"
--  spawnOnce "xsetroot -cursor_name left_ptr"


myManageHook = composeAll
  -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
  -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
  -- I'm doing it this way because otherwise I would have to write out the full
  -- name of my workspaces and the names would be very long if using clickable workspaces.
  [ title =? "alsamixer" --> doCenterFloat
    ,appName =? "Browser" --> doCenterFloat
  ] -- <+> namedScratchpadManageHook scratchpads

--scratchpads = [
--    NS "music" "urxvtc -e ncmpcpp" (title =? "ncmpcpp")
--        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
--
--              ] where role = stringProperty "WM_WINDOW_ROLE"

myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent = xmobarColor "#c6c6c6" "" . wrap "[" "]"
      , ppTitle = xmobarColor "#c6c6c6" ""
      , ppVisible = wrap "(" ")"
      , ppSep = " "
      , ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!"
      , ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
          , ppLayout =
        ( \x -> case x of 
        "Tall" -> "[]=="
        "ResizableTall" -> "[]=" 
        "Full" -> "[]"
        "Simple Float" -> "^"
        "Tabbed Simplest" -> "--"
        _ -> "LAYOUT NOT DETECTED"
        )

    }

--myXmobarPP :: PP
--myXmobarPP = def
--    { ppSep             = " "
--    , ppTitleSanitize   = xmobarStrip
--    , ppCurrent         = white . wrap "[" "]"
--    , ppVisible         = wrap "(" ")"
--    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
--    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
--    , ppExtras          = [logTitles formatFocused formatUnfocused]
--    , ppLayout =
--        ( \x -> case x of 
--        "Tall" -> "[]=="
--        "ResizableTall" -> "[]=" 
--        "Full" -> "[]"
--        "Simple Float" -> "^"
--        "Tabbed Simplest" -> "--"
--        _ -> "LAYOUT NOT DETECTED"
--        )
--    }
--  where
--    formatFocused   = wrap (white    "[") (white    "]") . white  .  ppWindow
--    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . lowWhite  . ppWindow
--
--    ppWindow :: String -> String
--    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 70
--
--    lowWhite, red, white, yellow :: String -> String
--    white    = xmobarColor "#f8f8f2" ""
--    yellow   = xmobarColor "#ffff00" ""
--    red      = xmobarColor "#ff0000" ""
--    lowWhite = xmobarColor "#bbbbbb" ""
