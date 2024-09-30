import Control.Monad
import qualified Data.Map as M
import System.Exit
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

main :: IO ()
main =
  xmonad
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = myLayout,
      normalBorderColor = "#000000",
      focusedBorderColor = "#999999",
      workspaces = myWorkspaces,
      manageHook = manageSpawn <+> myManageHook,
      keys = myKeys
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- terminal
    [ ((modm, xK_Return), spawn "st -e tmux"),
      -- normal firefox
      ((modm, xK_c), spawn "firefox"),
      ((modm .|. shiftMask, xK_c), spawn "firefox --private-window"),
      -- private firefox
      ((mod1Mask, xK_c), spawn "firefox -P priv"),
      -- next song
      ((modm .|. shiftMask, xK_o), spawn "mpc next"),
      -- previous song
      ((modm .|. shiftMask, xK_i), spawn "mpc prev"),
      -- play/pause song
      ((modm .|. shiftMask, xK_p), spawn "mpc toggle"),
      -- spawn alsamixer
      ((modm, xK_a), spawn "st -e pulsemixer"),
      -- minus vol
      ((modm .|. shiftMask, xK_bracketleft), spawn "amixer sset Master 2%-"),
      -- add vol
      ((modm .|. shiftMask, xK_bracketright), spawn "amixer sset Master 2%+"),
      -- spawn emacsclient
      ((modm, xK_u), spawn "emacsclient -c -a emacs"),
      -- brightness up by 2
      ((modm .|. shiftMask, xK_Prior), spawn "light -A 2"),
      -- brightness down by 2
      ((modm .|. shiftMask, xK_Next), spawn "light -U 2"),
      -- application launcher
      ((modm, xK_p), spawn "dmenu_run"),
      -- close focused window
      ((modm .|. shiftMask, xK_q), kill),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      ((modm, xK_m), sendMessage $ JumpToLayout "Full"),
      ((modm, xK_t), sendMessage $ JumpToLayout "Tall"),
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
      -- ((modm, xK_m), windows W.focusMaster),
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
      -- Push window back into tiling
      ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- Quit xmonad
      --      ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
      ((modm .|. shiftMask .|. controlMask, xK_z), quitWithWarning),
      -- Restart xmonad
      -- ((modm .|. controlMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
      ((modm .|. controlMask, xK_r), spawn "xmonad --restart")
    ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      -- normal
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      -- warp cursor to screen
      ++ [ ((modm .|. controlMask, key), warpToScreen sc (0.5) (0.5))
           | (key, sc) <- zip [xK_z, xK_w, xK_e] [0 ..]
         ]
      ++
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_r, xK_w, xK_e] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

quitWithWarning :: X ()
quitWithWarning = do
  let m = "confirm quit"
  s <- dmenu [m]
  when (m == s) (io exitSuccess)

myLayout = tiled ||| noBorders Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myWorkspaces = do
  ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myManageHook =
  composeAll
    -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
    -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
    -- I'm doing it this way because otherwise I would have to write out the full
    -- name of my workspaces and the names would be very long if using clickable workspaces.
    [ className =? "firefox" --> doShift (myWorkspaces !! 0),
      className =? "Emacs" --> doShift (myWorkspaces !! 1),
      className =? "mpv" --> doShift (myWorkspaces !! 7),
      title =? "pulsemixer" --> doCenterFloat,
      appName =? "Browser" --> doCenterFloat
    ]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = xmobarColor "#c6c6c6" "" . wrap "[" "]",
      ppTitle = xmobarColor "#c6c6c6" "",
      ppVisible = wrap "(" ")",
      ppSep = " ",
      ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!",
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t],
      ppLayout =
        ( \x -> case x of
            "Tall" -> "[]=="
            "ResizableTall" -> "[]="
            "Full" -> "[]"
            "Simple Float" -> "^"
            "Tabbed Simplest" -> "--"
            _ -> "LAYOUT NOT DETECTED"
        )
    }
