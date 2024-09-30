import qualified Data.Map as M
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.Loggers

myTerminal = "alacritty -e tmux"

-- myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 1

myModMask = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myNormalBorderColor = "#000000"

myFocusedBorderColor = "#666666"
------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf),
      -- normal firefox
      ((modm, xK_c), spawn "firefox"),
      -- private firefox
      ((mod1Mask, xK_c), spawn "firefox -P priv"),
      -- next song
      ((mod1Mask, xK_o), spawn "mpc -p 6601 next"),
      -- previous song
      ((mod1Mask, xK_i), spawn "mpc -p 6601 prev"),
      -- play/pause song
      ((mod1Mask, xK_p), spawn "mpc -p 6601 toggle"),
      -- mute/unmute
      ((mod1Mask, xK_m), spawn "amixer sset Master toggle"),
      -- minus vol
      ((mod1Mask, xK_bracketleft), spawn "amixer sset Master 2%-"),
      -- add vol
      ((mod1Mask, xK_bracketright), spawn "amixer sset Master 2%+"),
      -- suspend and lock screen
      ((mod1Mask .|. controlMask, xK_s), spawn "xsecurelock & systemctl suspend"),
      -- lock
      ((mod1Mask .|. controlMask, xK_l), spawn "xsecurelock"),
      -- spawn alsamixer
      ((modm, xK_a), spawn "alacritty -e alsamixer"),
      -- brightness up by 2
      ((mod1Mask, xK_Prior), spawn "light -A 2"),
      -- brightness down by 2
      ((mod1Mask, xK_Next), spawn "light -U 2"),
      -- bluelight filter
      ((modm .|. shiftMask, xK_r), spawn "redshift -o -c ~/.config/redshift.conf"),
      -- application launcher
      ((modm, xK_p), spawn "dmenu_run"),
      -- close focused window
      ((modm, xK_q), kill),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
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
      ((modm, xK_m), windows W.focusMaster),
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
      ((modm, xK_t), withFocused $ windows . W.sink),
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

      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3

      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_z] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

------------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events

myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging

    [ ( (modm, button1),
        ( \w ->
            focus w >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button2, Raise the window to the top of the stack

      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging

      ( (modm, button3),
        ( \w ->
            focus w >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- If you change layout bindings be sure to use 'mod-shift-space' after

-- restarting (with 'mod-q') to reset your layout state to the new

-- defaults, as xmonad preserves your old layout settings by default.

--

-- The available layouts.  Note that each layout is separated by |||,

-- which denotes layout choice.
myLayout = smartBorders tiled ||| noBorders Full
  where
    -- default tiling algorithm partitions the screen into two panes

    tiled = {-- renamed [Replace "T"] $ --} Tall nmaster delta ratio
    --full = renamed [Replace "M"] $ noBorders Full
    --make sure to import XMonad.Layout.Renamed

    -- The default number of windows in the master pane

    nmaster = 1

    -- Default proportion of screen occupied by master pane

    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes

    delta = 3 / 100

------------------------------------------------------------------------

-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing

-- a new window. You can use this to, for example, always float a

-- particular program, or have a client always appear on a particular

-- workspace.

--

-- To find the property name associated with a program, use

-- > xprop | grep WM_CLASS

-- and click on the client you're interested in.

--

-- To match on the WM_NAME, you can use 'title' in the same way that

-- 'className' and 'resource' are used below.

--

myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat,
      resource =? "kdesktop" --> doIgnore
    ]

------------------------------------------------------------------------

-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--

-- Defines a custom handler function for X Events. The function should

-- return (All True) if the default handler is to be run afterwards. To

-- combine event hooks use mappend or mconcat from Data.Monoid.

--

myEventHook = mempty

------------------------------------------------------------------------

-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.

-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

--

myLogHook = return ()

------------------------------------------------------------------------

-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted

-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize

-- per-workspace layout choices.

--

-- By default, do nothing.

myStartupHook = return ()

------------------------------------------------------------------------

-- Command to launch the bar.

myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.

myPP =
  xmobarPP
    { ppCurrent = xmobarColor "#c6c6c6" "" . wrap "[" "]",
      ppTitle = xmobarColor "#c6c6c6" "" . shorten 120,
      ppSep = " ",
      ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!",
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t],
      ppLayout =
        ( \x -> case x of
            "Tall" -> "[]="
            "Full" -> "[]"
        )
    }

-- Key binding to toggle the gap for the bar.

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------

-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.

--

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

-- A structure containing your configuration settings, overriding

-- fields in the default config. Any you don't override, will

-- use the defaults defined in xmonad/XMonad/Config.hs

--

-- No need to modify this.

--

defaults =
  def
    { -- simple stuff

      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings

      keys = myKeys,
      mouseBindings = myMouseBindings,
      -- hooks, layouts

      layoutHook = myLayout,
      manageHook = myManageHook,
      handleEventHook = myEventHook,
      logHook = myLogHook,
      startupHook = myStartupHook
    }
