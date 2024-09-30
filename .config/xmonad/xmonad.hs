import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.NoBorders
-- import XMonad.Layout.Fullscreen

myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool

myFocusFollowsMouse = True

myClickJustFocuses :: Bool

myClickJustFocuses = False

myBorderWidth = 1

myModMask = mod4Mask

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor = "#000000"

myFocusedBorderColor = "#666666"

------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm,              xK_Return), spawn $ XMonad.terminal conf)
    
    , ((modm,		   xK_c      ), spawn "firefox")

    , ((mod1Mask,	   xK_c      ), spawn "firefox -P priv")

    , ((mod1Mask .|. shiftMask, xK_c ), spawn "firefox -P inc")

    , ((mod1Mask,          xK_o      ), spawn "playerctl next")

    , ((mod1Mask,          xK_i      ), spawn "playerctl previous")
    
    , ((mod1Mask,	   xK_p      ), spawn "playerctl play-pause")

    , ((mod1Mask .|. controlMask, xK_l), spawn "xsecurelock & systemctl suspend")
    , ((modm,		   xK_a     ), spawn "alacritty -e alsamixer")

    , ((mod1Mask,	   xK_Prior ), spawn "light -A 2")
    
    , ((mod1Mask,	   xK_Next  ), spawn "light -U 2")

    , ((modm .|. shiftMask, xK_r    ), spawn "redshift -o -c ~/.config/redshift.conf")

    , ((modm,              xK_p     ), spawn "dmenu_run")

    -- close focused window

    , ((modm,               xK_q     ), kill)

     -- Rotate through the available layout algorithms

    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default

    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size

    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window

    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window

    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window

    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window

    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window

    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window

    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window

    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area

    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area

    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling

    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area

    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area

    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap

    -- Use this binding with avoidStruts from Hooks.ManageDocks.

    -- See also the statusBar function from Hooks.DynamicLog.

    --

    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad

    , ((modm .|. controlMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)

    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    ]

    ++

    --

    -- mod-[1..9], Switch to workspace N

    -- mod-shift-[1..9], Move client to workspace N

    --

    [((m .|. modm, k), windows $ f i)

        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]

        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    --

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3

    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3

    --

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))

        | (key, sc) <- zip [xK_w, xK_e, xK_z] [0..]

        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events

--

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w

                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w

                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)

    ]

------------------------------------------------------------------------

-- Layouts:

-- You can specify and transform your layouts by modifying these values.

-- If you change layout bindings be sure to use 'mod-shift-space' after

-- restarting (with 'mod-q') to reset your layout state to the new

-- defaults, as xmonad preserves your old layout settings by default.

--

-- The available layouts.  Note that each layout is separated by |||,

-- which denotes layout choice.

--

myLayout = smartBorders tiled ||| noBorders Full

  where

     -- default tiling algorithm partitions the screen into two panes

     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane

     nmaster = 1

     -- Default proportion of screen occupied by master pane

     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes

     delta   = 3/100

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

myManageHook = composeAll

    [ className =? "MPlayer"        --> doFloat

    , className =? "Gimp"           --> doFloat

    , resource  =? "desktop_window" --> doIgnore

    , resource  =? "kdesktop"       --> doIgnore ]

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

myPP = xmobarPP { ppCurrent = xmobarColor "#c6c6c6" "" . wrap "[" "]"
                , ppTitle = xmobarColor "#c6c6c6" "" . shorten 90
		, ppSep =  "<fc=" ++ "#c6c6c6" ++ "> <fn=1></fn> </fc>" 
		, ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!"}


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

defaults = def {

      -- simple stuff

        terminal           = myTerminal,

        focusFollowsMouse  = myFocusFollowsMouse,

        clickJustFocuses   = myClickJustFocuses,

        borderWidth        = myBorderWidth,

        modMask            = myModMask,

        workspaces         = myWorkspaces,

        normalBorderColor  = myNormalBorderColor,

        focusedBorderColor = myFocusedBorderColor,

      -- key bindings

        keys               = myKeys,

        mouseBindings      = myMouseBindings,

      -- hooks, layouts

        layoutHook         = myLayout,

        manageHook         = myManageHook,

        handleEventHook    = myEventHook,

        logHook            = myLogHook,

        startupHook        = myStartupHook

    }

-- | Finally, a copy of the default bindings in simple textual tabular format.

help :: String

help = unlines ["The default modifier key is 'alt'. Default keybindings:",

    "",

    "-- launching and killing programs",

    "mod-Shift-Enter  Launch xterminal",

    "mod-p            Launch dmenu",

    "mod-Shift-p      Launch gmrun",

    "mod-Shift-c      Close/kill the focused window",

    "mod-Space        Rotate through the available layout algorithms",

    "mod-Shift-Space  Reset the layouts on the current workSpace to default",

    "mod-n            Resize/refresh viewed windows to the correct size",

    "",

    "-- move focus up or down the window stack",

    "mod-Tab        Move focus to the next window",

    "mod-Shift-Tab  Move focus to the previous window",

    "mod-j          Move focus to the next window",

    "mod-k          Move focus to the previous window",

    "mod-m          Move focus to the master window",

    "",

    "-- modifying the window order",

    "mod-Return   Swap the focused window and the master window",

    "mod-Shift-j  Swap the focused window with the next window",

    "mod-Shift-k  Swap the focused window with the previous window",

    "",

    "-- resizing the master/slave ratio",

    "mod-h  Shrink the master area",

    "mod-l  Expand the master area",

    "",

    "-- floating layer support",

    "mod-t  Push window back into tiling; unfloat and re-tile it",

    "",

    "-- increase or decrease number of windows in the master area",

    "mod-comma  (mod-,)   Increment the number of windows in the master area",

    "mod-period (mod-.)   Deincrement the number of windows in the master area",

    "",

    "-- quit, or restart",

    "mod-Shift-q  Quit xmonad",

    "mod-q        Restart xmonad",

    "mod-[1..9]   Switch to workSpace N",

    "",

    "-- Workspaces & screens",

    "mod-Shift-[1..9]   Move client to workspace N",

    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",

    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",

    "",

    "-- Mouse bindings: default actions bound to mouse events",

    "mod-button1  Set the window to floating mode and move by dragging",

    "mod-button2  Raise the window to the top of the stack",

    "mod-button3  Set the window to floating mode and resize by dragging"]