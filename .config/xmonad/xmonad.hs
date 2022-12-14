import System.Exit
import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.SpawnOn
import XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad
     -- . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask
    , layoutHook = myLayout
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ff0000"
    , startupHook = myStartupHook
    , manageHook = namedScratchpadManageHook scratchpads
    }
  `additionalKeys`
    [ ((mod, xK_Return), spawn "urxvtc -e tmux"),
      -- normal firefox
      ((mod, xK_f), spawn "firefox"),
      -- private firefox
      ((mod .|. shiftMask, xK_f), spawn "firefox -P priv"),
      -- next song
      ((mod .|. shiftMask, xK_o), spawn "mpc -p 6601 next"),
      -- previous song
      ((mod .|. shiftMask, xK_i), spawn "mpc -p 6601 prev"),
      -- play/pause song
      ((mod .|. shiftMask, xK_p), spawn "mpc -p 6601 toggle"),
      -- mute/unmute
      ((mod .|. shiftMask, xK_m), spawn "amixer sset Master toggle"),
      -- minus vol
      ((mod .|. shiftMask, xK_bracketleft), spawn "amixer sset Master 2%-"),
      -- add vol
      ((mod .|. shiftMask, xK_bracketright), spawn "amixer sset Master 2%+"),
      -- suspend and lock screen
      ((mod .|. shiftMask .|. controlMask, xK_s), spawn "systemctl suspend && xsecurelock"),
      -- lock
      ((mod .|. shiftMask .|. controlMask, xK_l), spawn "xsecurelock"),
      -- spawn alsamixer
      ((mod, xK_a), spawn "urxvtc -e alsamixer -V all"),
      -- brightness up by 2
      ((mod .|. shiftMask, xK_Prior), spawn "light -A 2"),
      -- brightness down by 2
      ((mod .|. shiftMask, xK_Next), spawn "light -U 2"),
      -- bluelight filter
      ((mod .|. shiftMask, xK_r), spawn "redshift -o -c ~/.config/redshift.conf"),
      -- application launcher
      ((mod, xK_p), spawn "dmenu_run"),
      -- rofi show window
      ((mod .|. shiftMask, xK_w), spawn "rofi -show window"),
      -- scrathpad
      ((mod, xK_m), namedScratchpadAction scratchpads "music"),
      -- close focused window
      ((mod, xK_q), kill),
      -- Quit xmonad
      ((mod .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
      -- Restart xmonad
      ((mod .|. controlMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    ]
 where mod = modMask myConfig

myLayout = smartBorders tiled ||| noBorders Full
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

myStartupHook :: X()
--myStartupHook = spawnOn "1" "firefox"                   
myStartupHook = spawnOnOnce "1" "firefox"

  
scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "music" "urxvt -e ncmpcpp" (title =? "ncmpcpp")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

              ] where role = stringProperty "WM_WINDOW_ROLE"

--myXmobarPP :: PP
--myXmobarPP = def
--    { ppCurrent = xmobarColor "#c6c6c6" "" . wrap "[" "]"
--      , ppTitle = xmobarColor "#c6c6c6" ""
--      , ppSep = " "
--      , ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!"
--      , ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
--      , ppLayout =
--        ( \x -> case x of
--            "Tall" -> "" --[]=
--            "Full" -> "" --[]
--        )
--    }
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = " "
    , ppTitleSanitize   = xmobarStrip
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    , ppLayout =
        ( \x -> case x of
            "Tall" -> "" --[]=
            "Full" -> "" --[]
        )
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . white -- .  ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . lowWhite -- . ppWindow

   -- ppWindow :: String -> String
   -- ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w)-- . shorten 30

    lowWhite, red, white, yellow :: String -> String
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#ffff00" ""
    red      = xmobarColor "#ff0000" ""
    lowWhite = xmobarColor "#bbbbbb" ""
