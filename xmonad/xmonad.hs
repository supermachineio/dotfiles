import XMonad
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksStartupHook)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Hooks.UrgencyHook
import XMonad.Config.Desktop                (desktopConfig)

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad
        $ withUrgencyHook LibNotifyUrgencyHook
        $ myConfig xmproc `additionalKeys` myKeyConfig

myConfig xmproc = def
    { workspaces = ["[dev]","[dev2]","[web]","[spotify]","[5]","[6]","[7]","[8]","[9]"]
    , manageHook = manageDocks <+> myManageHook
    , layoutHook = avoidStruts $ layoutHook def
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
                    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                    , ppSep = "   "
                    }
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , startupHook = startupHook desktopConfig <+> docksStartupHook
    }

myKeyConfig =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s -e 'mv $f ~/Pictures/'")
    , ((0, xK_Print), spawn "scrot -e 'mv $f ~/Pictures/'")
    , ((0, xK_F9), spawn "xrandr --output eDP-1 --mode 1920x1080 --dpi 128 --output DP-1 --off --output HDMI-1 --off")
    , ((0, xK_F10), spawn "xrandr --output DP-1 --mode 2560x1440 --output eDP-1 --off")
    , ((0, 0x1008ff11), lowerVolume 2 >> return ())
    , ((0, 0x1008ff12), toggleMute >> return ())
    , ((0, 0x1008ff13), raiseVolume 2 >> return ())
    ]

------------------------------------------------------------------------
-- Window rules
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
    [ className =? "Chromium"       --> doShift "[web]"
    , className =? "Google-chrome"  --> doShift "[web]"
    , className =? "Firefox"        --> doShift "[web]"
    , className =? "Slack"          --> doShift "[web]"
    , className =? "Spotify"        --> doShift "[spotify]"
    , className =? "Enpass"         --> doFloat
    , className =? "trayer"         --> doIgnore
    ]

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#aa00aa"
-- Color of current window title in xmobar.
xmobarTitleColor = "#ffffff"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = xmobarTitleColor
-- Width of the window border in pixels.
myBorderWidth = 1
