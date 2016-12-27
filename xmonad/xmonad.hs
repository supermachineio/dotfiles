import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Hooks.UrgencyHook

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
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
    , className =? "Slack"          --> doShift "[comm]"
    , className =? "Enpass"         --> doFloat
    , className =? "Gimp"           --> doFloat
    ]

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ffb6b0"
-- Color of current window title in xmobar.
xmobarTitleColor = "#ffffff"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = xmobarTitleColor
-- Width of the window border in pixels.
myBorderWidth = 1

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ def
        { workspaces = ["[dev]","[web]","[comm]","[spotify]","[tmp]","[6]","[7]","[8]","[9]"]
        , manageHook = manageDocks <+> myManageHook
        , layoutHook = avoidStruts  $  layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
                        , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                        , ppSep = "   "
                        }
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
