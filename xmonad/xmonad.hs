-- An example, simple ~/.xmonad/xmonad.hs file.
-- It overrides a few basic settings, reusing all the other defaults.

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import System.IO
import qualified Data.Map as M
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.Named

-- My shortcuts
additionalKeys conf@(XConfig { XMonad.modMask = modm }) = M.fromList $
        -- General keybindings
        [ ((modm, xK_x),                        kill) 
        , ((0   , 0x1008FF11),        spawn "amixer set Master 2-")
        , ((0   , 0x1008FF13),        spawn "amixer set Master 2+")
        , ((0   , 0x1008FF12),        spawn "amixer -D pulse set Master 1+ toggle")
        , ((modm, xK_F1),             spawn "sudo shutdown -r now") -- reboot
        , ((modm, xK_n),              spawn "touch ~/.pomodoro_session")
        , ((modm ,              xK_Print ), spawn "scrot screen_%Y-%m-%d-%H-%M-%S.png -d 0.1 -e 'mv $f ~/Pictures/'") --take a screenshot of entire display
        , ((modm .|. controlMask, xK_Print ), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 0.1 -u -e 'mv $f ~/Pictures/'") --take a screenshot of focused window
        ]
myKeys = \c -> additionalKeys c `M.union` keys defaultConfig c

-- My window management
myManageHook = composeAll
        [ className =? "Tk"     --> doFloat
        ]
myManageHookWithDefaults = manageDocks <+> myManageHook <+> manageHook defaultConfig

-- My workspaces
myWorkspaces = ["1:web", "2:code", "3:irc", "4:papers", "5:tex"] ++ map show [6..9]

-- My layout management
basicLayout = Tall nmaster delta ratio where
        nmaster = 1
        delta   = 5/100
        ratio   = 2/(1+(toRational(sqrt(5)::Double))) -- golden
wideLayout = smartBorders(      named "wide"    $ Mirror basicLayout )
tallLayout = smartBorders(      named "tall"    $ basicLayout )
tabbedLayout = noBorders(       named "fulltab" $ simpleTabbed )
myLayoutHook = (tallLayout ||| wideLayout ||| tabbedLayout)

main = do
        xmproc <- spawnPipe "/usr/bin/xmobar /home/alans/.xmobarrc"
        xmonad $ defaultConfig
                { manageHook         = myManageHookWithDefaults
                , layoutHook         = avoidStruts $ myLayoutHook
                , borderWidth        = 2
                , terminal           = "gnome-terminal" 
                , keys               = myKeys
                , logHook            = dynamicLogWithPP xmobarPP
                                        { ppOutput = hPutStrLn xmproc -- tell xmonad to use hPutStrLn to output to xmobar
                                        , ppTitle = shorten 50
                                        }
                , workspaces         = myWorkspaces
                }

