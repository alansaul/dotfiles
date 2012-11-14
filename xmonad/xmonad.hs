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
myKeys conf@(XConfig { XMonad.modMask = modm }) = M.fromList $
        -- General keybindings
        [ ((modm, xK_x),                        kill) 
        , ((modm .|. shiftMask, xK_r),         spawn "echo 'Hello'")
        ]

myManageHook = composeAll
        [ className =? "Tk"     --> doFloat
        ]

myWorkspaces = ["1:web", "2:code", "3:irc"] ++ map show [4..9]

basicLayout = Tall nmaster delta ratio where
        nmaster = 1
        delta   = 5/100
        ratio   = 2/(1+(toRational(sqrt(5)::Double))) -- golden
wideLayout = smartBorders(      named "wide"    $ Mirror basicLayout )
tallLayout = smartBorders(      named "tall"    $ basicLayout )
tabbedLayout = noBorders(       named "fulltab" $ simpleTabbed )
     
myLayoutHook = (tallLayout ||| wideLayout ||| tabbedLayout)

--, 
 
main = do
        xmproc <- spawnPipe "/usr/bin/xmobar /home/alans/.xmobarrc"
        xmonad $ defaultConfig
                { manageHook         = manageDocks <+> myManageHook <+> manageHook defaultConfig
                , layoutHook         = avoidStruts $ myLayoutHook
                , borderWidth        = 2
                , terminal           = "gnome-terminal" 
                , keys               = \c -> myKeys c `M.union` keys defaultConfig c
                , logHook            = dynamicLogWithPP xmobarPP
                                        { ppOutput = hPutStrLn xmproc -- tell xmonad to use hPutStrLn to output to xmobar
                                        , ppTitle = shorten 50
                                        }
                , workspaces         = myWorkspaces
                }

