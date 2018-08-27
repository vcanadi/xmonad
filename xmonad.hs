import Data.Monoid
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks hiding (L)
import XMonad.Hooks.Script
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M


greenColorizer = colorRangeFromClassName
    white            -- lowest inactive bg
    (0x70,0xFF,0x70) -- highest inactive bg
    black            -- active bg
    black            -- inactive fg
    white            -- active fg
  where black = minBound
        white = maxBound

gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellwidth = 100 }

myWorkspaces = ["1","2","3:reports","4","5", "6", "7:media", "8", "9:web"]

myTallLayout =  Tall  nmaster delta tiled_ratio
    where
    nmaster = 1
    delta = 10/100
    tiled_ratio = 11/19

myDefaultLayout =
        smartBorders myTallLayout
    ||| smartBorders (Mirror myTallLayout)

myLayout =
        avoidStruts
      $ onWorkspace (myWorkspaces!!6) (noBorders Full)
      $ onWorkspace (myWorkspaces!!8) (simpleTabbed ||| Mirror myTallLayout)
      $ myDefaultLayout

-- myLayoutWithDir dir = workspaceDir dir $ myLayout

myTerminal = "urxvt"

mousePress, mouseRelease, mouseClick :: Int -> X ()
mousePress btn = spawn $ "xdotool mousedown " <> show btn
mouseRelease btn = spawn $ "xdotool mouseup " <> show btn
mouseClick btn = spawn $ "xdotool mousedown " <> show btn <> "; xdotool mouseup " <> show btn

-- Get key release for mouse click simulation
keyDownEventHook :: Event -> X All
keyDownEventHook e = handle e >> return (All True)
  where
    handle :: Event -> X ()
    handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
        | t == keyPress = withDisplay $ \dpy -> do
            s  <- io $ keycodeToKeysym dpy code 0
            mClean <- cleanMask m
            ks <- asks keys
            userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
    handle _ = return ()
    keys _ = M.fromList $
      [ ((mod4Mask , xK_c), mousePress 1)
      , ((mod4Mask .|. controlMask, xK_c), mousePress 1)
      ]

keyUpEventHook :: Event -> X All
keyUpEventHook e = handle e >> return (All True)
  where
    handle :: Event -> X ()
    handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
        | t == keyRelease = withDisplay $ \dpy -> do
            s  <- io $ keycodeToKeysym dpy code 0
            mClean <- cleanMask m
            ks <- asks keys
            userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
    handle _ = return ()
    keys _ = M.fromList $
      [ ((mod4Mask, xK_v), mouseRelease 1)
      , ((mod4Mask .|. controlMask, xK_v), mouseRelease 1)
      ]

myAdditionalKeys =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "sleep 0.1 ; xtrlock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -e 'mv $f ~/downloads/screenshots/' ")
    , ((mod4Mask, xK_Left), spawn "pactl  set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo -5%")
    , ((mod4Mask, xK_Right), spawn "pactl  set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo +5%")
    , ((mod4Mask, xK_Up), spawn "setxkbmap us")
    , ((mod4Mask, xK_Down), spawn "setxkbmap hr")
    , ((mod4Mask .|. shiftMask, xK_Left), spawn "xrandr --output eDP1 --brightness 0.3")
    , ((mod4Mask .|. shiftMask, xK_Right), spawn "xrandr --output eDP1 --brightness 1")
    , ((mod4Mask .|. shiftMask, xK_c), return ())
    , ((mod4Mask , xK_p), shellPrompt greenXPConfig)
    , ((mod4Mask , xK_w), spawn "xrandr --output eDP1 --auto; xrandr --output HDMI1 --off")
    , ((mod4Mask , xK_e), spawn "xrandr --output HDMI1 --auto; xrandr --output eDP1 --off")
    , ((mod4Mask , xK_g), goToSelected (gsconfig2 greenColorizer))
    -- Mouse keys
    , ((mod4Mask , xK_h), mouseMoveLeft mouseStep )
    , ((mod4Mask , xK_l), mouseMoveRight mouseStep )
    , ((mod4Mask , xK_j), mouseMoveDown mouseStep )
    , ((mod4Mask , xK_k), mouseMoveUp mouseStep )
    , ((mod4Mask .|. controlMask, xK_h), mouseMoveLeft slowMouseStep )
    , ((mod4Mask .|. controlMask, xK_l), mouseMoveRight slowMouseStep )
    , ((mod4Mask .|. controlMask, xK_j), mouseMoveDown slowMouseStep )
    , ((mod4Mask .|. controlMask, xK_k), mouseMoveUp slowMouseStep )
    , ((mod4Mask, xK_x), mouseClick 1)
    , ((mod4Mask, xK_c), return ()) -- press
    , ((mod4Mask, xK_v), return ()) -- release
    , ((mod4Mask, xK_b), mouseClick 2)
    , ((mod4Mask, xK_n), mouseClick 3)
    , ((mod4Mask .|. controlMask, xK_x), mouseClick 1)
    , ((mod4Mask .|. controlMask, xK_c), return ()) -- press
    , ((mod4Mask .|. controlMask, xK_v), return ()) -- release
    , ((mod4Mask .|. controlMask, xK_b), mouseClick 2)
    , ((mod4Mask .|. controlMask, xK_n), mouseClick 3)
    , ((mod4Mask , xK_u), mouseClick 5 ) -- scroll
    , ((mod4Mask , xK_i), mouseClick 4 )
    ]
  where
    mouseStep = 80
    slowMouseStep = 15
    mouseMove dx dy = spawn $ "xdotool mousemove_relative -- " <> show dx <> " " <> show dy
    mouseMoveUp ds = mouseMove 0 (-ds)
    mouseMoveDown ds = mouseMove 0 ds
    mouseMoveLeft ds = mouseMove (-ds) 0
    mouseMoveRight ds = mouseMove ds 0

-- Disable (all) mouse events replaced by keyboard
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList []

myConfig xmproc  = def
    { logHook = dynamicLogWithPP def
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "blue" "" . shorten 50
                    }
    , layoutHook =  myLayout
    , handleEventHook = mconcat [ docksEventHook, handleEventHook def, keyUpEventHook, keyDownEventHook ]
    , modMask = mod4Mask
    , terminal = myTerminal
    , borderWidth = 0
    , focusedBorderColor = "#FF0000"
    , normalBorderColor = "#000000"
    , focusFollowsMouse = False
    , workspaces = myWorkspaces
    , mouseBindings = myMouseBindings
    }
    `additionalKeys` myAdditionalKeys


main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ myConfig xmproc

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
