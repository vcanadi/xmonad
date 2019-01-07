{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad               (forM_)
import           Data.Bool                   (bool)
import           Data.List
import qualified Data.Map                    as M
import           Data.Monoid
import           System.IO
import           XMonad
import           XMonad.Actions.GridSelect
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks    hiding (L)
import           XMonad.Hooks.Script
import           XMonad.Layout
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders     (noBorders, smartBorders)
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WorkspaceDir
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.EZConfig        (additionalKeys, additionalKeysP)
import           XMonad.Util.Run             (safeSpawn, spawnPipe)


-- Displays from 'xrandr'
-- TODO Automate this
primaryDisplay = "eDP-1-1"
secondaryDisplay = "HDMI-0"
displays = [primaryDisplay, secondaryDisplay]

data DisplayState = DSPrimary | DSSecondary | DSBoth deriving (Eq, Bounded, Enum, Show, Typeable)
instance ExtensionClass DisplayState where
    initialValue = DSPrimary

-- Easy configuration change when changing relative display 1 and 2 position
data SecondaryDisplayPosition = SDPLeft | SDPRight
secondaryDisplayPosition = SDPRight

sdpXrandr = case secondaryDisplayPosition of
            SDPLeft  -> "--right-of"
            SDPRight -> "--left-of"

sdpKeys = case secondaryDisplayPosition of
          SDPLeft  -> [xK_w, xK_e]
          SDPRight -> [xK_e, xK_w]

-- Cyclic enumeration hack
next :: (Eq a, Enum a, Bounded a) => a -> a
next = bool minBound <$> succ <*> (/= maxBound)

-- Force usage of tmux as a window manager by restricting control on xmonad layer
myWorkspaces = ["primary", "secondary"]

myTallLayout =  Tall nmaster delta tiled_ratio
    where
    nmaster = 1
    delta = 10/100
    tiled_ratio = 11/19

myDefaultLayout =
        simpleTabbed
    ||| smartBorders myTallLayout
    ||| smartBorders (Mirror myTallLayout)

myLayout = avoidStruts $ myDefaultLayout

myTerminal = "urxvt"

spawnShell = safeSpawn myTerminal []

restartXMonad = broadcastMessage ReleaseResources >>
                restart "xmonad" True

mousePress, mouseRelease, mouseClick :: Int -> X ()
mousePress btn = safeSpawn "xdotool" [ "mousedown", show btn]
mouseRelease btn = safeSpawn "xdotool" [ "mouseup", show btn]
mouseClick btn = safeSpawn "xdotool" [ "click", show btn]

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

myKeys = \conf -> M.fromList $
    [ ((0, xK_Print),  safeSpawn "scrot" [ "-e 'mv $f ~/downloads/screenshots/'"] )

    -- Toggle primary display
    , ( (mod4Mask , xK_r)
      ,  XS.modify @DisplayState next
      >> XS.get @DisplayState >>= \case
          DSPrimary   -> safeSpawn "xrandr" [ "--output", primaryDisplay,   "--auto"
                                            , "--output", secondaryDisplay, "--off"]
          DSSecondary -> safeSpawn "xrandr" [ "--output", primaryDisplay,   "--off"
                                            , "--output", secondaryDisplay, "--auto"]
          DSBoth      -> safeSpawn "xrandr" [ "--output", primaryDisplay,   "--auto"
                                            , sdpXrandr,  secondaryDisplay])

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
    , ((mod4Mask , xK_y), mouseClick 6 )
    , ((mod4Mask , xK_o), mouseClick 7 )

    -- Windows management is moved to tmux so use tmux's modifier for window specific actions on xmonad level
    , ((mod1Mask , xK_Tab), windows W.focusDown)
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.swapDown)
    , ((mod1Mask , xK_p), shellPrompt amberXPConfig)
    , ((mod1Mask , xK_semicolon), safeSpawn "rofi" ["-modi", "run", "-show", "drun"])
    , ((mod1Mask, xK_Return), spawnShell)
    , ((mod1Mask, xK_Left), safeSpawn "pactl" [ "set-sink-volume", "alsa_output.pci-0000_00_1f.3.analog-stereo", "-5%" ])
    , ((mod1Mask, xK_Right), safeSpawn "pactl" [ "set-sink-volume", "alsa_output.pci-0000_00_1f.3.analog-stereo", "+5%" ])
    , ((mod1Mask .|. shiftMask, xK_Left), forM_ displays $ \d -> safeSpawn "xrandr" [ "--output", d, "--brightness", "0.3"])
    , ((mod1Mask .|. shiftMask, xK_Right), forM_ displays $ \d -> safeSpawn "xrandr" [ "--output", d, "--brightness", "1"])
    , ((mod1Mask, xK_q), restartXMonad)
    , ((mod1Mask, xK_space), sendMessage NextLayout)
    ]
    <>
    -- mod-{w,e} %! Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{w,e} %! Move client to screen 1 or 2
    [ ((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip sdpKeys [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    <>
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  where
    mouseStep = 80
    slowMouseStep = 15
    mouseMove dx dy = safeSpawn "xdotool" [ "mousemove_relative", "--", show dx, show dy ]
    mouseMoveUp ds = mouseMove 0 (-ds)
    mouseMoveDown ds = mouseMove 0 ds
    mouseMoveLeft ds = mouseMove (-ds) 0
    mouseMoveRight ds = mouseMove ds 0

-- Disable (all) mouse events replaced by keyboard
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList []

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8


myConfig xmproc  = def
    { manageHook = manageDocks <+> manageHook defaultConfig
    , logHook    = myLogHook
    -- , logHook = dynamicLogWithPP def
    --                 { ppOutput = hPutStrLn xmproc
    --                 , ppTitle = xmobarColor "white" "" . shorten 50
    --                 }
    , layoutHook =  myLayout
    , handleEventHook = mconcat [ docksEventHook, handleEventHook def, keyUpEventHook, keyDownEventHook ]
    , modMask = mod4Mask
    , terminal = myTerminal
    , borderWidth = 0
    , focusFollowsMouse = False
    , workspaces = myWorkspaces
    , mouseBindings = myMouseBindings
    , keys = myKeys
    }


main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ myConfig xmproc

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
