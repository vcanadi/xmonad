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


myAdditionalKeys =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "sleep 0.1 ; xtrlock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -e 'mv $f /home/bunkar/downloads/screenshots/' ")
    , ((mod4Mask, xK_Left), spawn "pactl  set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -5%")
    , ((mod4Mask, xK_Right), spawn "pactl  set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +5%")
    , ((mod4Mask, xK_Up), spawn "setxkbmap us")
    , ((mod4Mask, xK_Down), spawn "setxkbmap hr")
    , ((mod4Mask .|. shiftMask, xK_Left), spawn "xrandr --output eDP1 --brightness 0.3")
    , ((mod4Mask .|. shiftMask, xK_Right), spawn "xrandr --output eDP1 --brightness 1")
    , ((mod4Mask .|. shiftMask, xK_c), return ())
    , ((mod4Mask .|. controlMask, xK_x), safePrompt "firefox" greenXPConfig)
    , ((mod4Mask , xK_p), shellPrompt greenXPConfig)
    , ((mod4Mask , xK_w), spawn "xrandr --output eDP1 --auto; xrandr --output HDMI1 --off")
    , ((mod4Mask , xK_e), spawn "xrandr --output HDMI1 --auto; xrandr --output eDP1 --off")
    , ((mod4Mask , xK_g), goToSelected (gsconfig2 greenColorizer))
    ]

myAdditionalKeysP =
    let mouseStep = "120" in let slowMouseStep = "20" in
    [ ("M-C-h",spawn $ "xdotool mousemove_relative -- -" <>  mouseStep<>" 0")
    ,("M-C-l",spawn $ "xdotool mousemove_relative "     <>  mouseStep<>" 0")
    ,("M-C-j",spawn $ "xdotool mousemove_relative 0 "   <>  mouseStep<>"")
    ,("M-C-k",spawn $ "xdotool mousemove_relative -- 0 -"<>mouseStep<>"")
    ,("M-C-S-h",spawn $ "xdotool mousemove_relative -- -"<>slowMouseStep<>" 0")
    ,("M-C-S-l",spawn $ "xdotool mousemove_relative "<>slowMouseStep<>" 0")
    ,("M-C-S-j",spawn $ "xdotool mousemove_relative 0 "<>slowMouseStep<>"")
    ,("M-C-S-k",spawn $ "xdotool mousemove_relative -- 0 -"<>slowMouseStep<>"")
    ,("M-C-S-<Space>",spawn $ "xdotool click 1     ")
    ]

myConfig xmproc  = def
        {
          logHook = dynamicLogWithPP def
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "blue" "" . shorten 50
                        }
        , layoutHook =  myLayout
        , handleEventHook = mconcat [ docksEventHook, handleEventHook def ]
        , modMask = mod4Mask
        , terminal = myTerminal
        , borderWidth = 0
        , focusedBorderColor = "#FF0000"
        , normalBorderColor = "#000000"
        , focusFollowsMouse = False
        , workspaces = myWorkspaces
        }
        `additionalKeys` myAdditionalKeys
        `additionalKeysP`myAdditionalKeysP

myPP = xmobarPP { ppOutput          = putStrLn
                , ppCurrent         = xmobarColor "#336433" "" . wrap "[" "]"
                , ppTitle           = xmobarColor "darkgreen"  "" . shorten 20
                , ppLayout          = shorten 50
                , ppUrgent          = xmobarColor "red" "yellow"
                }
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ myConfig xmproc

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
