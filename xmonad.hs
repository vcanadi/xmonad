import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks hiding (L)
import XMonad.Hooks.Script
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys,additionalKeysP)
import System.IO
import Data.Monoid
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders ( noBorders, smartBorders)
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Grid

myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
myWorkspaces = fmap show [1..7] <> ["8:media","9:web"] 


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
      $ onWorkspace (myWorkspaces!!7) (noBorders Full)  
      $ onWorkspace (myWorkspaces!!8) (simpleTabbed ||| Mirror myTallLayout)  
      $ myDefaultLayout

-- myLayoutWithDir dir = workspaceDir dir $ myLayout

myTerminal = "urxvt"


myAdditionalKeys =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "sleep 0.1 ; xtrlock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -e 'mv $f /home/deder/Documents/screenshots/' ")
    , ((mod4Mask, xK_Left), spawn "amixer set Master 5%-")
    , ((mod4Mask, xK_Right), spawn "amixer set Master 5%+")
    , ((mod4Mask, xK_Up), spawn "setxkbmap us && . ~/.xmodmaprc")
    , ((mod4Mask, xK_Down), spawn "setxkbmap hr  && . ~/.xmodmaprc")
    , ((mod4Mask .|. shiftMask, xK_Left), spawn "xrandr --output eDP1 --brightness 0.4")
    , ((mod4Mask .|. shiftMask, xK_Right), spawn "xrandr --output eDP1 --brightness 1")
    -- , ((mod4Mask .|. controlMask, xK_x), safePrompt "firefox" greenXPConfig)
    , ((mod4Mask , xK_p), shellPrompt greenXPConfig)
    , ((mod4Mask .|. controlMask, xK_x), xmonadPrompt def)
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


-- myXMobarConfig = XConfig { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
--         , borderColor = "black"
--         , border = TopB
--         , bgColor = "black"
--         , fgColor = "grey"
--         , position = TopW L 100
--         , commands = [ Run Weather "CYVR" ["-t","<tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
--                         , Run Network "eth0" ["-L","0","-H","32","--normal","green","--high","red"] 10
--                         , Run Network "eth1" ["-L","0","-H","32","--normal","green","--high","red"] 10
--                         , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
--                         , Run Memory ["-t","Mem: <usedratio>%"] 10
--                         , Run Swap [] 10
--                         , Run Com "uname" ["-s","-r"] "" 36000
--                         , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
--                         , Run StdinReader
--                         ]
--         , sepChar = "%"
--         , alignSep = "}{"
--         , template = "%StdinReader% | %cpu% | %memory% * %swap% | %eth0% - %eth1% }{<fc=#ee9a00>%date%</fc> | %uname% | %CYVR% "
--         }


myManageHooks :: [ManageHook]
myManageHooks = [
  resource =? "stalonetray" --> doIgnore
  ]

myConfig xmproc  = def
        { startupHook = spawn ". ~/.xmonad/hooks/startup"
        , manageHook = manageDocks <+>  composeAll myManageHooks
        , logHook = dynamicLogWithPP def
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "blue" "" . shorten 50
                        } 
                  <+> spawn "date >> .xmonad/output"
        , layoutHook =  myLayout
        , handleEventHook = mconcat [ docksEventHook, handleEventHook def ] 
        , modMask = mod4Mask
        , terminal = myTerminal
        , borderWidth=2
        , focusedBorderColor = "#00FF00"
        , normalBorderColor = "#000000"
        , focusFollowsMouse = False
        , workspaces = myWorkspaces 
        }
        `additionalKeys` myAdditionalKeys
        `additionalKeysP`myAdditionalKeysP


main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad (myConfig xmproc)
    -- xmonad =<< statusBar "xmobar" myXmobarPP toggleStrutsKey  myConfig

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
