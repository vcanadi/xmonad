Config {


   -- appearance
     font =         "xft:monospace:size=11:antialias=true"
   , bgColor =      "black"
   , fgColor =      "#AAAAAA"
   , position =     BottomSize C 100 30
   , border =       TopB
   , borderColor =  "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = " %disku% | %diskio% | %multicpu% | %coretemp% | %top% }{ %topmem% | %memory% | %dynnetwork% | %date% || %kbd%  "


   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = False    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     True    -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
      , commands =
        [ Run TopProc [] 50

        , Run TopMem  [] 50

        , Run DiskU
            [("/", "<used>/<size>"), ("sdb3", "<usedvbar>")]
            ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
            100

        , Run DiskIO
            [("/", "<read> <write>"), ("sdb3", "<total>")]
            []
            50

        -- network activity monitor (dynamic interface resolution)
        , Run DynNetwork
            [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
            , "--Low"      , "1000"       -- units: kB/s
            , "--High"     , "5000"       -- units: kB/s
            , "--low"      , "darkgreen"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
            ] 50

        -- cpu activity monitor
        , Run Memory ["-t","Mem: <usedratio>%"] 10
        , Run Swap [] 10
        , Run MultiCpu
            [ "--template" , "Cpu(%): <autototal>"
            , "--Low"      , "50"         -- units: %
            , "--High"     , "85"         -- units: %
            , "--low"      , "darkgreen"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
            ] 20

        -- cpu core temperature monitor
        , Run CoreTemp
            [ "--template" , "Temp(°C): <core0>|<core1>|<core2>|<core3>|<core4>|<core5>|<core6>|<core7>|<core8>|<core9>|<core10>|<core11>"
            , "--Low"      , "70"        -- units: °C
            , "--High"     , "80"        -- units: °C
            , "--low"      , "darkgreen"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
            ] 50

        -- memory usage monitor
        , Run Memory
            [ "--template" ,"Mem: <usedratio>%"
            , "--Low"      , "20"        -- units: %
            , "--High"     , "90"        -- units: %
            , "--low"      , "darkgreen"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
            ] 30

        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fc=#ABABAB>%d/%m %T</fc>" "date" 10

           -- keyboard layout indicator
        , Run Kbd
            [ ("us(dvorak)" , "<fc=#00008B>DV</fc>")
            , ("us"         , "<fc=#8B0000>US</fc>")
            ]
        ]
   }
