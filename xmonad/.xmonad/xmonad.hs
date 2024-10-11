import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.CenterMainFluid
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobar.config"
  xmonad $ docks $ desktopConfig
    { terminal    = "alacritty"
    , modMask     = mod4Mask
    , borderWidth = 3
    , layoutHook  = spacing 10 myLayout
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys = myKeys
    , logHook = myLogHook xmproc
    , startupHook = myStartupHook
    }


------------------------------------------------------------------------
-- Colours

myNormalBorderColor  = "#1f1f1f"
myFocusedBorderColor = colorFore
myWallpaperColor = colorBack

colorBack = "#090909"
colorFore = color7

color0 = "#4a3637"
color1 = "#d17b49"
color2 = "#7b8748"
color3 = "#af865a"
color4 = "#535c5c"
color5 = "#775759"
color6 = "#6d715e"
color7 = "#c0a48b"
color8 = "#4a3637"
color9 = "#d17b49"
color10 = "#7b8748"
color11 = "#af865a"
color12 = "#535c5c"
color13 = "#775759"
color14 = "#6d715e"
color15 = "#c0a48b"


------------------------------------------------------------------------
-- Layouts

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| centered
  where
    -- Default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- Three columns, centered main window. Main window always occupies the center part of the screen,
    -- even if it is the only window open. Good for ultrawide monitors.
    centered = CenterMainFluid nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


------------------------------------------------------------------------
-- Keybindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn $ "exe=`dmenu_path | dmenu -nb '" ++ colorBack ++ "' -nf '" ++ colorFore ++ "' -sb '" ++ colorBack ++ "' -sf  '" ++ color1 ++ "' -fn 'Iosevka:pixelsize=18' -p 'Run'` && eval \"exec $exe\"")

    -- launch networkmanager-dmenu
    , ((modm,               xK_n     ), spawn $ "networkmanager_dmenu -nb '" ++ colorBack ++ "' -nf '" ++ colorFore ++ "' -sb '" ++ colorBack ++ "' -sf  '" ++ color1 ++ "' -fn 'Iosevka:pixelsize=18' -p 'run: '")

    -- launch networkmanager-dmenu
    , ((modm .|. shiftMask, xK_s     ), spawn "~/bin/select-sink")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "~/bin/power-menu")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Volume control
    , ((0                 , xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
    , ((0                 , xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
    , ((0                 , xF86XK_AudioMute), spawn "amixer set Master toggle")

    -- Apps
    , ((modm              , xK_w     ), spawn "firefox")
    , ((modm              , xK_f     ), spawn "nautilus")
    , ((modm              , xK_s     ), spawn "spotify")
    , ((modm              , xK_e     ), spawn "emacs")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [kK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- XMobar Loghook

myLogHook xmproc    = dynamicLogWithPP xmobarPP 
  { ppOutput        = hPutStrLn xmproc 
  , ppCurrent       = bold . xmobarColor color1 colorBack . xmobarBorder "Bottom" color1 3
  , ppSep           = xmobarColor myNormalBorderColor colorBack $ pad "|"
  , ppTitle         = xmobarColor color5 colorBack
  , ppTitleSanitize = shorten 20
  , ppOrder         = \(ws : _ : t : _) -> [ws, t]
  }
  where bold = wrap "<fn=1>" "</fn>"


------------------------------------------------------------------------
-- Startup

myStartupHook = do
  -- spawnOnce $ "hsetroot -solid \"" ++ myWallpaperColor ++ "\""
  spawnOnce "feh --bg-fill ~/.wallpapers/sand-dune.jpg"
  spawnOnce "picom -b"
