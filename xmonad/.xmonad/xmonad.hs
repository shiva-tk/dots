import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Spacing

main = xmonad desktopConfig
    { terminal    = "kgx"
    , modMask     = mod4Mask
    , borderWidth = 3
    , layoutHook  = myLayoutWithSpacing
    }

------------------------------------------------------------------------
-- Layouts

myLayoutWithSpacing = smartSpacing 10 myLayout

myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
