import XMonad
import XMonad.Config.Desktop

main = xmonad desktopConfig
    { terminal    = "kgx"
    , modMask     = mod4Mask
    , borderWidth = 3
    }
