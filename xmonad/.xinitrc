xrdb -merge ~/.Xresources &
redshift -l 51.50853:-0.12574 &
xrandr --output eDP-1 --right-of DP-1
exec ~/.local/bin/xmonad
