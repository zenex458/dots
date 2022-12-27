#!/usr/bin/env sh
feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/balls.png' & \
#~/.config/status.sh & \
~/.config/batt.sh & \
~/.config/redshift.sh & \
dunst & \
xsetroot -cursor_name left_ptr & \
lxqt-policykit-agent & \
urxvtd -q -o -f & \
xrdb ~/.Xresources & \
setxkbmap -option ctrl:swap_lalt_lctl & \
setxkbmap -option caps:swapescape & \
exec xmonad
