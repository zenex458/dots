#!/usr/bin/env dash
feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/balls.png' & \
#~/.config/status.sh & \
~/.config/batt.sh & \
~/.config/redshift.sh & \
dunst & \
xsetroot -cursor_name left_ptr & \
lxqt-policykit-agent & \
urxvtd -q -o -f & \
xrdb ~/.Xresources & \
setxkbmap -option caps:swapescape & \
exec xmonad
