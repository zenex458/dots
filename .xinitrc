#!/usr/bin/env sh
feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/balls.png' & \
~/.config/status.sh & \
~/.config/batt.sh & \
#~/.config/redshift.sh & \
dunst & \
xsetroot -cursor_name left_ptr & \
#lxqt-policykit-agent & \
urxvtd -q -o -f & \
xrdb ~/.config/.Xresources & \
#setxkbmap -option ctrl:swap_lalt_lctl,caps:swapescape & \
#setxkbmap -option ctrl:swap_lalt_lctl,ctrl:nocaps & \
setxkbmap -option altwin:swap_lalt_lwin,altwin:ctrl_alt_win & \ #ctrl/win/alt -> win/alt/ctrl -- better for emacs
exec dbus-launch xmonad
