#!/usr/bin/env sh
xrandr --output eDP-1 --auto --primary --output HDMI-2 --auto --left-of eDP-1 --output HDMI-1 --auto --left-of HDMI-2 && feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/sriver.jpg' &
dunst &
xsetroot -cursor_name left_ptr &
lxqt-policykit-agent &
redshift -o -O 2000 &
xrdb ~/.config/.Xresources &
xset s 1800 5 &
xbanish -i shift -t 5 -s &
xautolock -corners +-00 -time 30 -locker xsecurelock -detectsleep &
#setxkbmap -option altwin:ctrl_alt_win,caps:shift_nocancel,caps:backspace
exec xmonad
