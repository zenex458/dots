#!/usr/bin/env sh
feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/sriver.jpg' &
dunst &
xsetroot -cursor_name left_ptr &
lxqt-policykit-agent &
opensnitch-ui &
redshift.sh &
status.sh &
xrdb ~/.config/.Xresources &
xset s 1800 5 &
xautolock -corners +-00 -time 30 -locker xsecurelock -detectsleep &
xbanish -i shift -t 5 -s &
setxkbmap -option altwin:ctrl_alt_win &
exec dwm
