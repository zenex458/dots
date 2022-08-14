while true; do
   xsetroot -name "$(cat /sys/class/power_supply/BAT0/capacity) $(date +"%d/%m %T" )"
   sleep 1    
done &

~/.config/batt.sh & \
~/.config/status.sh & \
feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/sat.png' & \
redshift -o -c ~/.config/redshift.conf & \
dunst & \
slstatus & \
#batsignal -b & \ 
# xsetroot -cursor_name left_ptr & \
exec dwm
