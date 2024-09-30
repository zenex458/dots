while true; do
   xsetroot -name "$(cat /sys/class/power_supply/BAT0/capacity) $(date +"%d/%m %T" )"
   sleep 1    
done &

while true; do
   batlvl="`acpi -b | grep -P -o '[0-9]+(?=%)'`"
if [[ $batlvl -le 15 || $batlvl -le 10 ]]
then
    notify-send -u critical "Battery low" "Battery level is ${batlvl}%!"
fi
   sleep 2m
done &



feh --no-fehbg --bg-fill '/home/zenex/Downloads/Images/sat.png' & \
redshift -o -c ~/.config/redshift.conf & \
dunst & \
slstatus & \
#batsignal -b & \ 
# xsetroot -cursor_name left_ptr & \
exec dwm
