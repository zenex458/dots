#!/bin/bash
while true; do                                                                   xsetroot -name "$(awk -F"[][]" '/dB/ { print $2 }' <(amixer sget Master)) $(cat /sys/class/power_supply/BAT0/capacity) $(date +"%d/%m %T" )"
   sleep 1
done &

