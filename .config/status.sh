#!/usr/bin/env bash
while true; do 
	xsetroot -name "$(cat /sys/class/power_supply/BAT0/capacity)$(echo "%") $(date +"%d/%m %T" )"
   sleep 1
done &

