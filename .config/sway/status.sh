#!/bin/dash
batt=$(cat /sys/class/power_supply/BAT0/capacity)
per=$(echo "%")
date=$(date +"%a %d/%m %T")

echo "$batt$per $date"
