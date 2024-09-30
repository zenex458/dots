#!/usr/bin/env bash

#batt=$(cat /sys/class/power_supply/BAT0/capacity)
#per=$(echo "%")
#date=$(date +"%a %d/%m %T")
#
#echo "$batt$per $date"

bat=$(cat /sys/class/power_supply/BAT0/capacity)
if [ "$bat" = 100 ]; then
	batp="$bat% [F]"
elif [ "$bat" -lt 100 ]; then
	batp="$bat% [C]"
else
	batp="$bat%"
fi

datp=$(date +"%a %d/%m %T")

volp=$(amixer get Master | grep -o [0-9]*% | head -1)

vol_status=$(amixer get Master | grep -o "\[on\]\|\[off\]" | head -1)

mic_status=$(amixer get Capture | grep -o "\[on\]\|\[off\]" | head -1)

micp=$(amixer get Capture | grep -o [0-9]*% | head -1)

wifip=$(cat /proc/net/wireless | awk 'NR==3 {printf("%.0f_\n",$3*10/7)}')

echo "$volp" "$vol_status" "| $micp" "$mic_status" "| $wifip" "| $batp" "| $datp"
