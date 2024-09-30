#!/usr/bin/env bash
batfun() {
	bat=$(cat /sys/class/power_supply/BAT0/capacity)
	batstatus="$(cat /sys/class/power_supply/BAT0/status)"
	if [ "$bat" = 100 ]; then
		printf '%s' "$bat%[F]"
	elif [ "$bat" -lt 100 ] && [ "$batstatus" = "Charging" ]; then
		printf '%s' "$bat%[C]"

	elif [ "$bat" -lt 100 ] && [ "$batstatus" = "Discharging" ]; then
		printf '%s' "$bat%[D]"
	else
		printf '%s' "$bat%"
	fi
}

datefun() {
	date +"%a %d/%m %T"
}

volfun() {
	amixer get Master | grep -o [0-9]*% | head -1
}

wififun() {
	cat /proc/net/wireless | awk 'NR==3 {printf("%.0f_\n",$3*10/7)}'
}
while true; do
	xsetroot -name " $(volfun) $(wififun) $(batfun) $(datefun)"
	sleep 1
done

#while true; do
#	xsetroot -name "$(cat /sys/class/power_supply/BAT0/capacity)$(echo "%") $(date +"%a %d/%m %T" ) $(amixer get Master | grep -o [0-9]*% | head -1) $(cat /proc/net/wireless | awk 'NR==3 {printf("W=%.0f%%\n",$3*10/7)}')"
#   sleep 1
#done
