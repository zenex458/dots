#!/usr/bin/env bash
while true; do
	#batlvl="`acpi -b | grep -P -o '[0-9]+(?=%)'`"
	batlvl="$(cat /sys/class/power_supply/BAT0/capacity)"
	batstatus="$(cat /sys/class/power_supply/BAT0/status)"
	if [ "$batlvl" -le 20 ] && [ "$batstatus" = "Discharging" ]; then
		notify-send -u critical "Battery low" "Battery level is ${batlvl}%!"
	fi
  if ! pgrep syncthing && [ ! "$batstatus" = "Discharging" ]; then
		syncthing --no-browser --logfile=/tmp/syncthing-log
  elif pgrep syncthing &&  [ "$batstatus" = "Discharging" ]; then
        pkill syncthing
	fi
	sleep 2m
done &
