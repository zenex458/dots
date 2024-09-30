#!/usr/bin/env bash
while true; do

#batlvl="`acpi -b | grep -P -o '[0-9]+(?=%)'`"
batlvl="`cat /sys/class/power_supply/BAT0/capacity`"
batstatus="`cat /sys/class/power_supply/BAT0/status`"

if [[ $batlvl -le 20 && $batstatus == "Discharging" ]]
then
    notify-send -u critical "Battery low" "Battery level is ${batlvl}%!" && mpg123 --loop 2 ~/Downloads/yyt/other/Notification.mp3
fi
sleep 2m
done &
