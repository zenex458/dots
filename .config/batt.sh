#!/usr/bin/env bash
while true; do

#batlvl="`acpi -b | grep -P -o '[0-9]+(?=%)'`"
batlvl="`cat /sys/class/power_supply/BAT0/capacity`"
if [[ $batlvl -le 15 ]]
then
    notify-send -u critical "Battery low" "Battery level is ${batlvl}%!" && mpv ~/Downloads/yyt/other/Notification.mp3 && mpv ~/Downloads/yyt/other/Notification.mp3
fi
sleep 2m
done &
