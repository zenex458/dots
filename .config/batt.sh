#!/bin/bash
while true; do

batlvl="`acpi -b | grep -P -o '[0-9]+(?=%)'`"
if [[ $batlvl -le 15 ]]
then
    notify-send -u critical "Battery low" "Battery level is ${batlvl}%!"
fi
sleep 2m
done &
