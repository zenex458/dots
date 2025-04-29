#!/usr/bin/env bash

userarg=$(printf "10\n15\n20\n25\n30\n35\nOther\nMute" | bemenu -p Volume\ Level\ \($(amixer get Master | grep -o [0-9]*% | head -n 1)\):)

if [ "$userarg" -gt 35 ]; then
    echo no
elif [ "$userarg" = "Other" ]; then
	userarg=$(printf "" | bemenu -p Custom\ Volume\ Level\ \($(amixer get Master | grep -o [0-9]*% | head -n 1)\):)
    amixer sset Master "$userarg"%

elif [ "$userarg" = "Mute" ]; then
    amixer sset Master toggle
    else
        amixer sset Master "$userarg"%
fi


