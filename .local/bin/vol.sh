#!/usr/bin/env bash

tstate=$(wpctl get-volume @DEFAULT_SINK@ | awk '{print $3}')
currentraw=$(echo "$(wpctl get-volume @DEFAULT_SINK@ | awk '{print $2}') * 100" | bc)
current=${currentraw%.*}

userarg=$(printf "10\n15\n20\n25\n30\n35\nOther\nToggle" | bemenu -p Volume\ Level\ \("$current"%\)"$tstate":)
if [ "$userarg" -gt 35 ]; then
    printf "Too loud" | bemenu
elif [ "$userarg" = "Other" ]; then
	  userarg=$(printf "" | bemenu -p Custom\ Volume\ Level\ \("$current"\):)
    wpctl set-volume @DEFAULT_SINK@ "$userarg"%
elif [ "$userarg" = "Toggle" ]; then
    wpctl set-mute @DEFAULT_SINK@ toggle
elif [ "$userarg" ]; then
    wpctl set-volume @DEFAULT_SINK@ "$userarg"%
fi


