#!/usr/bin/env bash

cmd="brightnessctl set"
currentb=$(brightnessctl i | grep -e '[0-9]*%' -o)
userarg=$(printf "10\n15\n20\n25\n30\n35\n40\n50\n60\n70\n80\n\n90\n100" | bemenu -p Brightness\ Level\ \("$currentb"\):)
$cmd "$userarg"% >/dev/null
