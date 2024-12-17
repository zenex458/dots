#!/usr/bin/env bash

userarg=$(printf "10\n15\n20\n25\n30\n35\n40" | bemenu -p Volume\ Level\ \($(amixer get Master | grep -o [0-9]*% | head -n 1)\):)

amixer sset Master "$userarg"%
