#!/usr/bin/env bash

userarg=$(printf "10\n15\n20\n25\n30\n35\n40\n50\n60\n70\n80\n\n90\n100" | bemenu -p Brightness\ Level\ \($(printf '%.*f\n' 0 $(light -G))\):)

light -S "$userarg"
