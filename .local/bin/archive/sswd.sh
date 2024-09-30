#!/usr/bin/env bash
num=$(seq 10)
del=$(printf '%s' "$num" | dmenu -p "how long for delay") ##rofi -dmenu -p
dela=$(seq "$del" -1 1)                                   ## from 10 decrement by one until 1

if [ ! "$del" ]; then
	exit
fi

NID=0

for i in $dela; do
	NID=$(notify-send -p -r $NID $i)
	sleep 1
done

notify-send -p -r $NID "Screenshot Taken"

scrot ~/Downloads/Images/ss/%Y-%m-%d_$wx$h.png
