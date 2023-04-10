#!/usr/bin/env dash
	
case  $XDG_SESSION_TYPE in
	"wayland")  shift="gammastep";;
	"x11")  shift="redshift";;
	*)  echo "Invalid"  && exit;;
esac 

while true; do
    hour="$(date +%H)"
    brit="$(light -G)"
    if [ "$hour" -ge 16 ] || [ "$hour" -le 7 ]
    then
	    $shift -x && $shift -o -O 2000
        if [ $brit -ge 70 ]
	    then
		    notify-send "brightness too high"
	    fi
    fi
    sleep 40m
done &
