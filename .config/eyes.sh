#!/usr/bin/env dash

case  $XDG_SESSION_TYPE in
	"wayland")  shift="gammastep -P -o -O 2000";;
	"x11")  shift="redshift -x && redshift -o -O 2000";;
	*)  echo "Invalid"  && exit;;
esac 

while true; do
    hour="$(date +%H)"
    brit="$(light -G)"
    if [ "$hour" -ge 16 ] || [ "$hour" -le 7 ]
    then
	    $shift
        if [ $brit -ge 70 ]
	    then
		    notify-send "brightness too high"
	    fi
    fi
    sleep 20m
    notfy-send "lookaway!"
    sleep 20m
done &
