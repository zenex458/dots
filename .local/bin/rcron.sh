#!/usr/bin/env dash

#PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
export DISPLAY=:0.0

case $XDG_SESSION_TYPE in
	"wayland")  shift="gammastep -P -o -O 2000" && norm="gammastep -o";;
	*)  shift="redshift -x && redshift -o -O 2000" && norm="redshift -o";;
esac 

    hour="$(date +%H)"
    if [ "$hour" -ge 16 ] || [ "$hour" -le 7 ]
    then
	    $shift
    else
	    $norm
    fi
