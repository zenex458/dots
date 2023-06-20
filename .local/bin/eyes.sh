#!/usr/bin/env dash

##case $XDG_SESSION_TYPE in
##	"wayland")  shift="gammastep -P -o -O 2000"
##				norm="gammastep -o"
##				;;
##	*)  shift="redshift -x && redshift -o -O 2000"
##		norm="redshift -o"
##		;;
##esac 
##
##while true; do
##    hour="$(date +%H)"
##    if [ "$hour" -le 7 ] || [ "$hour" -ge 16 ]; then
##		$shift
##    else 
##	    $norm
##    fi
##    sleep 40m
##done &



while true; do
    hour="$(date +%H)"
    if [ "$hour" -ge 16 ] || [ "$hour" -le 7 ]
    then
	    redshift -x && redshift -o -O 2000
    else
        redshift -x && redshift -o -c ~/.config/redshift.conf
    fi
    sleep 40m
done &
