#!/usr/bin/env dash


#check if wayland is being used
if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
	wayland=true
else
	wayland=false
fi


bluelight_on(){
	if [ "$wayland" = true ]; then
        gammastep -P -o -O 2000
    	else
        redshift -o -O 2000
	fi
}

bluelight_off(){
	if [ "$wayland" = true ]; then
        gammastep -x
    	else
        redshift -x
	fi
}
#on exit it will turn off the filter
exitfn(){
	bluelight_off
	exit
}

trap "exitfn" INT

#This will only run the bluelight filter if the time is before 7am or after 4pm and if it hasn't been run before, the x value will increment when the bluelight filter has been enabled and it will decrease when it is outside of  the hours mentioned above. This is done so i don't have to reset the screen before applying the filter, as this causes a flash which is harmfull. Plus every 20mins it will send a notification saying to look away.
x=0
while true; do
	hour="$(date +%H)"
	if [ "$hour" -le 7 ] || [ "$hour" -ge 16 ] && [ "$x" -eq 0 ]; then
		bluelight_on
		x=$((x+1))
	elif [ "$hour" -ge 7 ] && [ "$hour" -le 16 ] && [ "$x" -eq 1 ]; then
		bluelight_off
		x=$((x-1))
	fi
	notify-send "Look Away!"
	sleep 20m
	notify-send "Look Away!"
	sleep 20m
done
