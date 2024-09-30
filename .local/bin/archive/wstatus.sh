#!/usr/bin/env bash
batfun(){
	bat=$(cat /sys/class/power_supply/BAT0/capacity)
	if [ "$bat" = 100 ]; then
		printf '%s' "$bat%[F]"
        elif [ "$bat" -lt 100 ];then
		printf '%s' "$bat%[C]"
	else
		printf '%s' "$bat%"
	fi
}

datefun(){
	date +"%a %d/%m %T"
}

volfun(){
	amixer get Master | grep -o [0-9]*% | head -1
}

wififun(){
	cat /proc/net/wireless | awk 'NR==3 {printf("%.0f_\n",$3*10/7)}'
}
while true; do 
	somebar -c status "$(volfun) $(wififun) $(batfun) $(datefun)"
   sleep 1
done
