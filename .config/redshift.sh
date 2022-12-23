#!/usr/bin/env dash
while true; do
hour="$(date +%H)"
if [ "$hour" -ge 16 ] || [ "$hour" -le 7 ]
then
	redshift -x && redshift -o -O 2000
fi
sleep 40m
done &
