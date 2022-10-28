#!/usr/bin/env bash
while true; do
hour="`date +%H`"
case $hour in
	7)  redshift -x;;
	16) redshift -x && redshift -o -O 4500;;
	20) redshift -x && redshift -o -O 3000;;
	21) redshift -x && redshift -o -O 2000;;
	22) redshift -x && redshift -o -O 2000;;
	23) redshift -x && redshift -o -O 2000;;
	00) redshift -x && redshift -o -O 2000;;
	*)  redshift -x && redshift -o -c ~/.config/redshift.conf;;
esac
sleep 60m
done &
