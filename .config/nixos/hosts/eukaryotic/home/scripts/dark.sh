#!/usr/bin/env bash
while true; do
	hour="$(date +%H)"
	if [ "$hour" -lt 7 ] && [ "$hour" -gt 0 ]; then
		light -S 2
		light -O
	fi
	sleep 5m
done
