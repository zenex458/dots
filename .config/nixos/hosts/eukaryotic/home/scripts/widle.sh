#!/usr/bin/env bash

exec swayidle -w \
		  timeout 1800 'swaylock -f -c 141414 \
		  timeout 2100 'swaymsg "output * power off"' resume 'swaymsg "output * power on"' \
		  before-sleep 'swaylock -f -c 141414'
