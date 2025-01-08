#!/usr/bin/env bash

#kdecpath="/usr/lib/x86_64-linux-gnu/libexec/kdeconnectd"
#kdecpath="/usr/lib/kdeconnectd"
kdecppath="/nix/store/zh6n6jdi4l9cxwz2hyqgxp3zyqsndiys-kdeconnect-kde-23.04.3/libexec/kdeconnectd"

choice=$(printf 'On\nOff' | dmenu -i -p "Turn Kdeconnect")

if [ "$choice" = "On" ]; then
	if ! pgrep kdeconnectd; then
		"$kdecpath"
	fi
elif [ "$choice" = "Off" ]; then
	pkill kdeconnectd
fi
