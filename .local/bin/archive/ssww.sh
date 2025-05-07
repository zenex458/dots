#!/usr/bin/env bash

menu="bemenu -l 4 -i --no-exec -p"

sswd(){
	useri=$(echo " " | $menu "How long is the delay in seconds?")
	sleep "$useri" && grim ~/Downloads/Images/ss/"$(date +'%s.png')"
}

sswdwg(){
	useri=$(echo " " | $menu "How long is the delay in seconds?")
	sleep "$useri" && grim -g "$(slurp)" ~/Downloads/Images/ss/"$(date +'%s.png')"
}

sel=$(echo "Screenshot\nScreenshot With Grab\nScreenshot With Delay\nScreenshot With Delay With Grab" | $menu " ")

case "$sel" in
	Screenshot) grim ~/Downloads/Images/ss/"$(date +'%s.png')";;
	Screenshot\ With\ Grab) grim -g "$(slurp)" ~/Downloads/Images/ss/"$(date +'%s.png')";;
	Screenshot\ With\ Delay) sswd;;
	Screenshot\ With\ Delay\ With\ Grab) sswdwg;;
	*) exit
esac
