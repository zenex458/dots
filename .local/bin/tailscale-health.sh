#!/usr/bin/env bash

status=$(tailscale status)

while true; do
  sleep 1m
  if ! tailscale status > /dev/null; then
      if [ ! "$status" == "Tailscale is stopped." ]; then
	        notify-send "tailscale error"
      fi
  fi
done
