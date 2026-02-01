#!/usr/bin/env bash

status=$(tailscale status)

while true; do
  if ! tailscale status > /dev/null; then
      if [ ! "$status" == "Tailscale is stopped." ]; then
	        notify-send "tailscale error"
      fi
  fi
  sleep 1m
done
