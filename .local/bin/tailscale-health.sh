#!/usr/bin/env bash

while true; do
  sleep 3
  if ! tailscale status > /dev/null; then
      notify-send "tailscale error"
  fi
done
