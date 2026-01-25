#!/usr/bin/env bash

sleep 3

if ! tailscale status > /dev/null; then
    notify-send "tailscale error"
fi
