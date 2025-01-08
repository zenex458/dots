#!/usr/bin/env bash
sudo swapoff -a
echo "Clearing swap please wait 30 seconds..."
sleep 30
sudo swapon -a
echo "Swap cleared!"
free -mh
