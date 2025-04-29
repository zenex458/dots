#!/usr/bin/env bash
echo "Clearing swap please wait one minute..."
sudo swapoff -a
sleep 30
sudo swapon -a
free -mh
echo "Swap cleared!"
