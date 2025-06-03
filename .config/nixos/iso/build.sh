#!/usr/bin/env bash

printf "Building..."
nix build

path=$(realpath result/iso/*.iso)

read -rp "Copy iso to this directory?(y/n) " answ
if [[ $answ == "y" ]]; then
	cp "$path" "$PWD"
else
        ls
fi
