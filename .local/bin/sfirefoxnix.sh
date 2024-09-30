#!/bin/sh
exec bubblewrap \
	 --unshare-pid \
	 --unshare-net \
	 --unshare-ipc \
	 --ro-bind /etc /etc \
	 --ro-bind /usr /usr \
	 --ro-bind /var /var \
	 --new-session \
	 --chdir=$HOME \
	 --dir=$HOME/Downloads \
	 --bind=$HOME/Downloads $HOME/Downloads \
	 --bind=/dev /dev \
	 --bind=/tmp /tmp \
	 --bind=/run /run \
	 --bind=/etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt \
	 --bind=/etc/machine-id /etc/machine-id \
	 --bind=/run/user/$(id -u)/pipewire-0 /run/user/$(id -u)/pipewire-0 \    # Add this line for Pipewire access
--bind=/run/user/$(id -u)/wayland-0 /run/user/$(id -u)/wayland-0 \    # Add this line for Wayland access
--file 10 firefox -no-remote -profile $HOME/.firefox-profile
