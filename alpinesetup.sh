#!/usr/bin/env bash
read -p "Are you using sudo or doas? " perm


cp -r .xinitrc .bashrc ~/
mkdir -p ~/Downloads/
cp -r Downloads/* ~/Downloads
mkdir -p ~/.config
cp -r .config/* ~/.config

$perm apk add util-linux pciutils usbutils coreutils binutils findutils grep iproute2 bash bash-doc bash-completion udisks2 udisks2-doc git make gcc g++ libx11-dev libxft-dev libxinerama-dev ncurses dbus-x11 firefox adwaita-icon-theme ttf-dejavu mandoc man-pages docs gcompat alsa-utils alsa-utils-doc alsa-lib alsaconf alsa-ucm-conf pciutils neovim emacs wget curl htop feh redshift libreoffice dunst libnotify-dev dmenu slock scrot mupdf tar zip unzip fuse3 ntfs-3g pcmanfm mpv light keepassxc p7zip ufw nnn arandr libxrandr-dev xsetroot pm-utils setxkbmap libuser #xf86-video-intel mesa-dri-gallium libva-intel-driver kbd xf86-input-libinput
$perm setup-xorg-base
read -p "what did you call the user? " USER
$perm adduser $USER audio
$perm adduser root audio
$perm adduser $USER video
$perm adduser $USER input
$perm mkdir -p /etc/acpi/LID/
$perm touch /etc/acpi/LID/00000080
$perm doas chmod 777 /etc/acpi/LID/00000080
$perm echo "#!bin/sh" >> /etc/acpi/LID/00000080
$perm echo "exec pm-suspend" >> /etc/acpi/LID/00000080
$perm doas chmod 755 /etc/acpi/LID/00000080
$perm chmod +x /etc/acpi/LID/00000080
$perm /etc/init.d/acpid start
$perm rc-update add ufw
$perm touch /etc/login.defs
$perm mkdir /etc/default
$perm touch /etc/default/useradd
echo "/bin/bash"
$perm lchsh $USER
sleep 2
$perm reboot
