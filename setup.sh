#!/usr/bin/env bash

#ISC License
#
#Copyright (c) 2023 zenex
#
#Permission to use, copy, modify, and distribute this software for any
#purpose with or without fee is hereby granted, provided that the above
#copyright notice and this permission notice appear in all copies.
#
#THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
#WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
#MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
#ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
#WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
#ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
#OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
set -e
postsetup() {
	sudo systemctl enable ufw
	sudo systemctl start ufw
	sudo ufw enable
	sudo ufw default deny incoming
	sudo ufw default allow outgoing
	read -rp "Do you want xmonad?(y/n) " xmon
	if [[ $xmon == "y" || $xmon == "Y" ]]; then
		sudo $pkg xmonad xmobar xautolock
	else
		echo "xmonad not added"
	fi

	read -rp "Do you want sway?(y/n) " sw
	if [[ $sw == "y" || $sw == "Y" ]]; then
		sudo $pkg -y sway foot gammastep xwayland bemenu swaylock swayidle xdg-desktop-portal-wlr grim slurp imv emacs-pgtk python3-i3ipc
	else
		echo "sway not added"
	fi

	cd ~/ || cd $HOME || echo "$HOME not found, please set $HOME variable"
	git clone https://codeberg.org/zenex/looks
	cd looks || echo "looks directory not found"
	sudo cp -r Future* /usr/share/icons
	sudo cp -r plan9 /usr/share/icons
	sudo cp -r Cloudy* /usr/share/themes
	sudo cp -r IosevkaFixedCustom /usr/share/fonts
}

Debian_install_firefox() {
	sudo apt install firefox || sudo apt install firefox-esr || echo "Issue with package selection"

}
#add arch linux
Debian() {
	sudo apt update
	$pkg emacs elpa-pdf-tools-server astyle shfmt ormolu qt5ct pandoc wget curl htop libreoffice libreoffice-gnome dunst libnotify4 libnotify-dev libnotify-bin zathura bc network-manager tar zip unzip fuse3 ntfs-3g thunar light keepassxc xorg libx11-dev libxft-dev libxinerama-dev ufw nnn gcc alsa-utils tlp tmux zsh zoxide mpc mpd ncmpcpp p7zip-full intel-microcode make trash-cli mpv xterm fzf wireplumber pulsemixer pipewire-alsa apt-listbugs apt-listchanges hunspell-en-gb apparmor-profiles apparmor-profiles-extra libseccomp-dev seccomp libbpf-dev lxqt-policykit libpam-tmpdir opensnitch clamav chrony fd-find libtree-sitter-dev firejail thunderbird || echo "Issue with package selection" # xbanish xautolock feh redshift needrestart debian-goodies fail2ban rkhunter smartmontools smart-notifier
	Debian_install_firefox
	systemctl --user restart wireplumber pipewire pipewire-pulse
	sudo systemctl disable bluetooth
	sudo systemctl enable tlp
	postsetup
}

Fedora() {
	sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
	sudo dnf upgrade
	sudo dnf install --allowerasing mpv mpd mpc ncmpcpp ffmpegthumbnailer
	$pkg --allowerasing emacs curl wget firefox dmenu make gcc htop feh redshift libreoffice dunst libnotify libnotify-devel scrot zathura zathura-devel zathura-plugins-all zathura-pdf-mupdf @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar nnn ufw iwl* pcmanfm alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session rxvt-unicode tmux fzf tlp udisks udisks-devel trash-cli xsetroot dash xsecurelock lxappearance patch texlive-cantarell p7zip redhat-rpm-config rpmautospec-rpm-macros 
	sudo systemctl disable firewalld
	sudo systemctl disable bluetooth
	sudo systemctl stop firewalld
	xdg-mime default org.pwmt.zathura.desktop application/pdf
	postsetup
}

Os() {

	#osR=$(awk -F '^NAME=' '{print $2}' /etc/os-release | grep " " | sed -e 's/^"//' -e 's/"$//')
	#awk will use the "=" as a marker, it will look for the second occurance of a string that begins with NAME= and remove the sourounding quotes and print it
	osR=$(awk -F '=' '/^NAME/ {gsub(/"/, "", $2); print $2}' /etc/os-release)
	case $osR in
	"Debian GNU/Linux") pkg="sudo apt install" && Debian ;;
	"Fedora Linux") pkg="sudo dnf install" && Fedora ;;
	*) echo "Invalid os name" && echo "your os is :" && uname -a && exit ;;
	esac
}

Os
cp -r .xinitrc .bashrc ~/
mkdir -p ~/Downloads/
mkdir -p ~/.config && cp -r .config/. ~/.config
mkdir -p ~/.local/bin && cp -r .local/bin/. ~/.local/bin/
sudo cp /etc/chrony/chrony.conf /etc/chrony/chrony.conf.old
sudo cp ~/dots/root/30_security-misc.conf /etc/modprobe.d/
sudo cp ~/dots/root/chrony.conf /etc/chrony/chrony/
sudo cp ~/dots/root/tlp.conf /etc/tlp/
sudo cp ~/dots/root/sysctl.conf /etc/sysctl.d/
echo "All done!"
