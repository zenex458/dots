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
		sudo $pkg sway foot waybar gammastep xwayland bemenu swaylock swayidle fuzzel
	else
		echo "sway not added"
	fi

	read -rp "Do you want vim-plug?(y/n) " vplug
	if [[ $vplug == "y" || $vplug == "Y" ]]; then
		sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
 	      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	else
		echo "vim-plugged not added"
	fi

	read -rp "Do you want an ad-blocking hosts file?(y/n) " ahosts
	if [[ $ahosts == "y" || $ahosts == "Y" ]]; then
		curl -LO "https://codeberg.org/zenex/hosts/raw/branch/main/hosts"
		sudo mv /etc/hosts /etc/hosts.old
		sudo mv hosts /etc/hosts
	else
		echo "ad-blocking hosts not added"
	fi

	read -rp "Do you want to install ALL FONTS (this will install all the texlive-fonts so it will install over 200 packages, around 6GB in size)?(y/n) " fontstex
	if [[ $fontstex == "y" || $fontstex == "Y" ]]; then
		sudo $pkg texlive-full || echo "Issue with package selection"
	else
		echo "fonts not added"
	fi

	cd ~/ || cd $HOME || echo "$HOME not found, please set $HOME variable"
	git clone https://codeberg.org/zenex/looks
	cd looks || echo "looks directory not found"
	sudo cp -r Future* /usr/share/icons
	sudo cp -r plan9 /usr/share/icons
	sudo cp -r Cloudy* /usr/share/themes
	sudo cp -r iosevka /usr/share/fonts
}

Debian_install_firefox() {
	sudo apt install firefox || sudo apt install firefox-esr || echo "Issue with package selection"

}

Debian() {
	sudo apt update
	$pkg emacs elpa-pdf-tools-server astyle shfmt qt5ct pandoc xbanish xautolock wget curl htop feh redshift libreoffice libreoffice-gnome dunst libnotify4 libnotify-dev libnotify-bin scrot zathura bc network-manager tar zip unzip fuse3 ntfs-3g pcmanfm light keepassxc xorg libx11-dev libxft-dev libxinerama-dev ufw nnn gcc alsa-utils tlp tmux zsh zoxide mpc mpd ncmpcpp p7zip-full suckless-tools xsecurelock intel-microcode make trash-cli lxappearance mpv xterm fzf wireplumber pulsemixer pipewire-alsa apt-listbugs apt-listchanges hunspell-dictionary-en-gb apparmor-profiles apparmor-profiles-extra libseccomp-dev libbpf-dev needrestart debian-goodies lxqt-policykit libpam-tmpdir fail2ban opensnitch clamav fzy fd-find rkhunter smartmontools smart-notifier || echo "Issue with package selection"
	Debian_install_firefox
	systemctl --user restart wireplumber pipewire pipewire-pulse
	sudo systemctl disable bluetooth
	sudo systemctl enable tlp
	postsetup
}

Fedora() {
	sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
	sudo dnf upgrade
	sudo dnf install mpv mpd mpc ncmpcpp ffmpegthumbnailer
	$pkg neovim emacs curl wget firefox dmenu rofi make gcc htop feh redshift libreoffice dunst libnotify libnotify-devel scrot zathura zathura-devel zathura-plugins-all zathura-pdf-mupdf @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar nnn ufw iwl* pcmanfm alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g xz libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session rxvt-unicode tmux fzf tlp udisks udisks-devel trash-cli xsetroot dash xsecurelock lxappearance patch texlive-cantarell p7zip redhat-rpm-config || echo "Issue with package selection" #rpmautospec-rpm-macros sbcl
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

cp -r .xinitrc .bashrc ~/
mkdir -p ~/Downloads/
mkdir -p ~/.config && cp -r .config/. ~/.config
mkdir -p --parents ~/.local/bin && cp -r .local/bin/. ~/.local/bin/
Os

echo "All done!"
