#!/usr/bin/env bash
#write in posix sh
postsetup ()
{
        $perm systemctl enable ufw
	$perm systemctl start ufw
	$perm ufw enable
	$perm ufw default deny incoming
	$perm ufw default allow outgoing

	read -rp "Do you want xmonad?(y/n) " xmon
	if [[ $xmon == "y" || $xmon == "Y" ]]
	then
            $perm dnf install xmonad
	else
	  echo "xmonad not added"
	fi

	read -rp "Do you want vim-plug?(y/n) " vplug
	if [[ $vplug == "y" || $vplug == "Y" ]]
	then
          sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
 	      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	else
	  echo "vim-plugged not added"
	fi

	read -rp "Do you want an ad-blocking hosts file?(y/n) " ahosts
	if [[ $ahosts == "y" || $ahosts == "Y" ]]
	then
          curl -LO https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts
	  $perm mv /etc/hosts /etc/hosts.old
	  $perm mv hosts /etc/hosts
	else
          echo "ad-blocking hosts not added"
	fi

	cd ~/
	git clone https://codeberg.org/zenex/looks
	cd looks
	$perm cp -r Future* Material* /usr/share/icons
	$perm cp -r Hack /usr/share/fonts
}

Arch ()
{
	$pkg neovim wget curl firefox htop feh thunar sudo ufw playerctl redshift libreoffice slock dunst libnotify scrot mupdf bc cmus yt-dlp zip unzip tar fuse3 ntfs-3g exfat-utils networkmanager mpv light keepassxc xorg xorg-server xorg-xinit base-devel git libx11 libxft xorg-server xorg-xinit terminus-font lua dmenu pipewire pipewire-alsa pipewire-pulse
	suck_less
	postsetup

}

Debian ()
{
	$perm apt update
	$pkg emacs sbcl wget curl firefox-esr htop feh redshift libreoffice libreoffice-gnome dunst libnotify4 libnotify-dev libnotify-bin scrot zathura network-manager tar zip unzip fuse3 ntfs-3g pcmanfm light keepassxc xorg libx11-dev libxft-dev libxinerama-dev ufw nnn gcc alsa-utils tlp tmux mpc mpd ncmpcpp p7zip-full dmenu xsecurelock intel-microcode libxrandr-dev arandr make trash-cli lxappearance mpv

	suck_less
	$perm systemctl disable bluetooth
	$perm systemctl enable tlp
	postsetup 
}

Fedora()
{
	$pkg neovim emacs sbcl curl wget firefox dmenu rofi make gcc htop feh redshift libreoffice dunst libnotify libnotify-devel scrot zathura zathura-devel zathura-plugins-all zathura-pdf-mupdf @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar nnn ufw iwl* pcmanfm alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g xz libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session rxvt-unicode tmux fzf tlp udisks udisks-devel trash-cli xsetroot dash xsecurelock lxappearance patch texlive-cantarell p7zip ffmpegthumbnailer redhat-rpm-config #sway gammastep waybar rpmautospec-rpm-macros
	$perm dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
	$perm dnf upgrade
	$perm dnf install mpv mpd mpc ncmpcpp
	$perm systemctl disable firewalld
	$perm systemctl disable bluetooth
	$perm systemctl stop firewalld
	xdg-mime default org.pwmt.zathura.desktop application/pdf
	postsetup
}
	
Menu()
{
echo " "
echo "(A)rch"
echo "(D)ebian"
echo "(F)edora"                               
echo "(Z)other"
read -rp "Please enter your os: " os

case $os in
	A)  pkg="$perm pacman -S" && Arch;;
	D)  pkg="$perm apt install" && Debian ;;
	F)  pkg="$perm dnf install" && Fedora;;
	Z)  exit;;
	*)  echo "Invalid os name" && echo "your os is :" && uname -a && Menu;;
esac 
}

read -rp "Are you using sudo or doas? " perm

cp -r .xinitrc .bashrc ~/
mkdir -p ~/Downloads/
mkdir -p ~/.config && cp -r .config/. ~/.config
Menu

echo "All done!"
