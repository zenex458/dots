#!/usr/bin/env bash
suck_less ()
{
	cd ~/.config/dwm
	$perm make install
	cd ~/
}

postsetup ()
{
	$perm ufw enable &&
	$perm ufw default deny incoming &&
	$perm ufw default allow outgoing &&
	read -p "Do you want vim-plug?(y/n) " vplug
	if [[ $vplug == "y" || $vplug == "Y" ]]
	then
          sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
 	      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	else
	  echo "vim-plugged not added"
	fi

	read -p "Do you want an ad-blocking hosts file?(y/n) " ahosts
	if [[ $ahosts == "y" || $ahosts == "Y" ]]
	then
          wget https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts &&
	  $perm mv /etc/hosts /etc/hosts.old && 
	  $perm mv hosts /etc/hosts
	else
          echo "ad-blocking hosts not added"
	fi

	cp ~/dots/Downloads/vimix-dark.tar.7z ~/Downloads/ &&
	cd ~/Downloads/ &&
	7za x -so vimix-dark.tar.7z | tar xf - &&
	$perm mv vimix-dark /usr/share/themes/ &&
	rm -r vimix-dark.tar.7z &&
	cd ~/
}

Arch ()
{
	$pkg neovim wget curl firefox htop feh thunar sudo ufw playerctl redshift libreoffice slock dunst libnotify scrot mupdf bc cmus yt-dlp zip unzip tar fuse3 ntfs-3g exfat-utils networkmanager mpv light keepassxc xorg xorg-server xorg-xinit base-devel git libx11 libxft xorg-server xorg-xinit terminus-font lua dmenu pipewire pipewire-alsa pipewire-pulse && 
	suck_less &&
        $perm systemctl enable ufw &&
	$perm systemctl start ufw
	postsetup &&
	$perm systemctl enable fstrim.timer

}

Debian ()
{
	$perm apt update &&
	$pkg neovim wget curl firefox htop feh redshift libreoffice libreoffice-gnome dunst libnotify4 libnotify-dev libnotify-bin scrot zathura network-manager tar zip unzip fuse3 ntfs-3g pcmanfm mpv light keepassxc xorg libx11-dev libxft-dev libxinerama-dev ufw nnn gcc alsa-utils thermald tlp tmux mpd mpd ncmpcpp p7zip-full rxvt-unicode dmenu xsecurelock && #intel-microcode kdeconnect libxrandr-dev arandr

	suck_less &&  
	$perm systemctl disable bluetooth &&
	$perm systemctl enable tlp
	#sudo systemctl enable fstrim.timer &&
        $perm systemctl enable ufw &&
	$perm systemctl start ufw &&
	postsetup 
}

Fedora()
{
	$pkg neovim curl wget firefox slock dmenu make gcc htop feh redshift libreoffice dunst libnotify libnotify-devel scrot zathura @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar nnn ufw iwl* pcmanfm alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g xz libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session rxvt-unicode tmux fzf tlp udisks udisks-devel
        suck_less && 
	#sway gammastep waybar
	$perm dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm && 
	$perm dnf upgrade &&
	$perm dnf install mpv mpd mpc ncmpcpp &&
	$perm systemctl disable firewalld &&
	$perm systemctl disable bluetooth &&
	$perm systemctl stop firewalld &&
        $perm systemctl enable ufw &&
	$perm systemctl start ufw &&
	postsetup  
}
	

read -p "Are you using sudo or doas? " perm

cp -r .xinitrc .bashrc ~/
mkdir -p ~/Downloads/
cp -r Downloads/* ~/Downloads
mkdir -p ~/.config
cp -r .config/* ~/.config

echo " "
echo "(A)rch"
echo "(D)ebian"
echo "(F)edora"                               
echo "(Z)other"


read -p "Please enter your os: " os

case $os in
	A)  pkg="$perm pacman -S" && Arch;;
	D)  pkg="$perm apt install" && Debian ;;
	F)  pkg="$perm dnf install" && Fedora;;
	Z)  exit;;
	*)  echo "Invalid os name" && echo "your os is :" && uname -a;;
esac &&
cd ~/ && echo "All done!"
