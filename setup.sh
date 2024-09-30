#do this last: make it so you only need the "setup.sh" file
#put everything into functions, make pkgs first

suck_less ()
{
	cd ~/.config/dwm &&
	$perm make install &&
	sleep 2 &&
	cd ~/.config/slstatus &&
	$perm make install &&
	sleep 2 &&
	cd ~/.config/st &&
	$perm make install &&
	sleep 2
}

Arch ()
{
	$pkg neovim wget firefox kitty rofi htop neofetch zsh feh mono thunar file-roller playerctl redshift libreoffice slock xclip dunst libnotify scrot zathura bc qualculate-gtk qemu virt-manager chromium qutebrowser cmus yt-dlp xorg-xbacklight zip unzip exa bat procs zettlr fuse3 networkmanager mpv newsboat ghc light lynx httrack keepassxc p7zip xorg xorg-server git xorg-xinit
	if [ "$de" = "D" ]
	then
		$pkg base-devel git libx11 libxft xorg-server xorg-xinit terminus-font && suck_less 
	elif [ "$de" = "M" ]
	then
		$pkg mate mate-extra

	elif [ "$de" = "XF" ]
	then
		$pkg xfce4 xfce4-goodies
	
	elif [ "$de" = "X" ]
	then
		$pkg xmonad xmonad-contrib xmonad-utils xmobar &&
		cp .xmonad ~/

	else
		echo "wrong letter"
	fi
}

Debian ()
{
	sudo apt update &&
	$pkg neovim wget firefox-esr kitty rofi htop neofetch zsh feh mono-complete file-roller playerctl redshift libreoffice suckless-tools xclip dunst libnotify4 libnotify-dev libnotify-bin scrot zathura bc wcalc qemu virt-manager chromium qutebrowser cmus yt-dlp xbacklight zip unzip fuse3 network-manager mpv newsboat ghc light lynx httrack keepassxc p7zip git light exa bat xorg

	if [ "$de" = "D" ]
	then
		$pkg "dwm" "libx11-dev" "libxft-dev" "libxinerama-dev" && suck_less;  
	elif [ "$de" = "M" ]
	then
		$pkg mate-desktop-environment mate-desktop-environment-extras

	elif [ "$de" = "XF" ]
	then
		$pkg xfce4 xfce4-goodies
	
	elif [ "$de" = "X" ]
	then
		$pkg xmonad xmobar libx11-dev libxft-dev libxinerama-dev libghc-xmonad-contrib-dev
		cp .xmonad ~/
	else
		echo "wrong letter"
	fi
}

Fedora()
{
	$pkg neovim curl wget firefox slock dmenu make gcc htop feh playerctl redshift libreoffice dunst libnotify libnotify-devel scrot mupdf bc @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar zoxide nnn ufw iwl* thunar alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g xz

	#sway gammastep waybar
	$perm dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm && 
	$perm dnf upgrade &&
	$perm dnf install mpv cmus &&
	cd ~/.config/batsignal &&
	$perm make clean install &&
	cd ~/ &&

	$perm systemctl disable firewalld &&
	$perm systemctl stop firewalld &&
	$perm systemctl enable ufw &&
	$perm systemctl start ufw &&
	$perm ufw enable &&
	$perm ufw default deny incoming &&
	$perm ufw default allow outgoing &&
	#$perm systemctl enable trim.timer &&
	sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
 	      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim' &&

	

	if [ "$de" = "D" ]
	then
		$pkg libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session && suck_less;  
	elif [ "$de" = "M" ]
	then
		$pkg @mate-desktop @mate-applications

	elif [ "$de" = "XF" ]
	then
		$pkg xfce4 xfce4-goodies
	
	elif [ "$de" = "X" ]
	then
		$pkg libX11-devel libXft-devel libXinerama-devel libXrandr-devel libXScrnSaver-devel stack
		mkdir -p ~/.config/xmonad && cd ~/.config/xmonad
		cp .xmonad ~/.config/xmonad
		git clone https://github.com/xmonad/xmonad
		git clone https://github.com/xmonad/xmonad-contrib
		stack init
		stack install
		mkdir -p ~/.config/xmobar && cd ~/.config/xmobar
		git clone https://github.com/jaor/xmobar
		cd xmobar
		stack init
		stack install --flag xmobar:all_extensions
	else
		echo "wrong letter"
	fi
}

OpenBSD()
{
	$pkg neovim wget curl firefox alacritty dmenu htop feh thunar file-roller redshift libreoffice slock xclip dunst libnotify scrot mupdf chromium cmus yt-dlp tar zip unzip exfat-fuse mpv keepassxc ntfs_3g nnn xarchiver xclip xsel lxappearance

	if [ "$de" = "D" ]
	then
		suck_less 
	elif [ "$de" = "M" ]
	then
		$pkg mate

	elif [ "$de" = "XF" ]
	then
		$pkg xfce4 xfce4-goodies
	
	elif [ "$de" = "X" ]
	then
		$pkg xmonad xmonad-contrib xmobar &&
		cp .xmonad ~/

	else
		echo "wrong letter"
	fi
}




other ()
{
	cp -r .xinitrc .bashrc ~/ &&
	mkdir -p ~/Downloads/ &&
	cp -r Downloads/* ~/Downloads &&
	mkdir -p ~/.config &&
	cp -r .config/* ~/.config
	cd ~/.confg/batsignal &&
	$perm make clean install &&
	sleep 2

}

echo "Welcome to my config script!"
read -p "Are you using sudo or doas? " perm


cp -r .xinitrc .bashrc ~/ &&
mkdir -p ~/Downloads/ &&
cp -r Downloads/* ~/Downloads &&
mkdir -p ~/.config &&
cp -r .config/* ~/.config && 


echo "(D)WM"
echo "(M)ate"
echo "(XF)ce"
echo "(X)monad"
read -p "Which desktop environment do you want? " de

echo " "
echo "(A)rch"
echo "(D)ebian"
echo "(F)edora"
echo "(O)penBSD"                                           
echo "(Op)enSuse"
echo "(V)oid"
echo "(Z)other"

read -p "Please enter your operating system: " os

case $os in
	A)  pkg="$perm pacman -S" && Arch;;
	D)  pkg="$perm apt install" && Debian ;;
	F)  pkg="$perm dnf install" && Fedora;;
	O)  pkg="$perm pkg_add" && OpenBSD ;;
	Op) pkg="$perm zypper instal" && OpenSUSE;;
	V)  pkg="$perm xbps-install" && Void;;
	Z)  other
	* echo "errm thats not right!" && echo "your os is :" && uname -a && exit;;
esac &&
cd ~/ && echo "All done!"
