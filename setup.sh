suck_less ()
{
	cd ~/.config/dwm
	$perm make install &&
	sleep 2 &&
	cd ~/.config/slstatus &&
	$perm make install &&
	sleep 2 &&
	cd ~/.config/st &&
	$perm make install &&
	sleep 2 &&
	cd ~/
}

postsetup ()
{
	$perm systemctl enable ufw &&
	$perm systemctl start ufw &&
	$perm ufw enable &&
	$perm ufw default deny incoming &&
	$perm ufw default allow outgoing &&
	sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
 	      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim' &&
	wget https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts &&
	$perm mv /etc/hosts /etc/hosts.old && 
	$perm mv hosts /etc/hosts
	cp ~/dots/Downloads/FiraMono.tar.7z ~/dots/Downloads/vimix-dark.tar.7z ~/Downloads/ &&
	cd ~/Downloads/ &&
	7za x -so FiraMono.tar.7z | tar xf -
	$perm mv FiraMono /usr/share/fonts/ &&
	7za x -so vimix-dark.tar.7z | tar xf -
	$perm mv vimix-dark /usr/share/themes/ &&
	rm -r vimix-dark.tar.7z FiraMono.tar.7z &&
	cd ~/
}

Arch ()
{
	$pkg neovim wget curl firefox htop feh thunar sudo ufw playerctl redshift libreoffice slock dunst libnotify scrot mupdf bc cmus yt-dlp zip unzip tar fuse3 ntfs-3g exfat-utils networkmanager mpv light keepassxc xorg xorg-server xorg-xinit base-devel git libx11 libxft xorg-server xorg-xinit terminus-font lua dmenu pipewire pipewire-alsa pipewire-pulse && 
	suck_less &&
	postsetup &&
	sudo systemctl enable fstrim.timer
}

Debian ()
{
	sudo apt update &&
	$pkg neovim wget curl firefox-esr htop feh redshift libreoffice libreoffice-gnome dunst libnotify4 libnotify-dev libnotify-bin scrot mupdf network-manager lua5.4 bc cmus tar zip unzip fuse3 ntfs-3g thunar mpv light keepassxc sdcv p7zip-full acpi xorg libx11-dev libxft-dev libxinerama-dev ufw make gcc nnn arandr libxrandr-dev alsa-utils intel-microcode &&

	suck_less &&  
	sudo systemctl disable bluetooth &&
	sudo systemctl enable fstrim.timer &&

	postsetup 
}

Fedora()
{
	$pkg neovim curl wget firefox slock dmenu make gcc htop feh playerctl redshift libreoffice dunst libnotify libnotify-devel scrot mupdf bc @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar zoxide nnn ufw iwl* thunar alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g xz libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session && suck_less; 
	#sway gammastep waybar
	$perm dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm && 
	$perm dnf upgrade &&
	$perm dnf install mpv cmus &&
	$perm systemctl disable firewalld &&
	$perm systemctl stop firewalld &&
	postsetup  
}
	
other ()
{
	cp -r .xinitrc .bashrc ~/ &&
	mkdir -p ~/Downloads/ &&
	cp -r Downloads/* ~/Downloads &&
	mkdir -p ~/.config &&
	cp -r .config/* ~/.config
	cd ~/.config/batsignal &&
	$perm make clean install &&
	sleep 2

}

read -p "Are you using sudo or doas? " perm


cp -r .xinitrc .bashrc ~/ &&
mkdir -p ~/Downloads/ &&
cp -r Downloads/* ~/Downloads &&
mkdir -p ~/.config &&
cp -r .config/* ~/.config && 

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
	Z)  other
	* echo "Invalid os name" && echo "your os is :" && uname -a ;;
esac &&
cd ~/ && echo "All done!"
