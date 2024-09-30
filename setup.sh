#!/usr/bin/env bash
#write in posix sh
postsetup ()
{
        sudo systemctl enable ufw
	sudo systemctl start ufw
	sudo ufw enable
	sudo ufw default deny incoming
	sudo ufw default allow outgoing

	read -rp "Do you want xmonad?(y/n) " xmon
	if [[ $xmon == "y" || $xmon == "Y" ]]
	then
            sudo $pkg xmonad xmobar
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
	  sudo mv /etc/hosts /etc/hosts.old
	  sudo mv hosts /etc/hosts
	else
          echo "ad-blocking hosts not added"
	fi

	cd ~/
	git clone https://codeberg.org/zenex/looks
	cd looks
	sudo cp -r Future* /usr/share/icons
	sudo cp -r Material* /usr/share/themes
	sudo cp -r Hack /usr/share/fonts
}

Debian ()
{
	sudo apt update
	$pkg emacs sbcl wget curl firefox-esr htop feh redshift libreoffice libreoffice-gnome dunst libnotify4 libnotify-dev libnotify-bin scrot zathura network-manager tar zip unzip fuse3 ntfs-3g pcmanfm light keepassxc xorg libx11-dev libxft-dev libxinerama-dev ufw nnn gcc alsa-utils tlp tmux mpc mpd ncmpcpp p7zip-full dmenu xsecurelock intel-microcode libxrandr-dev arandr make trash-cli lxappearance mpv lf rxvt-unicode xterm fzf rofi wireplumber pipewire-media-session- pipewire-alsa apt-list-bugs apt-list-changes hunspell-dictionary-en-gb
	sudo systemctl disable bluetooth
	sudo systemctl enable tlp
	postsetup 
}

Fedora()
{
	$pkg neovim emacs sbcl curl wget firefox dmenu rofi make gcc htop feh redshift libreoffice dunst libnotify libnotify-devel scrot zathura zathura-devel zathura-plugins-all zathura-pdf-mupdf @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar nnn ufw iwl* pcmanfm alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g xz libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session rxvt-unicode tmux fzf tlp udisks udisks-devel trash-cli xsetroot dash xsecurelock lxappearance patch texlive-cantarell p7zip redhat-rpm-config #sway gammastep waybar rpmautospec-rpm-macros
	sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
	sudo dnf upgrade
	sudo dnf install mpv mpd mpc ncmpcpp ffmpegthumbnailer
	sudo systemctl disable firewalld
	sudo systemctl disable bluetooth
	sudo systemctl stop firewalld
	xdg-mime default org.pwmt.zathura.desktop application/pdf
	postsetup
}
	
Os()
{

osR=$(awk -F '^NAME=' '{print $2}' /etc/os-release | grep " " | sed -e 's/^"//' -e 's/"$//')
case $osR in
	"Debian GNU/Linux")  pkg="sudo apt install" && Debian ;;
	"Fedora Linux")  pkg="sudo dnf install" && Fedora;;
	*)  echo "Invalid os name" && echo "your os is :" && uname -a && exit;;
esac 
}

cp -r .xinitrc .bashrc ~/
mkdir -p ~/Downloads/
mkdir -p ~/.config && cp -r .config/. ~/.config
mkdir -p --parents ~/.local/bin && cp .local/bin/. ~/.local/bin/
Os

echo "All done!"
