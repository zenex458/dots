#!/usr/bin/env bash
set -e
postsetup() {
	sudo systemctl enable ufw
	sudo systemctl start ufw
	sudo ufw enable
	sudo ufw default deny incoming
	sudo ufw default allow outgoing
	read -rp "Do you want xmonad?(y/n) " xmon
	if [[ $xmon == "y" || $xmon == "Y" ]]; then
		sudo "$pkg" xmonad xmobar xautolock
	else
		echo "xmonad not added"
	fi

	read -rp "Do you want sway?(y/n) " sw
	if [[ $sw == "y" || $sw == "Y" ]]; then
		sudo "$pkg" -y sway foot gammastep xwayland bemenu swaylock swayidle xdg-desktop-portal-wlr grim slurp imv emacs-pgtk python3-i3ipc
	else
		echo "sway not added"
	fi

	cd ~/ || cd "$HOME" || echo "$HOME not found, please set $HOME variable"
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
	sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-"$(rpm -E %fedora)".noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-!"$(rpm -E %fedora)".noarch.rpm
	sudo dnf upgrade
	sudo dnf install --allowerasing mpv mpd mpc ncmpcpp ffmpegthumbnailer
	$pkg --allowerasing emacs curl wget firefox dmenu make gcc htop feh redshift libreoffice dunst libnotify libnotify-devel scrot zathura zathura-devel zathura-plugins-all zathura-pdf-mupdf @base-x yt-dlp zip unzip fuse3 NetworkManager-tui NetworkManager-wifi light keepassxc tar nnn ufw iwl* pcmanfm alsa-firmware alsa-lib alsa-lib-devel alsa-utils xterm ntfs-3g libX11-devel libXft-devel libXinerama-devel xorg-x11-xinit-session rxvt-unicode tmux fzf tlp udisks udisks-devel trash-cli xsetroot dash xsecurelock lxappearance patch texlive-cantarell p7zip redhat-rpm-config rpmautospec-rpm-macros
	sudo systemctl disable firewalld
	sudo systemctl disable bluetooth
	sudo systemctl stop firewalld
	xdg-mime default org.pwmt.zathura.desktop application/pdf
	postsetup
}

NixOS() {
    if  whoami -ne "root"; then
        echo "Please run this script as root."
        exit
    fi
    cat .config/nixos/hosts/nidus/disko-config.nix
	  read -rp "Is this disk configuration correct, if you click yes ALL DATA WILL BE REMOVED?(y/n)" disko
    if [ "$disko" == n ]; then
        nano .config/nixos/hosts/nidus/disko-config.nix

    fi
    nixos-generate-config --no-filesystems --show-hardware-config > .config/nixos/hosts/nidus/hardware-configuration.nix
    nix  --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko .config/nixos/hosts/nidus/disko-config.nix
	  read -rp "Enter username: " usrname
    sed -i '23,27 s/^/#/' .config/nixos/hosts/nidus/configuration.nix
    sed -i '22 s/#//' .config/nixos/hosts/nidus/configuration.nix
    find .config/nixos -type f -exec sed -i s/zenex/"$usrname"/g {} +
	  mkdir -p /mnt/persistent/etc
    cp -r .config/nixos /mnt/persistent/etc
	  mkdir -p /mnt/persistent/var/keys
	  echo "Enter username password:"
	  mkpasswd -m yescrypt > /mnt/persistent/var/keys/"${usrname}P"
	  read -rp "Do you want root to have the same password as your user?(y/n)" rpasswd
    if [ "$rpasswd" == y ]; then
        cp /mnt/persistent/var/keys/"${usrname}P" /mnt/persistent/var/keys/rootP
    else
	      mkpasswd -m yescrypt > /mnt/persistentvar/keys/rootP
    fi
    nixos-install --no-root-password --root /mnt --flake '/mnt/persistent/etc/nixos#nidus'
    exit
}

Os() {

	#osR=$(awk -F '^NAME=' '{print $2}' /etc/os-release | grep " " | sed -e 's/^"//' -e 's/"$//')
	#awk will use the "=" as a marker, it will look for the second occurance of a string that begins with NAME= and remove the sourounding quotes and print it
	osR=$(awk -F '=' '/^NAME/ {gsub(/"/, "", $2); print $2}' /etc/os-release)
	case $osR in
	"Debian GNU/Linux") pkg="sudo apt install" && Debian ;;
	"Fedora Linux") pkg="sudo dnf install" && Fedora ;;
  "NixOS") NixOS ;;
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

