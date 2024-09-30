alias nv="nvim"
alias n="nnn"
alias ls="ls -F -h -1"
alias ga="git add"
alias gc="git commit -m"
alias gp="git push -u origin main"
alias re="systemctl reboot"
alias slep="xsecurelock & systemctl suspend"
alias off="systemctl poweroff"
alias upd="doas nix-channel --update && doas nixos-rebuild switch"
alias updc="doas nixos-rebuild switch"
alias nixgc="nix-collect-garbage"
alias remoldgen="doas nix-collect-garbage -d && upd"
alias updoff="upd && sleep 2 && off"
alias enc="doas nvim /etc/nixos/configuration.nix"
alias grep="grep -i --colour=always"
alias mkdir="mkdir -pv"
alias mv="mv -iv"
alias cp="cp -iv"
alias rm="rm -iv"
alias tm="ps auxww | grep"
alias lines="ls | wc -l"
alias tk="tmux kill-session" 
alias ss="scrot -d 5 -q 100 ~/Downloads/Images/ss/%Y-%m-%d_$wx$h.jpeg"
alias sss="maim -s -m 10 ~/Downloads/Images/ss/$(date +%N).jpeg"
alias cco="gcc -Wall"
alias ytmp3="yt-dlp -x -o '%(title)s.%(ext)s' --audio-format mp3 "
alias ytt="yt-dlp --embed-thumbnail -o '%(title)s.%(ext)s' "
alias yt="yt-dlp -o '%(title)s.%(ext)s' "
alias xdup="xrandr --output HDMI-2 --same-as eDP-1"
alias dict="sdcv --data-dir ~/.config/stardict/web1913/ "
alias chnum="stat -c '%a %n'"
alias tas="tmux attach-session"
alias tls="tmux list-session"
alias tat="tmux attach -t"
alias mofat="udisksctl mount -b /dev/mmcblk0p1"
alias umofat="udisksctl unmount -b /dev/mmcblk0p1"
alias sysdlist="systemctl list-unit-files --type=service --state=enabled"
#alias mofat="doas mount -t exfat /dev/mmcblk0p1 ~/.sdcard"
alias col="setxkbmap gb -variant colemak"
alias gb="setxkbmap gb -variant qwerty"
alias ghc="ghc -dynamic"
alias py="python"
alias sb="source ~/.bashrc"
alias li="bashmount && mpd && ncmpcpp"
alias rli="mpd --kill && bashmount"
alias bd="bluetoothd -f /etc/bluetooth/main.conf"
alias hmb="nv ~/Documents/txt/hmb"
alias the="nv ~/Documents/txt/THEBEST"
alias inf="~/.config/inf.sh"
alias dr="dotnet run"
alias dow="aria2c"
alias del="trash-put"

function see_sharp (){
	mcs $1.cs && mono $1.exe 
}
function dict_def (){
	
	sdcv --data-dir ~/.config/stardict/web1913 $1 | less
}
function mu (){
	find /run/media/zenex/04C3-E2B3/ | grep "$*"
}
function inman (){
	man $1 | grep -C1 "$2"
}
function inmans (){
	man $1 | \grep -C1 $2
}

PS1="[\w]\n$ "

HISTFILE="/tmp/.bash_history"
HISTSIZE=1000
HISTFILESIZE=2000

set -o vi
bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'

export DOTNET_CLI_TELEMETRY_OPTOUT=1
export ASPNETCORE_ENVIRONMENT=Development
export NNN_FCOLORS='c1e2c42e006033f7c6d6ab27'
export NNN_BMS='h:/home/zenex;.:/home/zenex/.config;d:/home/zenex/Downloads;D:/home/zenex/Documents'
export NNN_ARCHIVE="\\.(7z|a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma|lzo|rar|rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip)$"
export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG='t:preview-tabbed'
export NNN_OPTS='HdP'
export TERMINAL="st"
export EDITOR="nvim"
export VISUAL="nvim"
export PATH=/run/current-system/sw/bin/omnisharp:$PATH
export LF_COLORS="~/Documents=01;31:~/Downloads=01;31:~/.local/share=01;31:~/.config/lf/lfrc=31:.git/=01;32:.git=32:.gitignore=32:Makefile=32:README.*=33:*.txt=34:*.md=34:ln=01;36:di=01;31:ex=01;32:"
export QT_QPA_PLATFORMTHEME=qt5ct
export _ZL_DATA='~/.local/share/.zlua'
#backup tar cf - directory | 7za a -si directory.tar.7z
#extract 7za x -so directory.tar.7z | tar xf -
eval "$(lua ~/.config/z.lua --init bash)"
if [ $TERM != "linux" ]
then
	clifm
fi
