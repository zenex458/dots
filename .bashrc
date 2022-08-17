alias nv="nvim"
alias n="tmux new-session "nnn""
alias ls="ls -F"
alias ga="git add"
alias gc="git commit -m"
alias gp="git push -u origin main"
alias re="systemctl reboot"
alias slep="xsecurelock & systemctl suspend"
alias off="systemctl poweroff"
alias upd="doas nix-channel --update && doas nixos-rebuild switch"
alias nixgc="nix-collect-garbage"
alias remoldgen="doas nix-collect-garbage -d && upd"
alias updoff="upd && sleep 2 && off"
alias cat="cat -n"
alias grep="grep -i --colour=always"
alias mkdir="mkdir -pv"
alias mv="mv -iv"
alias cp="cp -iv"
alias rm="rm -iv"
alias tm="ps auxww | grep"
alias lines="ls | wc -l"
alias tk="tmux kill-session" 
alias scro="scrot -d 5 -q 100 '%Y-%m-%d_$wx$h.jpeg'"
alias cco="gcc -Wall"
alias ytmp3="yt-dlp -x -o '%(title)s.%(ext)s' --audio-format mp3  --audio-quality 0 "
alias yt="yt-dlp -o '%(title)s.%(ext)s' "
alias xdup="xrandr --output HDMI-2 --same-as eDP-1"
alias dict="sdcv --data-dir ~/.config/stardict/web1913/ "
alias chnum="stat -c '%a %n'"
alias tas="tmux attach-session"
alias tls="tmux list-session"
alias tat="tmux attach -t"
alias mofat="udisksctl mount -b /dev/mmcblk0p1"
alias umofat="udisksctl unmount -b /dev/mmcblk0p1"
#alias mofat="doas mount -t exfat /dev/mmcblk0p1 ~/.sdcard"

function dict_def () {
	
	sdcv --data-dir ~/.config/stardict/web1913 $1 | less
}

PS1="[\w][\t]\nλ "

HISTFILE="/tmp/.bash_history"
HISTSIZE=1000
HISTFILESIZE=2000

set -o vi
bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'

export NNN_FCOLORS='c1e2c42e006033f7c6d6ab27'
export NNN_BMS='h:/home/zenex;.:/home/zenex/.config;d:/home/zenex/Downloads;D:/home/zenex/Documents'
export NNN_ARCHIVE="\\.(7z|a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma|lzo|rar|rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip)$"
export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG='t:preview-tabbed'
export NNN_OPTS='HdP'
export TERMINAL="st"
export EDITOR="nvim"
export VISUAL="nvim"
export PATH=~/.local/bin/:$PATH
export LF_COLORS="~/Documents=01;31:~/Downloads=01;31:~/.local/share=01;31:~/.config/lf/lfrc=31:.git/=01;32:.git=32:.gitignore=32:Makefile=32:README.*=33:*.txt=34:*.md=34:ln=01;36:di=01;31:ex=01;32:"
export QT_QPA_PLATFORMTHEME=qt5ct
export _ZL_DATA='~/.local/share/.zlua'
#openssl enc -aes-256-cbc -md sha512 -pbkdf2 -iter 250000 -salt -in InputFilePath -out OutputFilePath
#openssl enc -aes-256-cbc -d -md sha512 -pbkdf2 -iter 250000 -salt -in InputFilePath -out OutputFilePath
#backup tar cf - directory | 7za a -si directory.tar.7z
#extract 7za x -so directory.tar.7z | tar xf -
#systemctl list-unit-files --type=service --state=enabled
#xinput list | grep orbit
#xinput list-props 15
#xinput set-prop 15 'libinput Middle Emulation Enabled' 1
eval "$(lua ~/.config/z.lua --init bash)"
