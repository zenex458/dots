[ -f "$HOME/.config/.aliasrc" ] && source "$HOME/.config/.aliasrc"
[ -f "$HOME/.config/.envrc" ] && source "$HOME/.config/.envrc"

#shopt -s autocd cdspell dotglob nocaseglob
shopt -s autocd cdspell
#function fe(){
#	du -a ~/ | awk {'print $2'} | fzf | xargs -r nvim
#}
function fe (){
	#nvim "$(find ~/ -type f | fzf | xargs -r)"
	find ~/ -type f | fzf | xargs -r $EDITOR
}
function fcd (){
	cd "$(find ~/ -type d | fzf)"
}
function see_sharp (){
	mcs $1.cs && mono $1.exe 
}
function dict_def (){
	
	sdcv --data-dir ~/.config/stardict/web1913 $1 | less
}
function inman (){
	man $1 | grep -C1 "$2"
}
function inmans (){
	man $1 | \grep -C1 $2
}

PS1="[\w]\nλ "

HISTFILE="/home/zenex/.local/share/.bash_history"
HISTSIZE=1000
HISTFILESIZE=2000

set -o vi
bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'set show-mode-in-prompt on'
#https://unix.stackexchange.com/questions/533509/how-to-display-the-current-mode-of-vi-command-line-editing-set-editing-mode
bind 'set vi-ins-mode-string \1\e[3 q\2'
bind 'set vi-cmd-mode-string \1\e[1 q\2'
#bind 'set on'
bind 'TAB:menu-complete'
#backup tar cf - directory | 7za a -si directory.tar.7z or just 7z a -si .....
#extract 7za x -so directory.tar.7z | tar xf -
#eval "$(lua ~/.config/z.lua --init bash)"
#eval "$(zoxide init bash)"
