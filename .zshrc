[ -f "$HOME/.config/.aliasrc" ] && source "$HOME/.config/.aliasrc"
[ -f "$HOME/.config/.envrc" ] && source "$HOME/.config/.envrc"

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

PROMPT='[%~]
λ '

HISTFILE=~/.local/share/.histfile
HISTSIZE=100000
SAVEHIST=100000
unsetopt beep extendedglob
bindkey -v

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)
bindkey -v
export KEYTIMEOUT=1
# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';; #block
        viins|main) echo -ne '\e[3 q';; #underline
    esac
}
zle -N zle-keymap-select
#backup tar cf - directory | 7za a -si directory.tar.7z
#extract 7za x -so directory.tar.7z | tar xf -
#source ~/.zsh/zsh-autocomplete/zsh-autocomplete.plugin.zsh
#eval "$(lua ~/.config/z.lua --init zsh)"
