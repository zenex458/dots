[ -f "$HOME/.config/.aliasrc" ] && source "$HOME/.config/.aliasrc"
[ -f "$HOME/.config/.envrc" ] && source "$HOME/.config/.envrc"

fe(){ find ~/ -type f | fzf | xargs -r $EDITOR; }
fcd(){ cd "$(find ~/ -type d | fzf)"; }
lc(){ for i in *; do "$@" "$i"; done }

PROMPT='[%~]
$ '

HISTFILE=~/.local/share/.histfile
HISTSIZE=100000
SAVEHIST=100000
unsetopt beep extendedglob
bindkey -v
unsetopt correct_all correct
setopt autocd hist_ignore_dups hist_expire_dups_first
zstyle ':completion:*' completer _expand _complete _ignored _approximate menu
autoload -Uz compinit
zmodload zsh/complist
compinit
_comp_options+=(globdots)
bindkey -v
export KEYTIMEOUT=1
# Change cursor shape for different vi modes.
zle-keymap-select() {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';; #block
        viins|main) echo -ne '\e[3 q';; #underline
    esac
}
zle -N zle-keymap-select
bindkey -s '^l\r' clear
bindkey '^r' history-incremental-search-backward

typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg=#cccccc
ZSH_HIGHLIGHT_STYLES[precommand]=fg=#cccccc
ZSH_HIGHLIGHT_STYLES[arg0]=fg=#cccccc
ZSH_HIGHLIGHT_STYLES[alias]=fg=#cccccc
ZSH_HIGHLIGHT_STYLES[path]=fg=#cccccc
source /home/zenex/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
