[ -f "$HOME/.config/.aliasrc" ] && source "$HOME/.config/.aliasrc"
[ -f "$HOME/.config/.envrc" ] && source "$HOME/.config/.envrc"

PROMPT='[%~]
λ '

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate _aliases _functions
#zstyle ':completion:*' format '%d'
zstyle ':completion:*:*:*:*:descriptions' format '%F{red}[%d]%f'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "~/.cache/.zcompcache"
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' verbose true
zstyle ':completion:*' menu select search
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit -d ~/.cache/.zcompdump

HISTFILE=~/.local/share/.zshhist
HISTSIZE=100000
SAVEHIST=100000
setopt autocd extendedglob nomatch notify correct list_packed hist_find_no_dups share_history globdots interactivecomments inc_append_history extended_history noclobber hist_ignore_dups hist_expire_dups_first COMPLETE_IN_WORD
unsetopt beep
bindkey -e
eval "$(lua ~/.config/z.lua --init zsh enhanced)"
ZSH_COMMAND_TIME_COLOR="green"
source ~/.config/zsh/zsh-command-time/command-time.plugin.zsh

#typeset -A ZSH_HIGHLIGHT_STYLES
#ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg=#cccccc
#ZSH_HIGHLIGHT_STYLES[precommand]=fg=#cccccc
#ZSH_HIGHLIGHT_STYLES[arg0]=fg=#cccccc
#ZSH_HIGHLIGHT_STYLES[alias]=fg=#cccccc
#ZSH_HIGHLIGHT_STYLES[path]=fg=#cccccc
#source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
