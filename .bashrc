[ -f "$HOME/.config/.aliasrc" ] && source "$HOME/.config/.aliasrc"
[ -f "$HOME/.config/.envrc" ] && source "$HOME/.config/.envrc"

shopt -s autocd cdspell
set -o noclobber

#PS1="[\w]\n$ "
PS1="[\w]\nλ "
HISTFILE="/home/zenex/.local/share/.bash_history"
HISTSIZE=1000
HISTFILESIZE=2000

set -o emacs
bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'TAB:menu-complete'

#bind 'set show-mode-in-prompt on'
#https://unix.stackexchange.com/questions/533509/how-to-display-the-current-mode-of-vi-command-line-editing-set-editing-mode
#bind 'set vi-ins-mode-string \1\e[3 q\2'
#bind 'set vi-cmd-mode-string \1\e[1 q\2'
#bind -x '"\C-l":"clear"'
