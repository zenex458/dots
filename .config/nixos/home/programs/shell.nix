{
  config,
  pkgs,
  ...
}: {
  programs = {
    zoxide = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    bash = {
      enable = true;
      enableCompletion = true;
      historyControl = ["ignoredups"];
      historyFile = "$HOME/.local/share/.bash_history";
      historyFileSize = 1000;
      #historySize = 10000;
      shellOptions = [
        "cdspell"
        "dirspell"
        "autocd"
        "histappend"
      ];
      initExtra = ''
        if [ -z "$INSIDE_EMACS" ]; then
           bind -x '"\C-g":"cd $(bfs -type d -exclude -name .git -exclude -name .ccls-cache -exclude -name '*env*' | fzy)"'
           bind -x '"\C-r":history_search'
           history_search(){
           READLINE_LINE=$(
           	HISTTIMEFORMAT=
           	history | sort -rn | cut -c 8- | awk '!visited[''$0]++' | fzy -q "''$READLINE_LINE")
           READLINE_POINT=0x7FFFFFFF
           }
           bind 'set show-all-if-ambiguous on'
           bind 'set completion-ignore-case on'
           bind 'TAB:menu-complete'
        fi
        cd() {
        	if [ -z "$#" ]; then
        		builtin cd
        	else
        		builtin cd "$@"
        	fi
        	if [ $? -eq 0 ]; then
            ls -h --classify=auto --color=auto --group-directories-first
        	fi
        }
      '';
      shellAliases = {
        upd = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#nidus --use-remote-sudo --log-format multiline-with-logs";
        updv = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#nidus --use-remote-sudo -v --show-trace --log-format multiline-with-logs";
        updf = "nh os switch -a";
        updflake = "nix flake update --commit-lock-file";
        listnixgen = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
        remoldgen = "nix-collect-garbage --delete-older-than 2d && sudo nix-collect-garbage --delete-older-than 2d && upd";
        re = "systemctl reboot";
        off = "systemctl poweroff";
        nv = "nvim";
        ls = "ls -A -h --classify=auto --group-directories-first --color=auto";
        ga = "git add";
        gc = "git commit -m";
        updoff = "upd && sleep 2 && off";
        updr = "upd && sleep 2 && re";
        grep = "grep -i --colour=auto";
        mkdir = "mkdir -pv";
        mv = "mv -iv";
        cp = "cp -iva";
        rm = "rm -iv";
        ll = "ls -lAF --color=auto";
        tm = "ps auxww | grep";
        tk = "tmux kill-session";
        ytmp3 = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format mp3 --audio-quality 0 --embed-thumbnail";
        yt10 = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=1080][fps=30]+bestaudio/best[height<=1080]'";
        yt7 = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
        yt7s = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --sponsorblock-remove sponsor --remux-video mp4 --embed-subs; --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
        ytb = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en";
        chnum = "stat -c '%a %n'";
        tas = "tmux attach-session";
        tls = "tmux list-session";
        tat = "tmux attach -t";
        mm = "sudo mount -m -v -o rw,uid=1000,gid=1000";
        sysdlist = "systemctl list-unit-files --type=service --state=enabled";
        rsy = "rsync -ahPzRcL --info=progress2 --stats --exclude=.ccls-cache --exclude=sessionData --exclude=elfeed --exclude=eln-cache --exclude=Signal --exclude=simplex --exclude=chromium --exclude=.mozilla --exclude=.local --exclude=.cache --exclude=.nix-defexpr --exclude=.nix-profile --exclude=.java --exclude=yyt --exclude=iso --exclude=Music --filter=':- .gitignore'";
        trp = "trash-put";
        tre = "trash-empty";
        dow = "aria2c -c -s 16 -x 16 -k 1M -j 1";
        chkfstab = "sudo findmnt --verify";
        logs = "journalctl -S today -o verbose -r -x";
        log = "journalctl -S today -r -x";
        e = "emacsclient -a emacs -t";
        upded = "systemctl --user restart emacs.service  &&  systemctl --user status emacs.service";
        hy = "Hyprland >> /tmp/hy";
        ns = "niri-session";
        bfs = "bfs -exclude -name .git -exclude -name .ccls-cache -exclude -name '*env*'";
        locate = "locate -i -d /var/cache/locate/locatedb";
        rbackup = "restic -r sftp:restic-backup-host:/home/ubuntu/data/Inc_Backup backup ~/Documents ~/.ssh ~/.gnupg";
        dc = "docker compose";
      };
      sessionVariables = {
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_STATE_HOME = "$HOME/.local/state";
        XDG_CACHE_HOME = "$HOME/.cache";
        MUPDFHISTFILE = "/tmp/.mupdf.history";
        DOTNET_CLI_TELEMETRY_OPTOUT = 1;
        TERMINAL = "foot";
        EDITOR = "emacsclient -c -a emacs";
        VISUAL = "emacsclient -c -a emacs";
        LESSHISTFILE = "/tmp/.lesshst";
        MOZ_ENABLE_WAYLAND = 1;
        QT_QPA_PLATFORM = "wayland;xcb";
        GDK_BACKEND = "wayland";
        _JAVA_AWT_WM_NONREPARENTING = 1;
        SAL_USE_VCLPLUGIN = "gtk3";
        XCURSOR_SIZE = 20;
        BEMENU_OPTS = ''-i --fn 'Ttyp0' -B '1' -f -p '>' -n --tb '#bdae93' --tf '#060606' --fb '#060606' --ff '#bdae93' --nb '#060606' --nf '#bdae93' --ab '#060606' --af '#bdae93' --sb '#060606' --sf '#bdae93' --cb '#bdae93' --cf '#bdae93' --hb '#bdae93' --hf '#060606' --sb '#bdae93' --sf '#060606' --scb '#060606' --scf '#bdae93' --bdr '#bdae93' '';
        MATHPATH = "/run/current-system/sw/share/man";
      };
      #initExtra = ''
      #  PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND$'
      #  '}history -a; history -c; history -r"'';
    };
    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      shellAliases = config.programs.bash.shellAliases;
      completionInit = "autoload -Uz compinit";
      enableCompletion = true;
      autocd = true;
      # defaultKeymap = "viins";
      defaultKeymap = "emacs";
      sessionVariables = config.programs.bash.sessionVariables;
      history = {
        ignoreAllDups = true;
        path = "$ZDOTDIR/.zsh_history";
      };
      autosuggestion = {
        enable = true;
        highlight = "fg=#bdae93,bg=#060606,bold,underline";
      };
      syntaxHighlighting = {
        enable = true;
        styles = {
          suffix-alias = "fg=#bdae93";
          precommand = "fg=#bdae93";
          arg0 = "fg=#bdae93";
          alias = "fg=#bdae93";
          path = "fg=#bdae93";
          unknown-token = "fg=#bdae93,underline";
          command_error = "fg=#bdae93,underline";
        };
      };
      plugins = [
        {
          name = "zsh-command-time";
          src = pkgs.zsh-command-time;
          file = "share/zsh/plugins/command-time/command-time.plugin.zsh";
        }
        # {
        #   name = "zsh-vi-mode";
        #   src = pkgs.zsh-vi-mode;
        #   file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
        # }
      ];
      initContent = ''
        PROMPT="[%~]''\nλ "
        #https://scottspence.com/posts/speeding-up-my-zsh-shell
        if [ "$(date +'%j')" != "''$(stat -f '%Sm' -t '%j' $HOME/.config/zsh/.zcompdump 2>/dev/null)" ]; then
            compinit -d $HOME/.config/zsh/.zcompdump
        else
            compinit -C -d $HOME/.config/zsh/.zcompdump #i don't think -d is needed here
        fi

        zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate _aliases _functions
        zstyle ':completion:*:*:*:*:descriptions' format '%F{#bdae93}[%d]%f'
        zstyle ':completion:*' use-cache on
        zstyle ':completion:*' cache-path "$HOME/.config/zsh/.zcompcache"
        zstyle ':completion:*' group-name ' '
        zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
        zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"
        zstyle ':completion:*' verbose true
        zstyle ':completion:*' menu select search
        ZSH_AUTOSUGGEST_STRATEGY=(completion)
        setopt AUTO_PUSHD PUSHD_IGNORE_DUPS PUSHD_MINUS COMPLETE_IN_WORD REC_EXACT LIST_PACKED LIST_ROWS_FIRST GLOBDOTS NOMATCH NOTIFY CORRECT LIST_PACKED HIST_FIND_NO_DUPS HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS INC_APPEND_HISTORY SHARE_HISTORY
        _comp_options+=(globdots)
        unsetopt beep
        if [ -z "$INSIDE_EMACS" ]; then
          fzy-history-widget() {
            emulate -L zsh
           	zle -I
           	local S=$(history 0 | sort -rn | cut -c 8- | awk '!visited[''$0]++' | fzy -q "''${LBUFFER//$/\\$}")
           	if [[ -n $S ]] ; then
           		LBUFFER=$S
           	fi
           	zle reset-prompt
           }
           zle -N fzy-history-widget
           bindkey '^R' fzy-history-widget
           # zvm_bindkey vicmd '^R' fzy-history-widget
           # zvm_bindkey viins '^R' fzy-history-widget
        fi
        cd() {
        	if [ -z "$#" ]; then
        		builtin cd
        	else
        		builtin cd "$@"
        	fi
        	if [ $? -eq 0 ]; then
        		ls -h -A --classify=auto --color=auto --group-directories-first
        	fi
        }
        # ZVM_VI_INSERT_ESCAPE_BINDKEY=jk
      '';
    };
  };
}
