{
  config,
  pkgs,
  ...
}: {
  home.shell.enableFishIntegration = true;
  programs = {
    dircolors = {
      enable = true;
    };
    direnv = {
      enable = true;
      enableBashIntegration = true;
      silent = true;
      config = {whitelist = {prefix = ["~/Dev"];};};
    };
    zoxide = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
        function fish_prompt --description 'prompt'
             set -l suffix 'λ'
             set -l prompt (echo -n '['(prompt_pwd -D 10)']')

             echo -n -s $prompt\n$suffix " "
        end

        function cd
             builtin cd $argv[1] && ls -A -h --classify=auto --group-directories-first --color=auto
        end
        set fish_color_autosuggestion $fish_color_normal --underline
        set fish_color_valid_path $fish_color_normal
        set fish_color_param $fish_color_normal --bold
        set fish_color_error $fish_color_normal --reverse --underline
      '';
      shellAbbrs =
        config.programs.bash.shellAliases
        // {
          # a space before a command means fish will not show up in history
          fg = " fg";
          bg = " bg";
          wormhole = " wormhole";
          curl = " curl";
        };
      plugins = [
        {
          name = "grc";
          src = pkgs.fishPlugins.grc.src;
        }
        {
          name = "done";
          src = pkgs.fishPlugins.done;
        }
      ];
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
        upd = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#nidus --sudo --log-format multiline-with-logs";
        updb = "sudo nixos-rebuild boot --flake ~/Dev/dots/.config/nixos#nidus --sudo --log-format multiline-with-logs";
        updv = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#nidus --sudo -v --show-trace --log-format multiline-with-logs";
        updf = "nh os switch";
        updflake = "nix flake update --commit-lock-file";
        listnixgen = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
        remoldgen = "nix-collect-garbage --delete-older-than 2d && sudo nix-collect-garbage --delete-older-than 2d && upd";
        nv = "nvim";
        ls = "ls -A -h --classify=auto --group-directories-first --color=auto";
        ga = "git add";
        gc = "git commit -m";
        updoff = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#nidus --use-remote-sudo --log-format multiline-with-logs && sleep 2 && systemctl poweroff";
        updr = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#nidus --use-remote-sudo --log-format multiline-with-logs && sleep 2 && systemctl reboot";
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
        rsy = "rsync -ahPzRcL --info=progress2 --stats --filter=':- .gitignore'";
        trp = "trash-put";
        tre = "trash-empty";
        dow = "aria2c -c -s 16 -x 16 -k 1M -j 1";
        chkfstab = "sudo findmnt --verify";
        logs = "journalctl -S today -o verbose -r -x";
        log = "journalctl -S today -r -x";
        e = "emacsclient -a emacs -t";
        upded = "systemctl --user restart emacs.service && systemctl --user status emacs.service";
        ns = "niri-session";
        bfs = "bfs -exclude -name .git -exclude -name .ccls-cache -exclude -name '*env*'";
        locate = "locate -i -d /var/cache/locate/locatedb";
        rbackup = "restic -r sftp:restic-backup-host:/home/ubuntu/data/Inc_Backup backup ~/Documents ~/.ssh ~/.gnupg ~/Dev";
        dc = "docker compose";
        tss = "tailscale status";
        tsu = "tailscale up";
        tsd = "tailscale down";
        ts = "tailscale";
      };
    };
  };
}
