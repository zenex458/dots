{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../pkgs.nix
    ./firefox.nix
    ./hyprland.nix
  ];

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };

  xdg = {
    mime = {
      enable = true;
    };
    mimeApps = {
      enable = true;
      associations.added = {
        "text/markdown" = "emacs.desktop";
        "image/png" = "imv.desktop";
      };
      defaultApplications = {
        "text/plain" = "emacs.desktop";
        "text/html" = "firefox.desktop";
        "image/png" = "imv.desktop";
        "image/jpeg" = "imv.desktop";
        "image/gif" = "imv.desktop";
        "video/mp4" = "mpv.desktop";
        "audio/x-mpegurl" = "mpv.desktop";
        "application/pdf" = "org.pwmt.zathura.desktop";
        "application/vnd.ms-powerpoint" = "libreoffice-impress.desktop;";
        "application/vnd.ms-powerpoint.presentation" = "libreoffice-impress.desktop;";
        "application/vnd.ms-powerpoint.template" = "libreoffice-impress.desktop;";
        "application/vnd.ms-word" = "libreoffice-writer.desktop;";
        "application/vnd.ms-word.document" = "libreoffice-writer.desktop;";
        "application/vnd.ms-word.template" = "libreoffice-writer.desktop;";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
        "inode/directory" = "nemo.desktop";
      };
    };
  };

  manual.manpages.enable = true;

  services = {
    # services.opensnitch-ui.enable = true;
    ssh-agent.enable = true;
    emacs = {
      enable = true;
      startWithUserSession = true;
      client = {
        enable = true;
        arguments = [
          "-c"
          "-a"
          "emacs"
        ];
      };
    };

    mpd = {
      enable = true;
      musicDirectory = "/home/zenex/Music/Alt";
      dataDir = "/home/zenex/Music/";
      dbFile = "/home/zenex/Music/mpd.db";
      network.startWhenNeeded = true;
      #change so instead of zenex it is the current user, do this also for the mounting, #change to a home.file by using: ${config.home.username}
      extraConfig = ''
          audio_output {
             	type "pipewire"
              	name "pipewire"
          }
         #volume_normalization "yes"
        #replaygain "track"
      '';
    };

    dunst = {
      enable = true;
      settings = {
        global = {
          width = 300;
          height = 300;
          offset = "0x0";
          origin = "top-right";
          transparency = 0;
          frame_color = "#bdae93"; # c6c6c6
          font = "Ttyp0 Bold 10";
          vertical_alignment = "center";
          alignment = "center";
          mouse_left_click = "close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
          notification_limit = 0;
          follow = "mouse";
        };
        urgency_low = {
          background = "#111111";
          foreground = "#a08a64";
          timeout = 10;
        };

        urgency_normal = {
          background = "#000000";
          foreground = "#bdae93";
          timeout = 10;
        };

        urgency_critical = {
          background = "#900000";
          foreground = "#FFFFFF";
          frame_color = "#FF0000";
          timeout = 0;
        };
      };
    };
  };

  programs = {
    man = {
      enable = true;
      generateCaches = false;
    };

    emacs = {
      enable = true;
      package = pkgs.emacs-pgtk; #use just `emacs' if you want it the daemon to survive after the gui terminates
      extraPackages = epkgs:
        with pkgs.unstable.emacsPackages; [
          vterm
          pdf-tools
          multi-vterm
          ace-window
          apheleia
          async
          auctex
          cape
          consult
          corfu
          diminish
          dired-subtree
          eglot
          elfeed
          elfeed-org
          embark
          embark-consult
          expreg
          flymake
          gcmh
          hungry-delete
          haskell-mode
          indent-guide
          magit
          markdown-mode
          multiple-cursors
          nix-ts-mode
          orderless
          org-bullets
          org-make-toc
          rainbow-delimiters
          rainbow-mode
          sudo-edit
          undo-fu-session
          vertico
          zoxide
          vlf
          yasnippet
        ];
      extraConfig = ''
        (use-package pdf-tools
            :magic ("%PDF" . pdf-view-mode)
            :hook (pdf-view-mode . pdf-view-themed-minor-mode)
            :config
              (setq pdf-info-epdfinfo-program "${pkgs.emacsPackages.pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-20240429.407/epdfinfo")
             (pdf-tools-install))
      '';
    };

    chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium.override {enableWideVine = true;};
    };

    zoxide = {
      enable = true;
      enableBashIntegration = true;
    };

    bash = {
      enable = true;
      enableCompletion = true;
      historyControl = ["ignoredups"];
      historyFile = "$HOME/.local/share/.bash_history";
      historyFileSize = 10000;
      historySize = 10000;
      shellOptions = [
        "cdspell"
        "dirspell"
        "autocd"
        "histappend"
      ];
      initExtra = ''
        if [ -z "$INSIDE_EMACS" ]; then
           bind -x '"\C-g":"cd $(bfs -type d -exclude -name .git -exclude -name .ccls-cache -exclude -name env -exclude -name '*venv*' | fzy)"'
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
        upd = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#eukaryotic --use-remote-sudo";
        updv = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#eukaryotic --use-remote-sudo -v --show-trace";
        updflake = "nix flake update --commit-lock-file";
        listnixgen = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
        remoldgen = "nix-collect-garbage --delete-older-than 2d && sudo nix-collect-garbage --delete-older-than 2d && upd";
        re = "systemctl reboot";
        off = "systemctl poweroff";
        nv = "vim";
        ls = "ls -h --classify=auto --group-directories-first --color=auto";
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
        mhd = "sudo mount -v -t ntfs -m -o rw,noexec,uid=1000,gid=1000 UUID=742455142454DAA6 /run/media/zenex/seagate";
        umhd = "sudo umount -v /run/media/zenex/seagate && lsblk";
        sysdlist = "systemctl list-unit-files --type=service --state=enabled";
        rsy = "rsync -ahPzRcL --info=progress2";
        del = "trash";
        dele = "trash empty --all";
        dow = "aria2c -c -s 16 -x 16 -k 1M -j 1";
        chkfstab = "sudo findmnt --verify";
        logs = "journalctl -S today -o verbose -r -x";
        log = "journalctl -S today -r -x";
        e = "emacsclient -a emacs -t";
        upded = "systemctl --user restart emacs.service  &&  systemctl --user status emacs.service";
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
        BEMENU_OPTS = ''-i --fn 'Ttyp0' -B '1' -f -p '>' -n --tb '#bdae93' --tf '#000000' --fb '#000000' --ff '#bdae93' --nb '#000000' --nf '#bdae93' --ab '#000000' --af '#bdae93' --sb '#000000' --sf '#bdae93' --cb '#bdae93' --cf '#bdae93' --hb '#bdae93' --hf '#000000' --sb '#bdae93' --sf '#000000' --scb '#000000' --scf '#bdae93' --bdr '#bdae93' '';
      };
      #initExtra = ''
      #  PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND$'
      #  '}history -a; history -c; history -r"'';
    };

    ncmpcpp = {
      enable = true;
      mpdMusicDir = "/home/zenex/Music/Alt";
      settings = {
        ncmpcpp_directory = "~/.config/ncmpcpp";
        mpd_crossfade_time = 1;
        header_visibility = "yes";
        lyrics_directory = "";
        current_item_prefix = "$(white)$r";
        current_item_inactive_column_prefix = "$(white)$r";
        browser_sort_mode = "name";
        browser_sort_format = "{%a - }{%t}{%b}|{%f} {%l}";
        song_columns_list_format = "(20)[]{a} (6f)[white]{NE} (50)[white]{t|f:Title} (20)[white]{b} (7f)[white]{l}";
        playlist_show_remaining_time = "yes";
        playlist_shorten_total_times = "yes";
        playlist_display_mode = "columns";
        browser_display_mode = "columns";
        search_engine_display_mode = "columns";
        playlist_editor_display_mode = "columns";
        autocenter_mode = "yes";
        centered_cursor = "yes";
        progressbar_look = "->";
        allow_for_physical_item_deletion = "no";
        clock_display_seconds = "yes";
        external_editor = "vim";
        use_console_editor = "yes";
        header_window_color = "default";
        state_line_color = "black";
        state_flags_color = "default:b";
        main_window_color = "white";
        color1 = "red";
        color2 = "red";
        progressbar_color = "black:b";
        progressbar_elapsed_color = "green:b";
        statusbar_color = "default";
        statusbar_time_color = "default:b";
        player_state_color = "default:b";
        message_delay_time = 1;
        default_find_mode = "wrapped";
      };
      bindings = [];
    };

    htop = {
      enable = true;
      settings = {
        show_cpu_frequency = 1;
        show_cpu_temperature = 1;
      };
    };

    zathura = {
      enable = true;
      mappings = {
        "<PageUp>" = "navigate previous";
        "<PageDown>" = "navigate next";
        "+" = "zoom in";
        "-" = "zoom out";
        "<C-q>" = "quit";
      };
    };

    tmux = {
      # add new-window -c "#{pane_current_path}"
      # add splitp -c "#{pane_current_path}"
      enable = true;
      aggressiveResize = true;
      prefix = "C-q";
      baseIndex = 0;
      escapeTime = 0;
      historyLimit = 100000;
      keyMode = "emacs";
      mouse = true;
      terminal = "tmux-256color";
      extraConfig = ''
        set -g set-titles on
        set -g status-keys emacs
        set -s set-clipboard external
        set -g status-style "fg=#bdae93,bg=#000000"
        setw -g monitor-activity on
        set -g visual-activity on
        set -g status-right ""
        set -g status-left "#{session_group}"
        set -g window-status-current-format "#[fg=#000000 bg=#000000]|#[fg=#bdae93 bg=#000000]#W#[fg=#000000 bg=#000000]|"
        set -g window-status-last-style "fg=#a08a64 bg=#000000"
        bind-key -n M-"v" split-window -v
        bind-key -n M-"V" split-window -h
        bind-key -n M-h select-pane -L
        bind-key -n M-j select-pane -D
        bind-key -n M-k select-pane -U
        bind-key -n M-l select-pane -R
        bind-key -n M-H swap-pane -U
        bind-key -n M-J swap-pane -D
        bind-key -n M-K swap-pane -U
        bind-key -n M-L swap-pane -D
        bind-key -n M-C-h resize-pane -L
        bind-key -n M-C-j resize-pane -D
        bind-key -n M-C-k resize-pane -U
        bind-key -n M-C-l resize-pane -R
      '';
    };

    foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          term = "xterm-256color";
          font = "Ttyp0:style=Regular:size=10";
          dpi-aware = "no";
        };
        mouse = {
          hide-when-typing = "yes";
        };
        cursor = {
          style = "block";
          blink = "yes";
        };
        colors = {
          background = "000000";
          foreground = "bdae93";
          regular0 = "444444"; # black
          regular1 = "B33929"; # red
          regular2 = "75B329"; # green
          regular3 = "c0c000"; # yellow
          regular4 = "2874B2"; # blue
          regular5 = "802caa"; # magenta
          regular6 = "6cb2eb"; # cyan
          regular7 = "a08a64"; # white

          bright0 = "666666"; # black
          bright1 = "f62b5a"; # red
          bright2 = "47b413"; # green
          bright3 = "e3c401"; # yellow
          bright4 = "24acd4"; # blue
          bright5 = "f2affd"; # magenta
          bright6 = "13c299"; # cyan
          bright7 = "bdae93"; # white
        };
      };
    };
    home-manager.enable = true;
  };

  home = {
    stateVersion = "24.05";
    username = "zenex";
    file = {
      ".local/bin" = {
        source = ./scripts;
        recursive = true;
        executable = true;
      };
      ".config/emacs" = {
        source = ./emacs;
        recursive = true;
      };
    };
    persistence."/persistent/home/zenex" = {
      directories = [
        ".config/emacs"
        ".config/feather"
        ".config/gh"
        ".config/opensnitch"
        ".config/Signal"
        ".config/simplex"
        ".config/vesktop"
        ".config/zotero"
        ".local/share/simplex"
        ".mozilla"
        "Dev"
        "Documents"
        "Downloads"
        "Music"
        "Sync"
      ];
      files = [".local/share/.bash_history"];
      allowOther = true;
    };

    sessionPath = [
      "$HOME/.local/bin"
    ];

    packages = with pkgs; [
      # bsdgames
      # entr # run a command when files change
      # ffmpegthumbnailer
      # fq # jq for binary formats
      # gron # json grepper
      # https://viric.name/soft/ts/
      # https://www.gnu.org/software/parallel
      # imhex
      # kismet
      # macchanger
      # mpvScripts.mpris
      # nodePackages.prettier
      # rlwrap # for the readline
      # sigrok-cli
      age
      alacritty
      alejandra
      alsa-utils
      android-tools
      anki-bin
      aria2
      astyle
      basedpyright
      bc
      bemenu
      bfs
      ccls
      cliphist
      cryptsetup
      dig
      exfatprogs
      exif
      feather
      ffmpeg-full
      file
      fuse3
      fzy
      gcc
      gh
      gimp
      git
      gojq
      grim
      haskell-language-server
      html-tidy
      htop
      hunspell
      hunspellDicts.en-gb-large
      imagemagick
      imv
      keepassxc
      libnotify
      libreoffice
      lsof
      magic-wormhole
      man-pages
      man-pages-posix
      mpc-cli
      mpv
      ghc
      mupdf
      nemo
      nixd
      nodePackages.bash-language-server
      ormolu
      p7zip
      pandoc
      pulsemixer
      python312Packages.deemix
      python3Full
      ripgrep
      ripgrep-all
      rsync
      ruff
      sbctl
      shellcheck
      shfmt
      signal-desktop
      sioyek
      slurp
      syncthing
      texliveFull
      traceroute
      trashy
      tree
      unstable.simplex-chat-desktop
      unzip
      usbutils
      vesktop
      vim
      virt-manager
      wdisplays
      wl-clip-persist
      wl-clipboard
      wl-color-picker
      wlr-randr
      wlsunset
      xdg-utils
      xmlformat
      yt-dlp
      zip
      zotero
      (aspellWithDicts (
        dicts:
          with dicts; [
            en
            en-computers
            en-science
          ]
      ))
    ];
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
