{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  nixpkgs = {
    # Configure your nixpkgs instance
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  manual.manpages.enable = true;
  programs.man.enable = true;
  programs.man.generateCaches = false;

  services.emacs = {
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

  wayland.windowManager.river = {
    enable = true;
    xwayland.enable = true;
    extraConfig = ''
      #!/bin/sh

      # This is the example configuration file for river.
      #
      # If you wish to edit this, you will probably want to copy it to
      # $XDG_CONFIG_HOME/river/init or $HOME/.config/river/init first.
      #
      # See the river(1), riverctl(1), and rivertile(1) man pages for complete
      # documentation.

      # Note: the "Super" modifier is also known as Logo, GUI, Windows, Mod4, etc.

      # Super+Shift+E to exit river
      riverctl map normal Super+Shift E exit

      # Super+J and Super+K to focus the next/previous view in the layout stack
      riverctl map normal Super J focus-view next
      riverctl map normal Super K focus-view previous

      # Super+Shift+J and Super+Shift+K to swap the focused view with the next/previous
      # view in the layout stack
      riverctl map normal Super+Shift J swap next
      riverctl map normal Super+Shift K swap previous

      # Super+Period and Super+Comma to focus the next/previous output
      riverctl map normal Super Period focus-output next
      riverctl map normal Super Comma focus-output previous

      # Super+Shift+{Period,Comma} to send the focused view to the next/previous output
      riverctl map normal Super+Shift Period send-to-output next
      riverctl map normal Super+Shift Comma send-to-output previous

      # Super+Shift+Return to start an instance of foot (https://codeberg.org/dnkl/foot)
      riverctl map normal Super Return spawn foot

      # Super+H and Super+L to decrease/increase the main ratio of rivertile(1)
      riverctl map normal Super H send-layout-cmd rivertile "main-ratio -0.05"
      riverctl map normal Super L send-layout-cmd rivertile "main-ratio +0.05"

      # Super+Shift+H and Super+Shift+L to increment/decrement the main count of rivertile(1)
      riverctl map normal Super+Shift H send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super+Shift L send-layout-cmd rivertile "main-count -1"

      # Super+Alt+{H,J,K,L} to move views
      riverctl map normal Super+Alt H move left 100
      riverctl map normal Super+Alt J move down 100
      riverctl map normal Super+Alt K move up 100
      riverctl map normal Super+Alt L move right 100

      # Super+Alt+Control+{H,J,K,L} to snap views to screen edges
      riverctl map normal Super+Alt+Control H snap left
      riverctl map normal Super+Alt+Control J snap down
      riverctl map normal Super+Alt+Control K snap up
      riverctl map normal Super+Alt+Control L snap right

      # Super+Alt+Shift+{H,J,K,L} to resize views
      riverctl map normal Super+Alt+Shift H resize horizontal -100
      riverctl map normal Super+Alt+Shift J resize vertical 100
      riverctl map normal Super+Alt+Shift K resize vertical -100
      riverctl map normal Super+Alt+Shift L resize horizontal 100

      # Super + Left Mouse Button to move views
      riverctl map-pointer normal Super BTN_LEFT move-view

      # Super + Right Mouse Button to resize views
      riverctl map-pointer normal Super BTN_RIGHT resize-view

      # Super + Middle Mouse Button to toggle float
      riverctl map-pointer normal Super BTN_MIDDLE toggle-float

      for i in $(seq 1 9)
      do
          tags=$((1 << ($i - 1)))

          # Super+[1-9] to focus tag [0-8]
          riverctl map normal Super $i set-focused-tags $tags

          # Super+Shift+[1-9] to tag focused view with tag [0-8]
          riverctl map normal Super+Shift $i set-view-tags $tags

          # Super+Control+[1-9] to toggle focus of tag [0-8]
          riverctl map normal Super+Control $i toggle-focused-tags $tags

          # Super+Shift+Control+[1-9] to toggle tag [0-8] of focused view
          riverctl map normal Super+Shift+Control $i toggle-view-tags $tags
      done

      # Super+0 to focus all tags
      # Super+Shift+0 to tag focused view with all tags
      all_tags=$(((1 << 32) - 1))
      riverctl map normal Super 0 set-focused-tags $all_tags
      riverctl map normal Super+Shift 0 set-view-tags $all_tags

      # Super+Space to toggle float
      riverctl map normal Super Space toggle-float

      # Super+F to toggle fullscreen
      riverctl map normal Super F toggle-fullscreen

      # Super+{Up,Right,Down,Left} to change layout orientation
      riverctl map normal Super Up    send-layout-cmd rivertile "main-location top"
      riverctl map normal Super Right send-layout-cmd rivertile "main-location right"
      riverctl map normal Super Down  send-layout-cmd rivertile "main-location bottom"
      riverctl map normal Super Left  send-layout-cmd rivertile "main-location left"

      # Declare a passthrough mode. This mode has only a single mapping to return to
      # normal mode. This makes it useful for testing a nested wayland compositor
      riverctl declare-mode passthrough

      # Super+F11 to enter passthrough mode
      riverctl map normal Super F11 enter-mode passthrough

      # Super+F11 to return to normal mode
      riverctl map passthrough Super F11 enter-mode normal

      # Various media key mapping examples for both normal and locked mode which do
      # not have a modifier
      for mode in normal locked
      do
          # Eject the optical drive (well if you still have one that is)
          riverctl map $mode None XF86Eject spawn 'eject -T'

          # Control pulse audio volume with pamixer (https://github.com/cdemoulins/pamixer)
          riverctl map $mode None XF86AudioRaiseVolume  spawn 'pamixer -i 5'
          riverctl map $mode None XF86AudioLowerVolume  spawn 'pamixer -d 5'
          riverctl map $mode None XF86AudioMute         spawn 'pamixer --toggle-mute'

          # Control MPRIS aware media players with playerctl (https://github.com/altdesktop/playerctl)
          riverctl map $mode None XF86AudioMedia spawn 'playerctl play-pause'
          riverctl map $mode None XF86AudioPlay  spawn 'playerctl play-pause'
          riverctl map $mode None XF86AudioPrev  spawn 'playerctl previous'
          riverctl map $mode None XF86AudioNext  spawn 'playerctl next'

          # Control screen backlight brightness with brightnessctl (https://github.com/Hummer12007/brightnessctl)
          riverctl map $mode None XF86MonBrightnessUp   spawn 'brightnessctl set +5%'
          riverctl map $mode None XF86MonBrightnessDown spawn 'brightnessctl set 5%-'
      done

      # Set background and border color
      riverctl background-color 0x002b36
      riverctl border-color-focused 0x93a1a1
      riverctl border-color-unfocused 0x000000

      # Set keyboard repeat rate
      riverctl set-repeat 50 300

      riverctl keyboard-layout -options "altwin:ctrl_alt_win,caps:shift_nocancel,caps:backspace" gb

      # Make all views with an app-id that starts with "float" and title "foo" start floating.
      riverctl rule-add -app-id 'float*' -title 'foo' float

      # Make all views with app-id "bar" and any title use client-side decorations
      riverctl rule-add -app-id "bar" csd

      # Set the default layout generator to be rivertile and start it.
      # River will send the process group of the init executable SIGTERM on exit.
      riverctl default-layout rivertile
      rivertile -view-padding 0 -outer-padding 0 &



    '';
    settings = {
      border-width = 2;
      declare-mode = [
        "locked"
        "normal"
        "passthrough"
      ];
      input = {
        pointer-foo-bar = {
          accel-profile = "flat";
          events = true;
          pointer-accel = -0.3;
          tap = false;
        };
      };
      map = {
        normal = {
          "Alt Q" = "close";
        };
      };
      set-cursor-warp = "on-output-change";
      set-repeat = "60 300";
      spawn = [
        "firefox"
        "'emacsclient -c -a emacs'"
      ];
    };
  };

  programs.librewolf.enable = true;

  services.ssh-agent.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.pdf-tools
      epkgs.multi-vterm
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

  programs.neovim = {
    enable = true;
    extraConfig = ''
      let mapleader = " "
      let g:currentmode={
             \ 'n'  : '[N] ',
             \ 'v'  : '[V] ',
             \ 'V'  : '[VLine] ',
             \ "\<C-V>" : '[VBlock] ',
             \ 'i'  : '[I] ',
             \ 'R'  : '[R] ',
             \ 'Rv' : '[V·Replace] ',
             \ 'c'  : '[Command] ',
             \}

      syntax on
      filetype plugin indent on
      "set laststatus=0
      set ruler
      set ignorecase
      set smartcase
      set smartindent
      set autoindent
      set cursorline
      set title
      set cursorcolumn
      set showcmd
      set showmatch
      set hlsearch
      set title
      set nocompatible
      set wildmode=longest,list,full
      set clipboard+=unnamedplus
      set termguicolors
      set noshowmode
      "set guicursor=i:bloack-iCursor
      "set guicursor+=i:blinkon100
      "set guicursor=n-v-c:hor50-Curosr
      "set guicursor+=n-v-c:blinkon100

      set guicursor=i:hor50-Cursor
      set guicursor+=i:blinkon100
      set guicursor+=n-v-c:blinkon100


      "set guicursor=n-v-c:hor50-Cursor
      "set guicursor+=i:hor50-Cursor
      set statusline+=\%{toupper(g:currentmode[mode()])}
      set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "[%n] %Y
      "set guicursor=i:block-iCursor
      "set guicursor+=i:blinkon100
      "set guicursor+=n-v-c:blinkon100
      "set mouse=a
      "set linebreak
      set rnu nu
      colorscheme default
      inoremap <expr> <Tab> pumvisible() ? '<C-n>' :
      \ getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : '<Tab>'
      map <leader>r :%s/
      map <leader>+ <C-w>+
      map <leader>- <C-w>-
      map <leader>= <C-w>=
      map <leader>/ :noh<CR>
      map <leader>sp :setlocal spell! spelllang=en_gb<CR>
      tnoremap <Esc> <C-\><C-n>
      vmap <C-c> "+y
      map <leader>oe :bro ol<CR>
    '';
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
  qt = {
    enable = true;
    platformTheme.name = "gtk";
  };

  gtk = {
    enable = true;
    # theme.package = pkgs.shades-of-gray-theme;
    # theme.name = "Shades-of-gray";
    # cursorTheme.name = "plan9";
    # cursorTheme.size = 20;
    # font.package = pkgs.iosevka;
    # font.name = "Iosevka";
    # font.package = pkgs.uw-ttyp0;
    # font.name = "Ttyp0";
    # font.size = 10;
  };
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium.override {enableWideVine = true;};
  };

  home.sessionPath = [
    "$HOME/.local/bin"
  ];
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    shellAliases = config.programs.bash.shellAliases;
    completionInit = "autoload -Uz compinit && compinit -d $HOME/.cache/.zcompdump";
    enableCompletion = true;
    autocd = true;
    defaultKeymap = "emacs";
    sessionVariables = config.programs.bash.sessionVariables;
    history = {
      extended = true;
      ignoreAllDups = true;
      path = "$ZDOTDIR/.zsh_history";
    };
    autosuggestion = {
      enable = true;
      highlight = "fg=#bdae93,bg=#000000,bold,underline";
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
    initExtra = ''
      PROMPT="[%~]''\nλ "
      zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate _aliases _functions
      zstyle ':completion:*:*:*:*:descriptions' format '%F{#bdae93}[%d]%f'
      zstyle ':completion:*' use-cache on
      zstyle ':completion:*' cache-path "$HOME/.cache/.zcompcache"
      zstyle ':completion:*' group-name ' '
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
      zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"
      zstyle ':completion:*' verbose true
      zstyle ':completion:*' menu select search
      ZSH_AUTOSUGGEST_STRATEGY=(completion)
      setopt AUTO_PUSHD PUSHD_IGNORE_DUPS PUSHD_MINUS COMPLETE_IN_WORD REC_EXACT LIST_PACKED LIST_ROWS_FIRST
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
    '';
  };

  programs.bash = {
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
      "checkwinsize"
    ];
    bashrcExtra = ''
      bind 'set show-all-if-ambiguous on'
      bind 'set completion-ignore-case on'
      bind 'TAB:menu-complete'
      cd() {
      	if [ -z "$#" ]; then
      		builtin cd
      	else
      		builtin cd "$@"
      	fi
      	if [ $? -eq 0 ]; then
      		# ls -h --classify=auto --color=never --group-directories-first
          ls -h --classify=auto --color=auto --group-directories-first
      	fi
      }
    '';
    shellAliases = {
      upd = "sudo nixos-rebuild switch --flake ~/Dev/dots/.config/nixos#eukaryotic";
      updflake = "nix flake update --commit-lock-file";
      listnixgen = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
      remoldgen = "nix-collect-garbage --delete-older-than 2d && sudo nix-collect-garbage --delete-older-than 2d && upd";
      re = "systemctl reboot";
      off = "systemctl poweroff";
      nv = "nvim";
      # ls = "ls -h --classify=auto --color=auto --group-directories-first";
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
      cco = "gcc -O -Wall -W -pedantic";
      ytmp3 = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format mp3 --audio-quality 0 --embed-thumbnail";
      ytflac_thum_chap = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format flac --audio-quality 0 --embed-thumbnail --embed-chapters";
      ytflac_aud = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format flac --audio-quality 0";
      yt10 = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=1080][fps=30]+bestaudio/best[height<=1080]'";
      yt7 = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
      yt7s = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --sponsorblock-remove sponsor --remux-video mp4 --embed-subs; --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
      ytb = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en";
      chnum = "stat -c '%a %n'";
      tas = "tmux attach-session";
      tls = "tmux list-session";
      tat = "tmux attach -t";
      mm = "sudo mount -m -v -o rw,uid=1000,gid=1000";
      msd = "sudo mount -m -v -o rw,noexec,uid=1000,gid=1000 UUID=3961-3035 /run/media/zenex/musicsd";
      umsd = "sudo umount -v /run/media/zenex/musicsd";
      mhd = "sudo mount -v -t ntfs -m -o rw,noexec,uid=1000,gid=1000 UUID=742455142454DAA6 /run/media/zenex/seagate";
      umhd = "sudo umount -v /run/media/zenex/seagate && lsblk";
      mssusb = "sudo mount -v -m -o rw,uid=1000,gid=1000 UUID=F5B6-E878 /run/media/zenex/silsam";
      umssusb = "sudo umount -v /run/media/zenex/silsam && lsblk";
      sysdlist = "systemctl list-unit-files --type=service --state=enabled";
      rsy = "rsync -ahPzRc --info=progress2";
      del = "trash-put";
      fnx = "find . -type f -exec chmod 644 {} +";
      dnx = "find . -type d -exec chmod 755 {} +";
      shx = "find . -name '*.sh' -execdir chmod +x {} +";
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
      WGETRC = "$XDG_CONFIG_HOME/wgetrc";
      DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      TERMINAL = "foot";
      EDITOR = "emacsclient -c -a emacs";
      VISUAL = "emacsclient -c -a emacs";
      FZF_DEFAULT_OPTS = "-e -i --no-scrollbar --border=none --reverse --no-info";
      LESSHISTFILE = "/tmp/.lesshst";
      MOZ_ENABLE_WAYLAND = 1;
      QT_QPA_PLATFORM = "wayland;xcb";
      GDK_BACKEND = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = 1;
      SAL_USE_VCLPLUGIN = "gtk3";
      XCURSOR_THEME = "plan9";
      XCURSOR_SIZE = 20;
      BEMENU_OPTS = ''-i --fn 'Ttyp0' -B '1' -f -p '>' -n --tb '#bdae93' --tf '#000000' --fb '#000000' --ff '#bdae93' --nb '#000000' --nf '#bdae93' --ab '#000000' --af '#bdae93' --sb '#000000' --sf '#bdae93' --cb '#bdae93' --cf '#bdae93' --hb '#bdae93' --hf '#000000' --sb '#bdae93' --sf '#000000' --scb '#000000' --scf '#bdae93' --bdr '#bdae93' '';
    };
    initExtra = ''
      PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND$'
      '}history -a; history -c; history -r"'';
  };
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/run/media/zenex/musicsd/Alt";
    dataDir = "/run/media/zenex/musicsd/.cache";
    dbFile = "/run/media/zenex/musicsd/.cache/tag_cache";
    network.startWhenNeeded = true;
    #change so instead of zenex it is the current user, do this also for the mounting, #change to a home.file
    extraConfig = ''
        audio_output {
           	type "pipewire"
            	name "pipewire"
        }
       #volume_normalization "yes"
      #replaygain "track"
    '';
  };

  programs.ncmpcpp = {
    enable = true;
    mpdMusicDir = "/run/media/zenex/musicsd/Alt";
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
      external_editor = "nvim";
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

  programs.htop = {
    enable = true;
    settings = {
      show_cpu_frequency = 1;
      show_cpu_temperature = 1;
    };
  };

  programs.zathura = {
    enable = true;
    mappings = {
      "<PageUp>" = "navigate previous";
      "<PageDown>" = "navigate next";
      "+" = "zoom in";
      "-" = "zoom out";
      "<C-q>" = "quit";
    };
    # options = {
    #   sandbox = "strict";
    #   database = "sqlite";
    # };
  };

  programs.tmux = {
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

  services.dunst = {
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

  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "Ttyp0:style=Regular:size=11";
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
        # background = "212121";
        background = "000000";
        foreground = "bdae93";
        regular0 = "444444"; # black
        regular1 = "B33929"; # red
        regular2 = "75B329"; # green
        regular3 = "c0c000"; # yellow
        regular4 = "2874B2"; # blue
        regular5 = "802caa"; # magenta
        regular6 = "6cb2eb"; # cyan
        regular7 = "bdae93"; # white

        bright0 = "666666"; # black
        bright1 = "f62b5a"; # red
        bright2 = "47b413"; # green
        bright3 = "e3c401"; # yellow
        bright4 = "24acd4"; # blue
        bright5 = "f2affd"; # magenta
        bright6 = "13c299"; # cyan
        bright7 = "e6e6e6"; # white
      };
      # colors = {
      #   # background = "212121";
      #   background = "000000";
      #   foreground = "bdae93";
      #   regular0 = "242424"; # black
      #   regular1 = "f62b5a"; # red
      #   regular2 = "47b413"; # green
      #   regular3 = "e3c401"; # yellow
      #   regular4 = "24acd4"; # blue
      #   regular5 = "f2affd"; # magenta
      #   regular6 = "13c299"; # cyan
      #   regular7 = "e6e6e6"; # white

      #   # Bright colors ;(color palette 8-15)
      #   bright0 = "616161"; # bright black
      #   bright1 = "ff4d51"; # bright red
      #   bright2 = "35d450"; # bright green
      #   bright3 = "e9e836"; # bright yellow
      #   bright4 = "5dc5f8"; # bright blue
      #   bright5 = "feabf2"; # bright magenta
      #   bright6 = "24dfc4"; # bright cyan
      #   bright7 = "ffffff"; # bright white
      # };
    };
  };

  home.packages = with pkgs; [
    aria2
    # bsdgames
    # cljfmt
    # clojure
    # clojure-lsp
    # glib
    # codeberg-cli
    # ffmpegthumbnailer
    # imhex
    # kismet
    # macchanger
    # nodePackages.prettier
    # pyright
    # ripgrep
    # rlwrap # for the readline

    # sigrok-cli
    # gron # json grepper
    # fq # jq for binary formats
    # entr # run a command when files change
    # https://github.com/ducaale/xh # httpie replacement
    # https://viric.name/soft/ts/
    # https://www.gnu.org/software/parallel
    # hyprshade
    # hyprsunset
    # nixfmt-rfc-style
    # kdePackages.kdeconnect-kde
    # mpvScripts.mpris
    age
    alacritty
    alejandra
    alsa-utils
    anki-bin
    astyle
    bc
    bemenu
    cargo
    ccls
    clifm
    cliphist
    cryptsetup
    cutter
    cabal-install
    dmenu
    exfatprogs
    exif
    fd
    ffmpeg
    file
    fuse3
    gcc
    gdb
    gh
    ghc
    haskellPackages.stack
    ghidra-bin
    radare2
    gimp
    gitFull
    tree
    glib
    gns3-gui
    gnumake
    gojq
    grim
    groff
    haskell-language-server
    htop
    hunspell
    hunspellDicts.en-gb-large
    ida-free
    imagemagick
    imv
    keepassxc
    libnotify
    libreoffice
    logisim-evolution
    lsof
    lxqt.lxqt-policykit
    magic-wormhole
    man-pages
    man-pages-posix
    mpc-cli
    mpv
    mupdf
    nemo
    nixd
    nodePackages.bash-language-server
    obs-studio
    openssl
    ormolu
    p7zip
    pandoc
    poppler_utils
    progress
    pulsemixer
    pv
    python3Full
    pkg-config
    ripgrep-all
    rsync
    ruff
    ruff-lsp
    sbcl
    sbctl
    sdcv
    shellcheck
    shfmt
    sioyek
    signal-desktop
    simplex-chat-desktop
    slurp
    smartmontools
    syncthing
    texliveFull
    traceroute
    trash-cli
    unzip
    usbutils
    ventoy-full
    vesktop
    virt-manager
    wdisplays
    wl-clip-persist
    wl-clipboard
    wl-color-picker
    wlr-randr
    wlsunset
    xdg-utils
    openvpn
    dig
    feather
    xmlformat
    yapf
    yt-dlp
    zip
    (aspellWithDicts (
      dicts:
        with dicts; [
          en
          en-computers
          en-science
        ]
    ))
  ];

  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
