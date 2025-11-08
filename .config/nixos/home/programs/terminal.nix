{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  manual.manpages.enable = true;
  programs = {
    man = {
      enable = true;
      generateCaches = false;
    };
    nh = {
      enable = true;
      clean.enable = false;
      flake = "${config.home.homeDirectory}" + "/.config/nixos";
    };
    foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          term = "xterm-256color";
          font = "ttyp0:style=regular:size=10";
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
          background = "060606";
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
        set -s set-clipboard external
        set -g status-style "fg=#bdae93,bg=#060606"
        setw -g monitor-activity on
        set -g visual-activity on
        set -g status-right ""
        set -g status-left "#{session_group}"
        set -g window-status-current-format "#{?window_zoomed_flag,#W#(echo \"(Z)\"),#W}" #if zoomed then show (z), if not, then show the name of the window
        set -g window-status-last-style "fg=#a08a64 bg=#060606"
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

    ncmpcpp = {
      enable = true;
      mpdMusicDir = "${config.home.homeDirectory}" + "/Music/Alt";
      settings = {
        enable_window_title = "no";
        ncmpcpp_directory = "${config.home.homeDirectory}" + ".config/ncmpcpp";
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
    fd = {
      enable = true;
      hidden = true;
      ignores = [".git/" ".ccls-cache/" "*env*"];
    };
    neovim = {
      enable = true;
      extraLuaConfig = ''
        vim.wo.number = true
        vim.wo.relativenumber = true
        vim.o.termguicolors = true
        vim.g.mapleader = " "
        vim.o.statusline = "%<%f%m   %= %R%H%W %l/%L:%c %p%% "
        vim.o.clipboard = "unnamedplus"
        vim.keymap.set("i", "jk", [[<ESC>]])
      '';
    };
    git = {
      enable = true;
      extraConfig = {
        core = {
          compression = 9;
        };
        init = {
          defaultBranch = "main";
        };
        status = {
          showUntrackedFiles = "all";
        };
        diff = {
          algorithm = "histogram";
          interHunkContext = 10;
          colorMoved = "plain";
        };
        # commit = {
        #   verbose = "true"; #enable this if you don't use emacs
        # };
        url = {
          "git@github.com:" = {
            insteadOf = "gh:";
          };
        };
        column = {
          ui = "auto";
        };
        branch = {
          sort = "committerdate";
        };
        help = {
          autocorrect = "prompt";
        };
      };
    };
  };
}
