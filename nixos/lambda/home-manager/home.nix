# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{ inputs, outputs, lib, config, pkgs, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.example

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "zenex";
    homeDirectory = "/home/zenex";
  };
  manual.manpages.enable = true;
  programs.man.enable = true;
  #  programs.man.generateCaches = true;

  services.emacs = {
    enable = true;
    package = pkgs.emacs29-gtk3;
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style.package = pkgs.adwaita-qt;
    style.name = "adwaita-dark";
  };

  gtk = {
    enable = true;
    theme.package = pkgs.shades-of-gray-theme;
    theme.name = "Shades-of-gray";
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper-Mono-Dark";
    cursorTheme.name = "Paper";
    font.package = pkgs.iosevka;
    font.name = "Iosevka Extended";
    font.size = 10;
  };
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
  };
  programs.firefox = {
    enable = true;
    profiles.priv = {
      id = 1;
      search.engines = {
        "Bing".metaData.hidden = true;
        "Google".metaData.hidden = true;
        "Amazon.co.uk".metaData.hidden = true;
        "eBay".metaData.hidden = true;
      };
      search.force = true;
      userChrome = ''
        /* hides the native tabs */
        #TabsToolbar {
          visibility: collapse;
        }

        /* https://gist.github.com/chris-vecchio/d6a47fc733559752cc3a09937381d7ae */
        /* Firefox userChrome.css */

        /*** PROTON TABS TWEAKS ***/
        /* SOURCE: modified version of https://www.userchrome.org/firefox-89-styling-proton-ui.html#tabstyler */
        /* Make tab shape square */
        #tabbrowser-tabs {
          --user-tab-rounding: 0px;
        }

        .tab-background {
          border-radius: var(--user-tab-rounding) var(--user-tab-rounding) 0px 0px !important;
          margin-block: 1px 0 !important;
        }

        /* Borders on tab scroll right and left buttons */
        #scrollbutton-up, #scrollbutton-down { /* 6/10/2021 */
          border-top-width: 1px !important;
          border-bottom-width: 0 !important;
        }

        /* Inactive tabs: Separator line style */
        /* For light backgrounds */
        .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
          border-right: 1px solid var(--lwt-background-tab-separator-color, rgba(0, 0, 0, .20)) !important;
        }

        /* For dark backgrounds */
        [brighttext="true"] .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
          border-right: 1px solid var(--lwt-background-tab-separator-color, var(--lwt-selected-tab-background-color, rgba(255, 255, 255, .20))) !important;
        }

        .tabbrowser-tab:not([selected=true]):not([multiselected=true]) .tab-background {
          border-radius: 0 !important;
        }

        /* Remove padding between tabs */
        .tabbrowser-tab {
          padding-left: 0 !important;
          padding-right: 0 !important;
        }

        /* Set tab fill color and text color */
        #TabsToolbar {
          background-color: #202340;
          color: #F9F9FA;
        }
        /*** END PROTON TABS TWEAKS ***/


        /*** TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/
        /* SOURCE: https://www.userchrome.org/firefox-89-styling-proton-ui.html#menuspacing */
        menupopup > menuitem, menupopup > menu {
          padding-block: 4px !important;
        }

        /* Tighten up hamburger menu spacing and square the edges */
        :root {
          --arrowpanel-menuitem-padding: 2px !important;
          --arrowpanel-border-radius: 0px !important;
          --arrowpanel-menuitem-border-radius: 0px !important;
        }
        /*** END TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/

      '';
      settings = {
        "browser.translations.enable" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data: =";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "browser.ping-centre.telemetry" = false;

      };

    };
    profiles."work" = {
      search.engines = {
        "Bing".metaData.hidden = true;
        "Google".metaData.hidden = true;
        "Amazon.co.uk".metaData.hidden = true;
        "eBay".metaData.hidden = true;
        "Startpage" = {
          urls = [{
            template = "https://www.startpage.com/do/search";
            params = [{
              name = "query";
              value = "{searchTerms}";
            }];
          }];
        };

      };
      search.default = "Startpage";
      search.force = true;
      userChrome = ''
        /* hides the native tabs */
        #TabsToolbar {
          visibility: collapse;
        }

        /* https://gist.github.com/chris-vecchio/d6a47fc733559752cc3a09937381d7ae */
        /* Firefox userChrome.css */

        /*** PROTON TABS TWEAKS ***/
        /* SOURCE: modified version of https://www.userchrome.org/firefox-89-styling-proton-ui.html#tabstyler */
        /* Make tab shape square */
        #tabbrowser-tabs {
          --user-tab-rounding: 0px;
        }

        .tab-background {
          border-radius: var(--user-tab-rounding) var(--user-tab-rounding) 0px 0px !important;
          margin-block: 1px 0 !important;
        }

        /* Borders on tab scroll right and left buttons */
        #scrollbutton-up, #scrollbutton-down { /* 6/10/2021 */
          border-top-width: 1px !important;
          border-bottom-width: 0 !important;
        }

        /* Inactive tabs: Separator line style */
        /* For light backgrounds */
        .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
          border-right: 1px solid var(--lwt-background-tab-separator-color, rgba(0, 0, 0, .20)) !important;
        }

        /* For dark backgrounds */
        [brighttext="true"] .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
          border-right: 1px solid var(--lwt-background-tab-separator-color, var(--lwt-selected-tab-background-color, rgba(255, 255, 255, .20))) !important;
        }

        .tabbrowser-tab:not([selected=true]):not([multiselected=true]) .tab-background {
          border-radius: 0 !important;
        }

        /* Remove padding between tabs */
        .tabbrowser-tab {
          padding-left: 0 !important;
          padding-right: 0 !important;
        }

        /* Set tab fill color and text color */
        #TabsToolbar {
          background-color: #202340;
          color: #F9F9FA;
        }
        /*** END PROTON TABS TWEAKS ***/


        /*** TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/
        /* SOURCE: https://www.userchrome.org/firefox-89-styling-proton-ui.html#menuspacing */
        menupopup > menuitem, menupopup > menu {
          padding-block: 4px !important;
        }

        /* Tighten up hamburger menu spacing and square the edges */
        :root {
          --arrowpanel-menuitem-padding: 2px !important;
          --arrowpanel-border-radius: 0px !important;
          --arrowpanel-menuitem-border-radius: 0px !important;
        }
        /*** END TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/

      '';
      settings = {
        "browser.search.region" = "GB";
        "browser.search.isUS" = false;
        "distribution.searchplugins.defaultLocale" = "en-GB";
        "intl.accept_languages" = "en-GB, en";
        "browser.translations.enable" = false;
        "keyword.enabled" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "browser.shell.shortcutFavicons" = true;
        "extensions.pocket.enabled" = false;
        "identity.fxaccounts.enabled" = false;
        "permissions.default.geo" = 2;
        "webgl.disabled" = false;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "privacy.clearOnShutdown.cookies" = true;
        "places.history.enabled" = true;
        "signon.rememberSignons" = false;
        "browser.cache.memory.enable" = false;
        "browser.cache.memory.capacity" = 0;
        "permissions.memory_only" = true;
        "browser.urlbar.suggest.history" = true;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.openpage" = true;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.suggest.engines" = false;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.firefox-view-newIcon" = false;
        "browser.tabs.firefox-view-next" = false;
        "browser.compactmode.show" = false;
        "browser.uidensity" = 1;
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.heuristics.enabled" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "signon.autofillForms" = false;
        "privacy.resistFingerprinting.letterboxing" = false;
        "media.eme.enabled" = true;
        "general.smoothScroll" = false;
        "media.hardwaremediakeys.enabled" = false;
        "beacon.enabled" = false;
        "browser.link.open_newwindow" = 3;
        "browser.startup.page" = 0;
        "privacy.clearOnShutdown.formdata" = true;
        "browser.urlbar.trimURLs" = false;
        "extensions.getAddons.showPane" = false;
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
        "browser.discovery.enabled" = false;
        "browser.shopping.experience2023.enabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data: =";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "browser.ping-centre.telemetry" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "app.shield.optoutstudies.enabled" = false;
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "breakpad.reportURL" = "";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
        "captivedetect.canonicalURL" = "";
        "network.captive-portal-service.enabled" = false;
        "network.connectivity-service.enabled" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
          false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
          false;
        "browser.download.useDownloadDir" = false;
        "browser.download.always_ask_before_handling_new_types" = true;
        "browser.search.suggest.enabled" = false;
        "browser.urlbar.suggest.searches" = false;
        "browser.newtabpage.enabled" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.default.sites" = "";
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;
        "privacy.clearOnShutdown.offlineApps" = true;
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "privacy.clearOnShutdown.cache" = true;
        "privacy.clearOnShutdown.downloads" = true;
        "privacy.clearOnShutdown.sessions" = true;
        "privacy.clearOnShutdown.history" = false;
        "privacy.cpd.history" = true;
        "browser.formfill.enable" = false;
        "privacy.clearOnShutdown.siteSettings" = true;
        "privacy.cpd.siteSettings" = true;
        "signon.management.page.breach-alerts.enabled" = false;
        "intl.regional_prefs.use_os_locales" = true;
        "extensions.screenshots.disabled" = true;
      };
    };
  };
  home.sessionPath = [ "$HOME/.local/bin" ];
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };
  programs.bash = {
    enableCompletion = true;
    historyControl = [ "ignoredups" ];
    historyFile = "$HOME/.local/share/.bash_history";
    historyFileSize = 10000;
    historySize = 10000;
    enable = true;
    shellOptions = [ "cdspell" "autocd" "histappend" ];
    bashrcExtra = ''
      bind 'set show-all-if-ambiguous on'
      bind 'set completion-ignore-case on'
      bind 'TAB:menu-complete'
    '';
    shellAliases = {
      upd = "sudo nixos-rebuild switch --flake ~/lambda/#eukaryotic";
      updflake = "nix flake update --commit-lock-file";
      #      upd = "sudo nix-channel --update && sudo nixos-rebuild switch";
      #     enc = "sudo $EDITOR /etc/nixos/configuration.nix";
      #    updc = "sudo nixos-rebuild switch";
      nixgc = "nix-collect-garbage";
      remoldgen = "sudo nix-collect-garbage -d && upd";
      re = "systemctl reboot";
      off = "systemctl poweroff";
      nv = "nvim";
      ls = "ls -F -h --color=always --group-directories-first";
      ga = "git add";
      gc = "git commit -m";
      gp = "git push -u origin main";
      updoff = "upd && sleep 2 && off";
      updr = "upd && sleep 2 && re";
      grep = "grep -i --colour=always";
      mkdir = "mkdir -pv";
      mv = "mv -iv";
      cp = "cp -iv";
      rm = "rm -iv";
      ll = "ls -lA";
      tm = "ps auxww | grep";
      lines = "ls | wc -l";
      tk = "tmux kill-session";
      sss = "scrot -d 5 ~/Downloads/Images/ss/%Y-%m-%d_$wx$h.png";
      cco = "gcc -Wall";
      ytmp3 =
        "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format mp3 --audio-quality 0 --embed-thumbnail";
      ytflac_thum_chap =
        "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format flac --audio-quality 0 --embed-thumbnail --embed-chapters";
      ytflac_aud =
        "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format flac --audio-quality 0";
      yt10 =
        "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=1080][fps=30]+bestaudio/best[height<=1080]'";
      yt7 =
        "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
      yt7s =
        "yt-dlp --progress -q -o '%(title)s.%(ext)s' --sponsorblock-remove sponsor --remux-video mp4 --embed-subs; --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
      ytb =
        "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en";
      xdup = "xrandr --output HDMI-1 --same-as eDP-1";
      xright =
        "xrandr --output eDP1 --auto --primary --output HDMI1 --auto --right-of eDP1";
      xup = "xrandr --output HDMI-1 --brightness 1.0";
      chnum = "stat -c '%a %n'";
      tas = "tmux attach-session";
      tls = "tmux list-session";
      tat = "tmux attach -t";
      msd =
        "sudo mount -m -v -o rw,noexec,uid=1000,gid=1000 UUID=04C3-E2B3 /run/media/zenex/musicsd";
      umsd = "sudo umount -v /run/media/zenex/musicsd";
      mhd =
        "sudo mount -v -t ntfs -m -o rw,noexec,uid=1000,gid=1000 UUID=742455142454DAA6 /run/media/zenex/seagate";
      umhd = "sudo umount -v /run/media/zenex/seagate && lsblk";
      sysdlist = "systemctl list-unit-files --type=service --state=enabled";
      rsy = "rsync -ahPz --info=progress2";
      del = "trash-put";
      fnx = "find . -type f -exec chmod 644 {} +";
      dnx = "find . -type d -exec chmod 755 {} +";
      shx = "find . -name '*.sh' -execdir chmod +x {} +";
      dow = "aria2c -c -s 16 -x 16 -k 1M -j 1";
      kremap = "setxkbmap -option altwin:ctrl_alt_win";
      krremap = "setxkbmap -option";
      chkfstab = "sudo findmnt --verify";
      emdr = "systemctl --user restart emacs";
    };
    sessionVariables = {
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
      XDG_CACHE_HOME = "$HOME/.cache";
      MUPDFHISTFILE = "/tmp/.mupdf.history";
      RXVT_SOCKET = "$XDG_RUNTIME_DIR/urxvtd";
      URXVT_PERL_LIB = "~/.config/urxvt/ext";
      XSECURELOCK_SHOW_HOSTNAME = 0;
      XSECURELOCK_SHOW_USERNAME = 0;
      XSECURELOCK_FONT = "Iosevka";
      XSECURELOCK_WAIT_TIME_MS = 200000;
      WGETRC = "$XDG_CONFIG_HOME/wgetrc";
      DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      TERMINAL = "urxvt";
      EDITOR = "emacsclient -c -a emacs";
      VISUAL = "emacsclient -c -a emacs";
      FZF_DEFAULT_OPTS = "-e --no-scrollbar --border=none --reverse --no-info";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      LESSHISTFILE = "/tmp/.lesshst";
      MOZ_ENABLE_WAYLAND = "0";
    };
    initExtra = ''
      PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND$'
      '}history -a; history -c; history -r"'';
  };
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };
  services.unclutter = {
    enable = true;
    timeout = 5;
    extraOptions =
      [ "exclude-root" "ignore-scrolling" "ignore-buttons" "start-hidden" ];
  };

  xsession.windowManager = {
    xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    baseIndex = 0;
    escapeTime = 0;
    historyLimit = 100000;
    keyMode = "emacs";
    mouse = true;
    terminal = "tmux-256color";
    extraConfig = ''
        set -g set-titles on
        set -g status-style fg=#c6c6c6,bg=#141414
        setw -g monitor-activity on
        set -g visual-activity on
        set -g status-right ""
        set -g status-left "#{session_group}"
        set -g window-status-current-format "#[fg=black bg=black]|#[fg=white bg=black]#W#[fg=black bg=black]|"
        set -g window-status-last-style "fg=#444444 bg=black"
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

  programs.urxvt = {
    enable = true;
    fonts = [ "xft:Iosevka:Regular:pixelsize=16=antialias=true" ];
    package = pkgs.rxvt-unicode-unwrapped;
    iso14755 = false;
    scroll.bar.enable = false;
    scroll.lines = 0;
    keybindings = {
      "Shift-Control-C" = "eval:selection_to_clipboard";
      "Shift-Control-V" = "eval:paste_clipboard";
    };
    extraConfig = {
      cursorBlink = true;
      visualBell = true;
      foreground = "#c6c6c6";
      background = "#1A1A1A";
      cursorColor = "#c6c6c6";

      # black
      color0 = "#000000";
      color8 = "#808080";
      # red
      color1 = "#cd0000";
      color9 = "#ff0000";
      # green
      color2 = "#00cd00";
      color10 = "#00ff00";
      # yellow
      color3 = "#cdcd00";
      color11 = "#ffff00";
      # blue
      color4 = "#2B7Df0";
      color12 = "#0066FF";
      # magenta
      color5 = "#cd00cd";
      color13 = "#ff00ff";
      # cyan
      color6 = "#00cdcd";
      color14 = "#00ffff";
      # white
      color7 = "#e5e5e5";
      color15 = "#ffffff";

    };
  };

  services.redshift = {
    enable = true;
    dawnTime = "7:00";
    duskTime = "20:00";
    provider = "manual";
    temperature = {
      day = 5700;
      night = 2000;
    };
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
        frame_color = "#c6c6c6";
        font = "Iosevka Bold 10";
        vertical_alignment = "center";
        alignment = "center";
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";
        notification_limit = 0;
        follow = "mouse";

      };
      urgency_low = {
        background = "#333333";
        foreground = "#888888";
        timeout = 10;
      };

      urgency_normal = {
        background = "#141414";
        foreground = "#c6c6c6";
        timeout = 10;
      };

      urgency_critical = {
        background = "#FF0000";
        foreground = "#FFFFFF";
        frame_color = "#900000";
        timeout = 0;
      };

    };
  };

  home.packages = with pkgs; [
    neovim
    emacs29-gtk3
    git
    htop
    ffmpeg_6-full
    xterm
    alacritty
    haskell-language-server
    trash-cli
    dmenu
    libreoffice
    hunspell
    hunspellDicts.en_GB-large
    hunspellDicts.en-gb-large
    libnotify
    mupdf
    zathura
    bc
    p7zip
    zip
    unzip
    fuse3
    mpv
    mpvScripts.mpris
    keepassxc
    openssl
    alsa-utils
    pulsemixer
    shellcheck
    yt-dlp
    kdeconnect
    gcc
    astyle
    nixfmt
    nil
    ormolu
    shfmt
    mpd
    mpc-cli
    ncmpcpp
    pandoc
    lxqt.lxqt-policykit
    texlive.combined.scheme-full
    xfce.thunar
    fd
    ripgrep
    opensnitch-ui
    virt-manager
    xdg-utils
    git
    smartmontools
    dotnet-sdk_8
    poppler_utils
    nodePackages.prettier
    feh
    arandr
    ncmpcpp
    mpd
    man-pages
    man-pages-posix
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science uk ]))
  ];

  # Enable home-manager and git
  programs.home-manager.enable = true;
  ##  programs.git.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
