{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home.stateVersion = "24.05";
  home.file.".xinitrc" = {
    enable = true;
    text = ''
      xsetroot -cursor_name left_ptr &
      redshift -o -O 2000 &
      xrdb ~/.Xresources &
      exec xmonad
    '';
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
    keyMode = "vi";
    mouse = true;
    terminal = "tmux-256color";
    extraConfig = ''
      set -g set-titles on
      set -g status-keys emacs
      set -s set-clipboard external
      set -g status-style "fg=#bdae93,bg=#060606"
      setw -g monitor-activity on
      set -g visual-activity on
      set -g status-right ""
      set -g status-left "#{session_group}"
      set -g window-status-current-format "#[fg=#060606 bg=#060606]|#[fg=#bdae93 bg=#060606]#W#[fg=#060606 bg=#060606]|"
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

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      main = xmonad defaultConfig
          { terminal    = "xterm -e tmux"
          , modMask     = mod4Mask
          , borderWidth = 2
          }
    '';
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.multi-vterm
    ];
    extraConfig = ''
       (setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/saves/")))
       (setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/saves/" t)))
       (load-theme 'modus-vivendi t)
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
      set guicursor=i:hor50-Cursor
      set guicursor+=i:blinkon100
      set guicursor+=n-v-c:blinkon100
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
  xresources = {
    properties = {
      "XTerm.vt100.foreground" = "#c6c6c6";
      "XTerm.vt100.background" = "#000000";
      "XTerm.vt100.cursorColor" = "#c6c6c6";
    };
  };

  programs.alacritty = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
    policies = {
      AutofillCreditCardEnabled = false;
      AppAutoUpdate = false;
      BackgroundAppUpdate = false;
      DisableFirefoxAccounts = true;
      DisableFirefoxScreenshots = true;
      DisableFirefoxStudies = true;
      DisableMasterPasswordCreation = true;
      DisablePasswordReveal = true;
      DontCheckDefaultBrowser = true;
      DisablePocket = true;
      DisableSetDesktopBackground = true;
      DisableTelemetry = true;
      OfferToSaveLogins = false;
      OfferToSaveLoginsDefault = false;
      PasswordManagerEnabled = false;
      NoDefaultBookmarks = true;
    };
    profiles."work" = {
      search.engines = {
        "Bing".metaData.hidden = true;
        "Google".metaData.hidden = true;
        "Amazon.co.uk".metaData.hidden = true;
        "eBay".metaData.hidden = true;
        "Startpage" = {
          urls = [
            {
              template = "https://www.startpage.com/do/search";
              params = [
                {
                  name = "query";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconUpdateURL = "https://www.startpage.com/sp/cdn/favicons/favicon-32x32-gradient.png";
          updateInterval = 24 * 60 * 60 * 1000 * 30; #every month
          definedAliases = ["@st"];
        };
      };
      search.force = true;
      search.default = "Startpage";
      userChrome = ''
        #   /* hides the native tabs */
        #   #TabsToolbar {
        #     visibility: collapse;
        #   }

           # /* hides that annoying extension button */
           # #unified-extensions-button {
           #    display: none !important;
           # }

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
        "sidebar.main.tools" = "	history,bookmarks";
        "browser.startup.homepage" = "about:blank";
        "browser.theme.content-theme" = 0;
        "browser.theme.toolbar-theme" = 0;
        "accessibility.force_disabled" = 1;
        "app.normandy.api_url" = "";
        "app.normandy.enabled" = false;
        "app.shield.optoutstudies.enabled" = false;
        "beacon.enabled" = false;
        "breakpad.reportURL" = "";
        "browser.cache.memory.capacity" = 0;
        "browser.cache.memory.enable" = false;
        "browser.compactmode.show" = true;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
        "browser.discovery.enabled" = false;
        "browser.download.always_ask_before_handling_new_types" = true;
        "browser.download.useDownloadDir" = false;
        "browser.download.viewableInternally.enabledTypes" = "";
        "browser.formfill.enable" = false;
        "browser.startup.blankWindow" = true;
        "browser.link.open_newwindow" = 3;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.newtabpage.activity-stream.default.sites" = "";
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.newtabpage.enabled" = false;
        "browser.ping-centre.telemetry" = false;
        "browser.preferences.moreFromMozilla" = false;
        "browser.search.isUS" = false;
        "browser.search.region" = "GB";
        "browser.search.suggest.enabled" = false;
        "browser.shell.shortcutFavicons" = true;
        "browser.shopping.experience2023.enabled" = false;
        "browser.startup.page" = "about:blank";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.preferences.experimental" = false; # disables firefox labs
        "browser.tabs.firefox-view" = false;
        "browser.tabs.firefox-view-newIcon" = false;
        "browser.tabs.firefox-view-next" = false;
        "browser.translations.enable" = true;
        "browser.uidensity" = 1;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.history" = true;
        "browser.urlbar.suggest.openpage" = true;
        "browser.urlbar.suggest.recentsearches" = false;
        "browser.urlbar.suggest.searches" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.trimURLs" = false;
        "captivedetect.canonicalURL" = "";
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "distribution.searchplugins.defaultLocale" = "en-GB";
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "extensions.getAddons.showPane" = false;
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
        "extensions.pocket.enabled" = false;
        "extensions.screenshots.disabled" = true;
        "general.smoothScroll" = false;
        "identity.fxaccounts.enabled" = false;
        "intl.accept_languages" = "en-GB, en";
        "intl.regional_prefs.use_os_locales" = true;
        "keyword.enabled" = true;
        "media.eme.enabled" = true;
        "media.hardwaremediakeys.enabled" = false;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "network.captive-portal-service.enabled" = false;
        "network.connectivity-service.enabled" = false;
        "permissions.default.geo" = 2;
        "permissions.memory_only" = true;
        "places.history.enabled" = true;
        "privacy.clearOnShutdown.cache" = true;
        "privacy.clearOnShutdown.cookies" = true;
        "privacy.clearOnShutdown.downloads" = true;
        "privacy.clearOnShutdown.formdata" = true;
        "privacy.clearOnShutdown.history" = false;
        "privacy.clearOnShutdown.offlineApps" = true;
        "privacy.clearOnShutdown.sessions" = true;
        "privacy.clearOnShutdown.siteSettings" = true;
        "privacy.clearHistory.siteSettings" = true;
        "privacy.clearOnShutdown_v2.siteSettings" = true;
        "privacy.clearSiteData.siteSettings" = true;
        "privacy.cpd.history" = true;
        "privacy.cpd.siteSettings" = true;
        "privacy.resistFingerprinting.letterboxing" = false;
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "reader.parse-on-load.enabled" = false;
        "signon.autofillForms" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "signon.rememberSignons" = false;
        "toolkit.coverage.endpoint.base" = "";
        "toolkit.coverage.opt-out" = true;
        "sidebar.animation.enabled" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.server" = "data: =";
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.cosmeticAnimations.enabled" = false;
        "webgl.disabled" = false;
        "browser.ml.chat.provider" = "";
        "browser.ml.chat.shortcuts" = false;
        "browser.ml.chat.sidebar" = false;
        "browser.ml.enable" = false;
        "sidebar.verticalTabs" = true;
        "browser.tabs.hoverPreview.enabled" = false;
        "browser.tabs.inTitlebar" = 0;
      };
    };
  };
}
