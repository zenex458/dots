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

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: [ epkgs.pdf-tools ];
    extraConfig = ''
      (use-package pdf-tools
        :magic ("%PDF" . pdf-view-mode)
        :hook (pdf-view-mode . pdf-view-themed-minor-mode)
        :config
        (setq pdf-info-epdfinfo-program "${pkgs.emacsPackages.pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-20230611.239/epdfinfo")
        (pdf-tools-install))
    '';
  };

  xdg = {
    mime = { enable = true; };
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
        "application/pdf" = "org.pwmt.zathura.desktop";
        "application/vnd.ms-powerpoint" = "libreoffice-impress.desktop;";
        "application/vnd.ms-powerpoint.presentation" =
          "libreoffice-impress.desktop;";
        "application/vnd.ms-powerpoint.template" =
          "libreoffice-impress.desktop;";
        "application/vnd.ms-word" = "libreoffice-writer.desktop;";
        "application/vnd.ms-word.document" = "libreoffice-writer.desktop;";
        "application/vnd.ms-word.template" = "libreoffice-writer.desktop;";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
        "inode/directory" = "pcmanfm.desktop";
      };
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
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
        "privacy.resistFingerprinting" = true;
        "browser.link.open_newwindow" = 3;
        "browser.sessionstore.interval" = 9999999;
        "browser.startup.page" = 0;
        "dom.allow_cut_copy" = false;
        "dom.security.https_only_mode_send_http_background_request" = false;
        "dom.serviceWorkers.enabled" = false;
        "gfx.font_rendering.opentype_svg.enabled" = false;
        "privacy.clearOnShutdown.formdata" = true;
        "privacy.clearOnShutdown.history" = true;
        "privacy.cpd.cookies" = true;
        "privacy.donottrackheader.enabled" = false;
        "privacy.resistFingerprinting.letterboxing" = true;
        "privacy.userContext.enabled" = true;
        "privacy.userContext.ui.enabled" = true;
        "security.ask_for_password" = 0;
        "security.insecure_connection_text.enabled" = true;
        "security.mixed_content.block_display_content" = true;
        "security.pki.sha1_enforcement_level" = 1;
        "security.ssl.require_safe_negotiation" = true;
        "signon.formlessCapture.enabled" = false;
        "keyword.enabled" = false;
        "network.cookie.cookieBehavior" = 2;
        "dom.event.clipboardevents.enabled" = false;
        "media.gmp-widevinecdm.enabled" = false;
        "media.navigator.enabled" = false;
        "permissions.default.geo" = 2;
        "permissions.default.camera" = 2;
        "permissions.default.microphone" = 2;
        "permissions.default.desktop-notification" = 2;
        "permissions.default.xr" = 2;
        "media.peerconnection.enabled" = false;
        "privacy.clearOnShutdown.cookies" = true;
        "places.history.enabled" = false;
        "permissions.memory_only" = true;
        "browser.sessionstore.max_tabs_undo" = 0;
        "browser.sessionstore.resume_from_crash" = false;
        "browser.urlbar.suggest.history" = false;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.suggest.engines" = false;
        "extensions.screenshots.disabled" = true;
        "javascript.options.ion" = false;
        "javascript.options.baselinejit" = false;
        "javascript.options.wasm" = false;
        "javascript.options.asmjs" = false;
        "browser.tabs.firefox-view" = false;
        "browser.compactmode.show" = true;
        "browser.uidensity" = 1;
        "browser.region.networ=.url" = "";
        "browser.region.update.enabled" = false;
        "beacon.enabled" = false;
        "browser.safebrowsing.provider.google4.dataSharingURL" = "";
        "browser.urlbar.trimURLs" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "signon.rememberSignons" = false;
        "signon.autofillForms" = false;
        "browser.pagethumbnails.capturing_disabled" = true;
        "security.cert_pinning.enforcement_level" = 2;
        "media.peerconnection.ice.no_host" = true;
        "privacy.partition.always_partition_third_party_non_cookie_storage.exempt_sessionstorage" =
          true;
        "network.dnsCacheEntries" = 0;
        "network.trr.uri" = "https://all.dns.mullvad.net/dns-query";
        "extensions.formautofill.creditCards.enabled" = false;
        "general.smoothScroll" = false;
        "app.update.service.enabled" = false;
        "app.update.silent" = false;
        "app.update.staging.enabled" = false;
        "browser.bookmarks.max_backups" = 5;
        "browser.cache.memory.enable" = true;
        "browser.cache.memory.capacity" = -1;
        "browser.cache.offline.enable" = false;
        "browser.contentblocking.report.lockwise.enabled" = false;
        "browser.contentblocking.report.monitor.enabled" = false;
        "browser.display.use_document_fonts" = 0;
        "browser.download.autohideButton" = true;
        "browser.download.folderList" = 1;
        "browser.download.forbid_open_with" = false;
        "browser.library.activity-stream.enabled" = false;
        "browser.link.open_newwindow.override.external" = 3;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
          false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
          false;
        "browser.safebrowsing.allowOverride" = false;
        "browser.safebrowsing.blockedURIs.enabled" = false;
        "browser.safebrowsing.downloads.enabled" = false;
        "browser.safebrowsing.downloads.remote.block_dangerous" = false;
        "browser.safebrowsing.downloads.remote.block_dangerous_host" = false;
        "browser.safebrowsing.downloads.remote.block_potentially_unwanted" =
          false;
        "browser.safebrowsing.downloads.remote.block_uncommon" = false;
        "browser.safebrowsing.malware.enabled" = false;
        "browser.safebrowsing.phishing.enabled" = false;
        "browser.safebrowsing.provider.google.getha=hURL" = "";
        "browser.safebrowsing.provider.google.repo=tURL" = "";
        "browser.safebrowsing.provider.google.upda=eURL" = "";
        "browser.safebrowsing.provider.google4.getha=hURL" = "";
        "browser.safebrowsing.provider.google4.repo=tURL" = "";
        "browser.safebrowsing.provider.google4.upda=eURL" = "";
        "browser.safebrowsing.reportPhi=hURL" = "";
        "browser.search.widget.inNavBar" = false;
        "browser.sessionstore.cleanup.forget_closed_after" = 600;
        "browser.sessionstore.interval.idle" = 9999999;
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.tabs.allowTabDetach" = true;
        "browser.tabs.loadDivertedInBackground" = true;
        "browser.tabs.loadInBackground" = true;
        "browser.triple_click_selects_paragraph" = true;
        "browser.urlbar.autoFill" = false;
        "browser.urlbar.formatting.enabled" = true;
        "browser.urlbar.suggest.openpage" = false;
        "devtools.aboutdebugging.showSystemAddons" = true;
        "devtools.toolbox.zoomValue" = "=.2";
        "dom.battery.enabled" = false;
        "dom.push.connection.enabled" = false;
        "dom.push.userAge=tID" = "";
        "dom.webnotifications.enabled" = false;
        "dom.webnotifications.serviceworker.enabled" = false;
        "extensions.pocket.enabled" = false;
        "extensions.webextensions.restrict=dDomains" = "";
        "extensions.webextensions.userScripts.enabled" = true;
        "findbar.highlightAll" = true;
        "full-screen-api.warning.delay" = 0;
        "full-screen-api.warning.timeout" = 0;
        "general.autoScroll" = false;
        "identity.fxaccounts.enabled" = false;
        "layout.spellcheckDefault" = 0;
        "mathml.disabled" = true;
        "media.autoplay.default" = 5;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "middlemouse.paste" = true;
        "mousewheel.with_shift.action" = 3;
        "network.manage-offline-status" = false;
        "network.cookie.lifetimePolicy" = 2;
        "network.trr.mode" = 3;
        "nglayout.enable_drag_images" = false;
        "privacy.trackingprotection.cryptomining.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.pbmode.enabled" = true;
        "reader.parse-on-load.enabled" = false;
        "security.osclientcerts.autoload" = true;
        "security.tls.version.min" = 1;
        "signon.generation.enabled" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "privacy.antitracking.enableWebcompat" = false;
        "browser.translations.enable" = false;
        "extensions.quarantinedDomains.enabled" = true;
        "extensions.quarantinedDomain=.list" = "";

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
  home.sessionPath = [ "$HOME/.local/bin" "$HOME/.dotnet/tools" ];
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
      upd = "sudo nixos-rebuild switch --flake ~/.config/Nixos/#eukaryotic";
      updflake = "nix flake update --commit-lock-file";
      #      upd = "sudo nix-channel --update && sudo nixos-rebuild switch";
      #     enc = "sudo $EDITOR /etc/nixos/configuration.nix";
      #    updc = "sudo nixos-rebuild switch";
      listnixgen =
        "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
      nixgc = "nix-collect-garbage";
      remoldgen = "sudo nix-collect-garbage --delete-older-than 2d && upd";
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
      cp = "cp -iva";
      rm = "rm -iv";
      ll = "ls -lA";
      tm = "ps auxww | grep";
      lines = "ls | wc -l";
      tk = "tmux kill-session";
      ws = "grim";
      wsc = ''"grim -g \\$(slurp)\""'';
      cco = "gcc -O -Wall -W -pedantic";
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
      rsy = "rsync -ahPzRc --info=progress2";
      del = "trash-put";
      fnx = "find . -type f -exec chmod 644 {} +";
      dnx = "find . -type d -exec chmod 755 {} +";
      shx = "find . -name '*.sh' -execdir chmod +x {} +";
      dow = "aria2c -c -s 16 -x 16 -k 1M -j 1";
      kremap = "setxkbmap -option altwin:ctrl_alt_win";
      krremap = "setxkbmap -option -layout gb";
      chkfstab = "sudo findmnt --verify";
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
      FZF_DEFAULT_OPTS = "-e --no-scrollbar --border=none --reverse --no-info";
      LESSHISTFILE = "/tmp/.lesshst";
      MOZ_ENABLE_WAYLAND = "1";
      BEMENU_OPTS = ''
        -i --fn "Iosevka" --tb "#c6c6c6" --tf "#212121" --nb "#212121" --nf "#c6c6c6" --sf "#c6c6c6" --sb "#212121"  --hb "#c6c6c6" --hf "#212121"'';
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

  services.mpd = {
    enable = true;
    musicDirectory = "/run/media/zenex/musicsd/Alt";
    #change so instead of zenex it is the current user, do this also for the mounting
    extraConfig = ''
           audio_output {
              	type "pulse" #alsa
               	name "pulse" #alsa
           }
          #volume_normalization "yes"
      	  replaygain "track"
    '';
  };

  programs.ncmpcpp = {
    enable = true;
    mpdMusicDir = "/run/media/zenex/musicsd/Alt";
    settings = {
      ncmpcpp_directory = "$HOME/.config/ncmpcpp";
      mpd_crossfade_time = 1;
      header_visibility = "yes";
      lyrics_directory = "";
      current_item_prefix = "$(white)$r";
      current_item_inactive_column_prefix = "$(white)$r";
      browser_sort_mode = "name";
      browser_sort_format = "{%a - }{%t}{%b}|{%f} {%l}";
      song_columns_list_format =
        "(20)[]{a} (6f)[white]{NE} (50)[white]{t|f:Title} (20)[white]{b} (7f)[white]{l}";
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
    bindings = [ ];
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
    options = {
      sandbox = "strict";
      database = "sqlite";
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    ## change bellow to use options from home manager
    extraConfig = ''
        #
      # Please note not all available settings / options are set here.
      # For a full list, see the wiki
      #

      autogenerated = 0 # remove this line to remove the warning

      # See https://wiki.hyprland.org/Configuring/Monitors/
      monitor=HDMI-A-1,1280x1024,0x0, 1
      monitor=HDMI-A-2,1280x1024,1280x0, 1
      monitor=eDP-1,1920x1080, 2560x0, 1


      # See https://wiki.hyprland.org/Configuring/Keywords/ for more

      # Execute your favorite apps at launch
      exec-once = dunst
      exec-once = lxqt-policykit-agent
      exec-once = gammastep -C ~/.config/gammastep/config.ini
      exec-once = foot --server
      exec-once = hyprpaper


      # Source a file (multi-file configs)
      # source = ~/.config/hypr/myColors.conf

      # Some default env vars.
      env = XCURSOR_SIZE,24

      # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
      input {

          kb_layout = gb
          kb_variant =
          kb_model =
          kb_options = altwin:ctrl_alt_win
          kb_rules =
          repeat_rate=60
          repeat_delay=200

          follow_mouse = 1

          touchpad {
              natural_scroll = no
          }

          sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
      }

      general {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          gaps_in = 0
          gaps_out = 0
          border_size = 2
          col.active_border = rgba(999999ff)
          col.inactive_border = rgba(000000aa)

          layout = master

          # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
          allow_tearing = false
      	cursor_inactive_timeout = 5

      }

      decoration {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more

          rounding = 0

          blur {
              enabled = false
              size = 3
              passes = 1
          }

          drop_shadow = no
          shadow_range = 0
          shadow_render_power = 0
          col.shadow = rgba(1a1a1aee)
      }

      animations {
          enabled = no

          # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

          bezier = myBezier, 0.05, 0.9, 0.1, 1.05

          animation = windows, 1, 7, myBezier
          animation = windowsOut, 1, 7, default, popin 80%
          animation = border, 1, 10, default
          animation = borderangle, 1, 8, default
          animation = fade, 1, 7, default
          animation = workspaces, 1, 6, default
      }

      dwindle {
          # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
          pseudotile = yes # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          preserve_split = yes # you probably want this
      }

      master {
          # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
          new_is_master = true
      	no_gaps_when_only = 2
      }

      gestures {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          workspace_swipe = off
      }

      misc {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          force_default_wallpaper = 0 # Set to 0 to disable the anime mascot wallpapers

      }

      # Example per-device config
      # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
      device:epic-mouse-v1 {
          sensitivity = -0.5
      }

      # Example windowrule v1
      # windowrule = float, ^(kitty)$
      # Example windowrule v2
      # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
      windowrulev2 = float,class:^(footclient)$,title:^(pulsemixer)$

      # See https://wiki.hyprland.org/Configuring/Keywords/ for more
      $mainMod = SUPER

      # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
      bind = $mainMod, Return, exec, footclient tmux
      bind = $mainMod, A, exec, footclient -T pulsemixer pulsemixer
      bind = $mainMod SHIFT, Q, killactive,
      bind = $mainMod SHIFT CTRL, Z, exit,
      bind = $mainMod, V, togglefloating
      bind = $mainMod, C, exec, firejail firefox
      bind = $mainMod SHIFT, C, exec, firejail firefox --private-window
      bind = $mainMod, U, exec, emacsclient -c -a emacs
      bind = $mainMod, P, exec, bemenu-run
      bind = $mainMod SHIFT, O, exec, mpc next
      bind = $mainMod SHIFT, I, exec, mpc prev
      bind = $mainMod SHIFT, P, exec, mpc toggle
      bind = $mainMod SHIFT, Prior, exec, light -A 2
      bind = $mainMod SHIFT, Next, exec, light -U 2
      bind = ALT, Tab, exec, show.sh

      bind = $mainMod, F, fullscreen, 0

      bind = $mainMod, W, focusmonitor, HDMI-A-1
      bind = $mainMod, E, focusmonitor, HDMI-A-2
      bind = $mainMod, R, focusmonitor, eDP-1

      bind = $mainMod, h, movefocus, l
      bind = $mainMod, l, movefocus, r
      bind = $mainMod, j, movefocus, u
      bind = $mainMod, k, movefocus, d

      # Switch workspaces with mainMod + [0-9]
      # bind = SUPER , tab , focusworkspaceoncurrentmonitor , previous # Toggle Workspace
      # bind = SUPER , 1   , focusworkspaceoncurrentmonitor , 1
      # bind = SUPER , 2   , focusworkspaceoncurrentmonitor , 2
      # bind = SUPER , 3   , focusworkspaceoncurrentmonitor , 3
      # bind = SUPER , 4   , focusworkspaceoncurrentmonitor , 4
      # bind = SUPER , 5   , focusworkspaceoncurrentmonitor , 5
      # bind = SUPER , 6   , focusworkspaceoncurrentmonitor , 6
      # bind = SUPER , 7   , focusworkspaceoncurrentmonitor , 7
      # bind = SUPER , 8   , focusworkspaceoncurrentmonitor , 8
      # bind = SUPER , 9   , focusworkspaceoncurrentmonitor , 9

      bind = $mainMod, 1, workspace, 1
      bind = $mainMod, 2, workspace, 2
      bind = $mainMod, 3, workspace, 3
      bind = $mainMod, 4, workspace, 4
      bind = $mainMod, 5, workspace, 5
      bind = $mainMod, 6, workspace, 6
      bind = $mainMod, 7, workspace, 7
      bind = $mainMod, 8, workspace, 8
      bind = $mainMod, 9, workspace, 9
      bind = $mainMod, 0, workspace, 10

      # Move active window to a workspace with mainMod + SHIFT + [0-9]
      bind = $mainMod SHIFT, 1, movetoworkspace, 1
      bind = $mainMod SHIFT, 2, movetoworkspace, 2
      bind = $mainMod SHIFT, 3, movetoworkspace, 3
      bind = $mainMod SHIFT, 4, movetoworkspace, 4
      bind = $mainMod SHIFT, 5, movetoworkspace, 5
      bind = $mainMod SHIFT, 6, movetoworkspace, 6
      bind = $mainMod SHIFT, 7, movetoworkspace, 7
      bind = $mainMod SHIFT, 8, movetoworkspace, 8
      bind = $mainMod SHIFT, 9, movetoworkspace, 9
      bind = $mainMod SHIFT, 0, movetoworkspace, 10

      # Move/resize windows with mainMod + LMB/RMB and dragging
      bindm = $mainMod, mouse:272, movewindow
      bindm = $mainMod, mouse:273, resizewindow

    '';
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
      		set -s set-clipboard external
              set -g status-style fg=#c6c6c6,bg=#212121
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

  services.gammastep = {
    enable = true;
    dawnTime = "7:00";
    duskTime = "20:00";
    provider = "manual";
    temperature = {
      day = 6300;
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

  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "Iosevka:size=11";
        dpi-aware = "yes";
      };
      mouse = { hide-when-typing = "yes"; };
      cursor = {
        style = "block";
        blink = "yes";
      };
      colors = {
        background = "212121";
        foreground = "c6c6c6";
        regular0 = "242424"; # black
        regular1 = "f62b5a"; # red
        regular2 = "47b413"; # green
        regular3 = "e3c401"; # yellow
        regular4 = "24acd4"; # blue
        regular5 = "f2affd"; # magenta
        regular6 = "13c299"; # cyan
        regular7 = "e6e6e6"; # white

        # Bright colors ;(color palette 8-15)
        bright0 = "616161"; # bright black
        bright1 = "ff4d51"; # bright red
        bright2 = "35d450"; # bright green
        bright3 = "e9e836"; # bright yellow
        bright4 = "5dc5f8"; # bright blue
        bright5 = "feabf2"; # bright magenta
        bright6 = "24dfc4"; # bright cyan
        bright7 = "ffffff"; # bright white

      };
    };
  };

  # enable this in 24.05
  # services.hyprpaper = {
  #   enable = true;
  #   ipc = "off";
  #   preload = [ "~/Downloads/Images/klemg.jpeg" ];
  #   wallpaper = [ ",~/Downloads/Images/klemg.jpeg" ];
  # };

  programs.swaylock = {
    enable = true;
    settings = {
      color = "808080";
      font-size = 24;
      indicator-idle-visible = false;
      indicator-radius = 100;
      line-color = "ffffff";
      show-failed-attempts = true;
    };
  };

  home.packages = with pkgs; [
    neovim
    git
    htop
    hyprpaper
    grim
    slurp
    ffmpeg
    ffmpegthumbnailer
    gojq
    wdisplays
    swaylock
    bemenu
    alacritty
    haskell-language-server
    trash-cli
    libreoffice
    hunspell
    hunspellDicts.en_GB-large
    hunspellDicts.en-gb-large
    libnotify
    mupdf
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
    asunder
    ccls
    lsof
    gimp
    imagemagick
    astyle
    nixfmt
    nil
    ormolu
    shfmt
    mpc-cli
    pandoc
    lxqt.lxqt-policykit
    texliveFull
    traceroute
    fd
    ripgrep
    #    opensnitch-ui
    virt-manager
    xdg-utils
    git
    aria2
    smartmontools
    # dotnet-sdk
    # dotnetPackages.Nuget
    # csharpier
    # csharp-ls
    poppler_utils
    nodePackages.prettier
    nodePackages.bash-language-server
    imv
    anki-bin
    #    nwg-panel
    obs-studio
    croc
    ghc
    cinnamon.nemo
    man-pages
    man-pages-posix
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
  ];

  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
