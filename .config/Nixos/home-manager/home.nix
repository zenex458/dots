# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}:
{
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
    extraPackages = epkgs: [
      epkgs.pdf-tools
      epkgs.vterm
    ];
    extraConfig = ''
      (use-package pdf-tools
         :magic ("%PDF" . pdf-view-mode)
         :hook (pdf-view-mode . pdf-view-themed-minor-mode)
         :config
          (setq pdf-info-epdfinfo-program "${pkgs.emacsPackages.pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-20240411.1703/epdfinfo")
          (pdf-tools-install))
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
    theme.package = pkgs.shades-of-gray-theme;
    theme.name = "Shades-of-gray";
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper-Mono-Dark";
    cursorTheme.name = "Paper";
    font.package = pkgs.iosevka;
    font.name = "Iosevka Extended";
    font.size = 10;
  };
  # programs.chromium = {
  #   enable = true;
  #   package = pkgs.ungoogled-chromium;
  # };
  programs.firefox = {
    enable = true;
    policies = {
      DisableFirefoxScreenshots = true;
      DisablePocket = true;
      PasswordManagerEnabled = false;
      DisableFirefoxAccounts = true;
      DisableSetDesktopBackground = true;
      DisableTelemetry = true;
      AutofillCreditCardEnabled = false;
      DisableMasterPasswordCreation = true;
      DisablePasswordReveal = true;
      OfferToSaveLogins = false;
      OfferToSaveLoginsDefault = false;
    };
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
        "accessibility.force_disabled" = 1;
        "reader.parse-on-load.enabled" = false;
        "privacy.firstparty.isolate" = true;
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
        "privacy.partition.always_partition_third_party_non_cookie_storage.exempt_sessionstorage" = true;
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
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.safebrowsing.allowOverride" = false;
        "browser.safebrowsing.blockedURIs.enabled" = false;
        "browser.safebrowsing.downloads.enabled" = false;
        "browser.safebrowsing.downloads.remote.block_dangerous" = false;
        "browser.safebrowsing.downloads.remote.block_dangerous_host" = false;
        "browser.safebrowsing.downloads.remote.block_potentially_unwanted" = false;
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
        "browser.urlbar.suggest.recentsearches" = false;
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
        "security.osclientcerts.autoload" = true;
        "security.tls.version.min" = 1;
        "signon.generation.enabled" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "privacy.antitracking.enableWebcompat" = false;
        "browser.translations.enable" = false;
        "browser.preferences.moreFromMozilla" = false;
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
        };
        "Searx" = {
          urls = [
            {
              template = "https://priv.au/search";
              params = [
                {
                  name = "q";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
        };

      };
      search.default = "Searx";
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
        "accessibility.force_disabled" = 1;
        "reader.parse-on-load.enabled" = false;
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
        "browser.startup.page" = 0;
        "browser.tabs.crashReporting.sendReport" = false;
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
        "privacy.cpd.history" = true;
        "privacy.cpd.siteSettings" = true;
        "privacy.resistFingerprinting.letterboxing" = false;
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "signon.autofillForms" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "signon.rememberSignons" = false;
        "toolkit.coverage.endpoint.base" = "";
        "toolkit.coverage.opt-out" = true;
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
        "webgl.disabled" = false;
        #   "privacy.firstparty.isolate" = true;
      };
    };
  };
  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.dotnet/tools"
  ];
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
    shellOptions = [
      "cdspell"
      "autocd"
      "histappend"
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
      		ls -F -h --color=always
      	fi
      }
    '';
    shellAliases = {
      upd = "sudo nixos-rebuild switch --flake ~/dots/.config/Nixos/#eukaryotic";
      updflake = "nix flake update --commit-lock-file";
      #      upd = "sudo nix-channel --update && sudo nixos-rebuild switch";
      #     enc = "sudo $EDITOR /etc/nixos/configuration.nix";
      #    updc = "sudo nixos-rebuild switch";
      listnixgen = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
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
      msd = "sudo mount -m -v -o rw,noexec,uid=1000,gid=1000 UUID=04C3-E2B3 /run/media/zenex/musicsd";
      umsd = "sudo umount -v /run/media/zenex/musicsd";
      mhd = "sudo mount -v -t ntfs -m -o rw,noexec,uid=1000,gid=1000 UUID=742455142454DAA6 /run/media/zenex/seagate";
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
      QT_QPA_PLATFORM = "wayland";
      GDK_BACKEND = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = 1;
      SAL_USE_VCLPLUGIN = "gtk3";
      # BEMENU_OPTS = ''
      #   -i --fn "Iosevka" --tb "#c6c6c6" --tf "#212121" --nb "#212121" --nf "#c6c6c6" --sf "#c6c6c6" --sb "#212121"  --hb "#c6c6c6" --hf "#212121" --ab "#212121" --af "#c6c6c6"'';
      BEMENU_OPTS = ''
        --tb '#c6c6c6'
         --tf '#212121'
         --fb '#212121'
         --ff '#c6c6c6'
         --nb '#212121'
         --nf '#c6c6c6'
         --hb '#c6c6c6'
         --hf '#212121'
         --sb '#c6c6c6'
         --sf '#212121'
         --scb '#444444'
         --scf '#c6c6c6'
         -f
         -p '>'
         -n
         --fn 'Iosevka' '';
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
    extraConfig = builtins.readFile ./hyprland.conf;
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
        background = "212121";
        foreground = "bdae93";
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

  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = false;
      splash = false;
      preload = [ "~/Downloads/Images/realsat.jpg" ];
      wallpaper = [ ",~/Downloads/Images/realsat.jpg" ];
    };
  };

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        after_sleep_cmd = "hyprctl dispatch dpms on";
        ignore_dbus_inhibit = false;
        ignore_systemd_inhibit = false;
        lock_cmd = "swaylock";
      };

      listener = [
        {
          timeout = 900;
          on-timeout = "swaylock";
        }
        {
          timeout = 1200;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
      ];
    };
  };

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
    alacritty
    alsa-utils
    anki-bin
    aria2
    astyle
    bc
    bemenu
    bsdgames
    briar-desktop
    ccls
    clojure
    cinnamon.nemo
    clojure-lsp
    cljfmt
    chess-tui
    leiningen
    magic-wormhole
    fd
    ffmpeg
    ffmpegthumbnailer
    fuse3
    gcc
    gh
    ghc
    gimp
    git
    gojq
    grim
    haskell-language-server
    htop
    hunspell
    hunspellDicts.en-gb-large
    hunspellDicts.en_GB-large
    imagemagick
    imv
    kdeconnect
    keepassxc
    libnotify
    libreoffice
    lsof
    lxqt.lxqt-policykit
    man-pages
    man-pages-posix
    mpc-cli
    mpv
    mpvScripts.mpris
    mupdf
    neovim
    nil
    nixfmt-rfc-style
    nodePackages.bash-language-server
    nodePackages.prettier
    obs-studio
    openssl
    ormolu
    p7zip
    pandoc
    poppler_utils
    pulsemixer
    # imhex
    # rlwrap
    rage
    ripgrep
    rsync
    shellcheck
    shfmt
    signal-desktop
    simplex-chat-desktop
    mpvScripts.mpris
    slurp
    smartmontools
    swaylock
    syncthing
    texliveFull
    traceroute
    trash-cli
    unzip
    virt-manager
    wlr-randr
    wl-clipboard
    wl-clip-persist
    cliphist
    wdisplays
    xdg-utils
    yt-dlp
    zip
    xboard
    (aspellWithDicts (
      dicts: with dicts; [
        en
        en-computers
        en-science
      ]
    ))
  ];

  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
