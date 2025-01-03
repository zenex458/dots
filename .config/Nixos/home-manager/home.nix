{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
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
    theme.package = pkgs.shades-of-gray-theme;
    theme.name = "Shades-of-gray";
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper-Mono-Dark";
    cursorTheme.name = "plan9";
    cursorTheme.size = 20;
    # font.package = pkgs.iosevka;
    # font.name = "Iosevka";
    font.package = pkgs.uw-ttyp0;
    font.name = "Ttyp0";
    font.size = 10;
  };
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium.override {enableWideVine = true;};
  };
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
    profiles."test" = {
      id = 2;
      search.engines = {
        "Bing".metaData.hidden = true;
        "Google".metaData.hidden = true;
        "Amazon.co.uk".metaData.hidden = true;
        "eBay".metaData.hidden = true;
      };
      search.force = true;
    };
    profiles.priv = {
      id = 1;
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
          definedAliases = ["@st"];
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
          iconUpdateURL = "https://priv.au/static/themes/simple/img/favicon.png?60321eeb6e2f478f0e5704529308c594d5924246";
          definedAliases = ["@pv"];
        };
        "NixosPackage" = {
          urls = [
            {
              template = "https://search.nixos.org/packages?channel=24.11&from=0&size=50&sort=relevance&type=packages";
              params = [
                {
                  name = "query";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconUpdateURL = "https://search.nixos.org/favicon.png";
          definedAliases = ["@np"];
        };
        "NixosOption" = {
          urls = [
            {
              template = "https://search.nixos.org/options?channel=24.11&from=0&size=50&sort=relevance&type=packages";
              params = [
                {
                  name = "query";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconUpdateURL = "https://search.nixos.org/favicon.png";
          definedAliases = ["@no"];
        };

        "NixosWiki" = {
          urls = [{template = "https://wiki.nixos.org/w/index.php?search={searchTerms}";}];
          iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
          definedAliases = ["@nw"];
        };

        "HomemanagerSearch" = {
          urls = [
            {
              template = "https://home-manager-options.extranix.com/?query={searchTerms}&release=release-24.11";
            }
          ];
          iconUpdateURL = "https://home-manager-options.extranix.com/images/favicon.png";
          definedAliases = ["@hs"];
        };
      };
      search.force = true;
      search.default = "Searx";
      search.order = [
        "Searx"
        "DuckDuckgo"
        "NixosPackage"
        "NixosOption"
        "HomemanagerSearch"
        "NixosWiki"
      ];
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
        "app.normandy.api_url" = "";
        "app.normandy.enabled" = false;
        "app.shield.optoutstudies.enabled" = false;
        "app.update.service.enabled" = false;
        "app.update.silent" = false;
        "app.update.staging.enabled" = false;
        "beacon.enabled" = false;
        "browser.bookmarks.max_backups" = 5;
        "browser.cache.memory.capacity" = -1;
        "browser.cache.memory.enable" = true;
        "browser.cache.offline.enable" = false;
        "browser.compactmode.show" = true;
        "browser.contentblocking.report.lockwise.enabled" = false;
        "browser.contentblocking.report.monitor.enabled" = false;
        "browser.display.use_document_fonts" = 0;
        "browser.download.autohideButton" = true;
        "browser.download.folderList" = 1;
        "browser.download.forbid_open_with" = false;
        "browser.library.activity-stream.enabled" = false;
        "browser.link.open_newwindow" = 3;
        "browser.link.open_newwindow.override.external" = 3;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.pagethumbnails.capturing_disabled" = true;
        "browser.ping-centre.telemetry" = false;
        "browser.preferences.moreFromMozilla" = false;
        "browser.region.networ=.url" = "";
        "browser.region.update.enabled" = false;
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
        "browser.safebrowsing.provider.google4.dataSharingURL" = "";
        "browser.safebrowsing.provider.google4.getha=hURL" = "";
        "browser.safebrowsing.provider.google4.repo=tURL" = "";
        "browser.safebrowsing.provider.google4.upda=eURL" = "";
        "browser.safebrowsing.reportPhi=hURL" = "";
        "browser.search.widget.inNavBar" = false;
        "browser.sessionstore.cleanup.forget_closed_after" = 600;
        "browser.sessionstore.interval" = 9999999;
        "browser.sessionstore.interval.idle" = 9999999;
        "browser.sessionstore.max_tabs_undo" = 0;
        "browser.sessionstore.resume_from_crash" = false;
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.startup.page" = 0;
        "browser.tabs.allowTabDetach" = true;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.loadDivertedInBackground" = true;
        "browser.tabs.loadInBackground" = true;
        "browser.translations.enable" = false;
        "browser.triple_click_selects_paragraph" = true;
        "browser.uidensity" = 1;
        "browser.urlbar.autoFill" = true;
        "browser.urlbar.formatting.enabled" = true;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.history" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.recentsearches" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.trimURLs" = false;
        "devtools.aboutdebugging.showSystemAddons" = true;
        "devtools.toolbox.zoomValue" = "=.2";
        "dom.allow_cut_copy" = false;
        "dom.battery.enabled" = false;
        "dom.event.clipboardevents.enabled" = false;
        "dom.push.connection.enabled" = false;
        "dom.push.userAge=tID" = "";
        "dom.security.https_only_mode_send_http_background_request" = false;
        "dom.serviceWorkers.enabled" = false;
        "dom.webnotifications.enabled" = true; # SET FALSE
        "dom.webnotifications.serviceworker.enabled" = true; # SET FALSE FOR PRIV
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "extensions.pocket.enabled" = false;
        "extensions.quarantinedDomain=.list" = "";
        "extensions.quarantinedDomains.enabled" = true;
        "extensions.screenshots.disabled" = true;
        "extensions.webextensions.restrict=dDomains" = "";
        "extensions.webextensions.userScripts.enabled" = true;
        "findbar.highlightAll" = true;
        "full-screen-api.warning.delay" = 0;
        "full-screen-api.warning.timeout" = 0;
        "general.autoScroll" = false;
        "general.smoothScroll" = false;
        "gfx.font_rendering.opentype_svg.enabled" = false;
        "identity.fxaccounts.enabled" = false;
        "javascript.options.asmjs" = false;
        "javascript.options.baselinejit" = false;
        "javascript.options.ion" = false;
        "javascript.options.wasm" = false;
        "keyword.enabled" = true; # false = no automatic search engine
        "layout.spellcheckDefault" = 0;
        "mathml.disabled" = true;
        "media.autoplay.default" = 5;
        "media.gmp-widevinecdm.enabled" = false;
        "media.navigator.enabled" = false;
        "media.peerconnection.enabled" = false;
        "media.peerconnection.ice.no_host" = true;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "middlemouse.paste" = true;
        "mousewheel.with_shift.action" = 3;
        "network.cookie.cookieBehavior" = 2;
        "network.cookie.lifetimePolicy" = 2;
        "network.dnsCacheEntries" = 0;
        "network.manage-offline-status" = false;
        "network.trr.mode" = 3;
        "network.trr.uri" = "https://all.dns.mullvad.net/dns-query";
        "nglayout.enable_drag_images" = false;
        "permissions.default.camera" = 2;
        "permissions.default.desktop-notification" = 1;
        "permissions.default.geo" = 2;
        "permissions.default.microphone" = 2;
        "permissions.default.xr" = 2;
        "permissions.memory_only" = true;
        "places.history.enabled" = false;
        "privacy.antitracking.enableWebcompat" = false;
        "privacy.clearOnShutdown.cookies" = true;
        "privacy.clearOnShutdown.formdata" = true;
        "privacy.clearOnShutdown.history" = true;
        "privacy.cpd.cookies" = true;
        "privacy.donottrackheader.enabled" = false;
        "privacy.firstparty.isolate" = true;
        "privacy.partition.always_partition_third_party_non_cookie_storage.exempt_sessionstorage" = true;
        "privacy.resistFingerprinting" = true;
        "privacy.resistFingerprinting.letterboxing" = true;
        "privacy.trackingprotection.cryptomining.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.pbmode.enabled" = true;
        "privacy.userContext.enabled" = true;
        "privacy.userContext.ui.enabled" = true;
        "reader.parse-on-load.enabled" = false;
        "security.ask_for_password" = 0;
        "security.cert_pinning.enforcement_level" = 2;
        "security.insecure_connection_text.enabled" = true;
        "security.mixed_content.block_display_content" = true;
        "security.osclientcerts.autoload" = true;
        "security.pki.sha1_enforcement_level" = 1;
        "security.ssl.require_safe_negotiation" = true;
        "security.tls.version.min" = 1;
        "signon.autofillForms" = false;
        "signon.formlessCapture.enabled" = false;
        "signon.generation.enabled" = false;
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
          iconUpdateURL = "https://www.startpage.com/sp/cdn/favicons/favicon-32x32-gradient.png";
          definedAliases = ["@st"];
        };
      };
      search.force = true;
      search.default = "Startpage";
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
        "browser.startup.page" = "https://priv.au";
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
        "reader.parse-on-load.enabled" = false;
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
      };
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      input = {
        kb_layout = "gb";
        kb_options = "altwin:ctrl_alt_win,caps:shift_nocancel,caps:backspace";
        repeat_rate = 25;
        repeat_delay = 600;
        follow_mouse = 1;
        sensitivity = 0;
        touchpad.natural_scroll = "no";
      };
      general = {
        gaps_in = 0;
        gaps_out = 0;
        border_size = 1;
        "col.active_border" = "rgba(bdae93ff)";
        "col.inactive_border" = "rgba(000000aa)";
        layout = "master";
        allow_tearing = false;
      };
      cursor = {
        hide_on_key_press = false;
        inactive_timeout = 20;
      };
      decoration = {
        rounding = 0;
        blur = {
          enabled = false;
          size = 3;
          passes = 1;
        };
      };
      animations = {
        enabled = "no";
      };
      gestures = {
        workspace_swipe = "off";
      };
      misc = {
        force_default_wallpaper = 0;
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };
      bind = [
        "$mainMod, Return, exec, footclient tmux"
        "$mainMod, A, exec, vol.sh"
        "$mainMod SHIFT, Q, killactive,"
        "$mainMod SHIFT CTRL, Z, exit,"
        "$mainMod, V, togglefloating"
        "$mainMod, C, exec, firejail firefox -P priv"
        "$mainMod SHIFT, C, exec, firejail firefox"
        "$mainMod, U, exec, emacsclient -c -a emacs"
        "$mainMod, P, exec, bemenu-run"
        "$mainMod SHIFT, O, exec, mpc next"
        "$mainMod SHIFT, I, exec, mpc prev"
        "$mainMod SHIFT, P, exec, mpc toggle"
        "$mainMod SHIFT, Prior, exec, light -A 2"
        "$mainMod SHIFT, Next, exec, light -U 2"
        "$mainMod SHIFT, Home, exec, light.sh"
        "$mainMod, M, exec, Menu"
        "ALT, Tab, exec, show.sh"
        "$mainMod, Y, exec, clipshow.sh"
        "$mainMod, F, fullscreen, 0"
        "$mainMod, W, focusmonitor, HDMI-A-1"
        "$mainMod, E, focusmonitor, eDP-1"
        "$mainMod, R, focusmonitor, HDMI-A-2"
        "$mainMod, h, movefocus, l"
        "$mainMod, l, movefocus, r"
        "$mainMod, j, movefocus, u"
        "$mainMod, k, movefocus, d"
        "SUPER, 1, focusworkspaceoncurrentmonitor, 1"
        "SUPER, 2, focusworkspaceoncurrentmonitor, 2"
        "SUPER, 3, focusworkspaceoncurrentmonitor, 3"
        "SUPER, 4, focusworkspaceoncurrentmonitor, 4"
        "SUPER, 5, focusworkspaceoncurrentmonitor, 5"
        "SUPER, 6, focusworkspaceoncurrentmonitor, 6"
        "SUPER, 7, focusworkspaceoncurrentmonitor, 7"
        "SUPER, 8, focusworkspaceoncurrentmonitor, 8"
        "SUPER, 9, focusworkspaceoncurrentmonitor, 9"
        "$mainMod SHIFT, 1, movetoworkspacesilent, 1"
        "$mainMod SHIFT, 2, movetoworkspacesilent, 2"
        "$mainMod SHIFT, 3, movetoworkspacesilent, 3"
        "$mainMod SHIFT, 4, movetoworkspacesilent, 4"
        "$mainMod SHIFT, 5, movetoworkspacesilent, 5"
        "$mainMod SHIFT, 6, movetoworkspacesilent, 6"
        "$mainMod SHIFT, 7, movetoworkspacesilent, 7"
        "$mainMod SHIFT, 8, movetoworkspacesilent, 8"
        "$mainMod SHIFT, 9, movetoworkspacesilent, 9"
        "$mainMod SHIFT, 0, movetoworkspacesilent, 10"
      ];
      bindm = [
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];
      "$mainMod" = "SUPER";
    };
    extraConfig = ''
      monitor=HDMI-A-1,1280x1024,0x0, 1
      monitor=eDP-1,1920x1080,1280x0, 1
      monitor=HDMI-A-2,1280x1024,3200x0, 1

      exec-once = dunst
      exec-once = lxqt-policykit-agent
      exec-once = hyprpaper
      exec-once = hypridle
      exec-once = ~/.local/bin/batt.sh
      exec-once = ~/.local/bin/dark.sh
      exec-once = wlsunset -S 07:00 -s 20:00 -T 4800 -t 2600
      exec-once = wl-paste --watch cliphist store
      exec-once = cliphist wipe
      exec-once = hyprctl setcursor plan9 20
      exec-once = dconf write /org/gnome/desktop/interface/cursor-theme "plan9"
      exec-once = gsettings set org.gnome.desktop.interface cursor-theme 'plan9'

      env = HYPRCURSOR_THEME,plan9
      env = HYPRCURSOR_SIZE,20
      env = XCURSOR_THEME,plan9
      env = XCURSOR_SIZE,20

      windowrulev2 = workspace 1,class:^(firefox)$,
      windowrulev2 = workspace 2,class:^(emacs)$,
      windowrulev2 = workspace 8,fullscreen,class:^(mpv)$,
    '';
  };
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = false;
      splash = false;
      preload = ["~/Downloads/Images/realsat.jpg"];
      wallpaper = [",~/Downloads/Images/realsat.jpg"];
    };
  };

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        after_sleep_cmd = "hyprctl dispatch dpms on";
        before_sleep_cmd = "hyprlock";
        ignore_dbus_inhibit = false;
        ignore_systemd_inhibit = false;
        lock_cmd = "hypridle";
      };
      listener = [
        {
          timeout = 900;
          on-timeout = "hypridle";
        }
        {
          timeout = 1200;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
      ];
    };
  };

  programs.hyprlock = {
    enable = true;
    settings = {
      general = [
        {
          monitor = "";
          ignore_empty_input = true;
          hide_cursor = true;
          no_fade_in = true;
          no_fade_out = true;
        }
      ];
      background = [{color = "rgb(0, 0, 0)";}];
      input-field = [
        {
          size = "300, 50";
          position = "0, -80";
          halign = "center";
          valign = "center";
          monitor = "";
          dots_center = true;
          fade_on_empty = true;
          font_color = "rgb(202, 211, 245)";
          # inner_color = "rgb(91, 96, 120)";
          inner_color = "rgb(0, 0, 0)";
          # outer_color = "rgb(24, 25, 38)";
          outer_color = "rgb(0, 0, 0)";
          outline_thickness = 3;
          placeholder_text = "";
          shadow_passes = 0;
          rounding = 0;
        }
      ];
    };
  };

  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.dotnet/tools"
  ];
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.zsh = {
    enable = false;
    enableCompletion = true;
    historySubstringSearch.enable = true;
    defaultKeymap = "emacs";
    autosuggestion = {
      enable = true;
      highlight = "fg=#c6c6c6,bg=black,bold,underline";
    };
    syntaxHighlighting = {
      enable = true;
      styles = {
        suffix-alias = "fg=#c6c6c6";
        precommand = "fg=#c6c6c6";
        arg0 = "fg=#c6c6c6";
        alias = "fg=#c6c6c6";
        path = "fg=#c6c6c6";
        unknown-token = "fg=#c6c6c6,underline";
        command_error = "fg=#c6c6c6,underline";
      };
    };
    initExtra = ''
      PROMPT="[%~]''\nλ "
      zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate _aliases _functions
      zstyle ':completion:*' use-cache on
      zstyle ':completion:*' cache-path "$HOME/.cache/.zcompcache"
      zstyle ':completion:*' group-name ' '
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
      zstyle ':completion:*' verbose true
      zstyle ':completion:*' menu select search
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
      listnixgen = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
      remoldgen = "nix-collect-garbage --delete-older-than 2d && sudo nix-collect-garbage --delete-older-than 2d && upd";
      re = "systemctl reboot";
      off = "systemctl poweroff";
      nv = "nvim";
      ls = "ls -F -h --color=always --group-directories-first";
      ga = "git add";
      gc = "git commit -m";
      updoff = "upd && sleep 2 && off";
      updr = "upd && sleep 2 && re";
      grep = "grep -i --colour=always";
      mkdir = "mkdir -pv";
      mv = "mv -iv";
      cp = "cp -iva";
      rm = "rm -iv";
      ll = "ls -lA";
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
      msd = "sudo mount -m -v -o rw,noexec,uid=1000,gid=1000 UUID=04C3-E2B3 /run/media/zenex/musicsd";
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
      kremap = "setxkbmap -option altwin:ctrl_alt_win";
      krremap = "setxkbmap -option -layout gb";
      chkfstab = "sudo findmnt --verify";
      logs = "journalctl -S today -o verbose -r -x";
      log = "journalctl -S today -r -x";
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
    musicDirectory = "/home/zenex/music";
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
    mpdMusicDir = "/home/zenex/music";
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
    options = {
      sandbox = "strict";
      database = "sqlite";
    };
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

  home.packages = with pkgs; [
    # aria2
    #  bsdgames
    #  cljfmt
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
    # rustup
    # age
    # sigrok-cli
    # gron # json grepper
    # fq # jq for binary formats
    # entr # run a command when files change
    # https://github.com/ducaale/xh # httpie replacement
    # https://viric.name/soft/ts/
    # https://www.gnu.org/software/parallel
    alacritty
    alsa-utils
    anki-bin
    astyle
    bc
    bemenu
    cargo
    ccls
    cliphist
    dmenu
    exfatprogs
    fd
    ffmpeg
    file
    fuse3
    gcc
    gdb
    gh
    ghc
    gimp
    git
    glib
    gnumake
    gojq
    grim
    haskell-language-server
    htop
    hunspell
    hunspellDicts.en-gb-large
    # hyprshade
    # hyprsunset
    wlsunset
    imagemagick
    imv
    kdePackages.kdeconnect-kde
    keepassxc
    libnotify
    libreoffice
    lsof
    lxqt.lxqt-policykit
    magic-wormhole
    man-pages
    man-pages-posix
    mpc-cli
    mpv
    mpvScripts.mpris
    mupdf
    nemo
    neovim
    nixd
    # nixfmt-rfc-style
    alejandra
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
    ripgrep-all
    rsync
    ruff
    ruff-lsp
    sbcl
    sdcv
    shellcheck
    shfmt
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
    xmlformat
    virt-manager
    wdisplays
    wl-clip-persist
    wl-clipboard
    wl-color-picker
    wlr-randr
    xdg-utils
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

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
