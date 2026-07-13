{ lib, config, ... }: {
  programs.firefox = {
    enable = true;
    configPath = "${config.xdg.configHome}/mozilla/firefox";
    policies = {
      languagePacks = [ "en-GB" ];
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
      PDFjs = false;
      GenerativeAI = {
        Chatbot = false;
        LinkPreviews = false;
        TabGroups = false;
      };

      ExtensionSettings =
        let
          moz = short: "https://addons.mozilla.org/firefox/downloads/latest/${short}/latest.xpi";
        in
        {
          "{9b84b6b4-07c4-4b4b-ba21-394d86f6e9ee}" = {
            install_url = moz "black21";
            installation_mode = "normal_installed";
            # updates_disabled = true;
            private_browsing = true;
          };

          "{d7742d87-e61d-4b78-b8a1-b469842139fa}" = {
            install_url = moz "vimium-ff";
            installation_mode = "normal_installed";
            # updates_disabled = true;
            private_browsing = true;
          };

          "addon@darkreader.org" = {
            install_url = moz "darkreader";
            installation_mode = "normal_installed";
            # updates_disabled = true;
            private_browsing = true;
          };
          "uBlock0@raymondhill.net" = {
            install_url = moz "ublock-origin";
            installation_mode = "normal_installed";
            # updates_disabled = true;
            private_browsing = true;
          };
        };

      "3rdparty".Extensions = {
        "uBlock0@raymondhill.net".adminSettings = {
          userSettings = rec {
            uiTheme = "dark";
            uiAccentCustom = true;
            advancedUserEnabled = true;
            uiAccentCustom0 = "#bdae93";
            cloudStorageEnabled = lib.mkForce false;
            popupPanelSections = 31;
            importedLists = [
              "https://filters.adtidy.org/extension/ublock/filters/3.txt"
              "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/LegitimateURLShortener.txt"
              "https://raw.githubusercontent.com/celenityy/BadBlock/pages/abp/badblock_plus.txt"
              "https://raw.githubusercontent.com/celenityy/BadBlock/pages/hardened/block-page-visibility.txt"
            ];

            externalLists = lib.concatStringsSep "\n" importedLists;
          };

          selectedFilterLists = [
            "user-filters"
            "ublock-filters"
            "ublock-badware"
            "ublock-privacy"
            "ublock-quick-fixes"
            "ublock-unbreak"
            "easylist"
            "adguard-generic"
            "easyprivacy"
            "adguard-spyware-url"
            "block-lan"
            "urlhaus-1"
            "curben-phishing"
            "plowe-0"
            "dpollock-0"
            "fanboy-cookiemonster"
            "ublock-cookies-easylist"
            "fanboy-social"
            "fanboy-thirdparty_social"
            "fanboy-ai-suggestions"
            "easylist-chat"
            "easylist-newsletters"
            "easylist-notifications"
            "easylist-annoyances"
            "adguard-mobile-app-banners"
            "adguard-other-annoyances"
            "adguard-popup-overlays"
            "adguard-widgets"
            "ublock-annoyances"
            "https://filters.adtidy.org/extension/ublock/filters/3.txt"
            "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/LegitimateURLShortener.txt"
            "https://raw.githubusercontent.com/celenityy/BadBlock/pages/abp/badblock_plus.txt"
            "https://raw.githubusercontent.com/celenityy/BadBlock/pages/hardened/block-page-visibility.txt"
          ];
        };
      };
    };
    profiles."test" = {
      id = 2;
      search.engines = {
        "bing".metaData.hidden = true;
        "google".metaData.hidden = true;
        "amazon.co.uk".metaData.hidden = true;
        "ebay".metaData.hidden = true;
      };
      search.force = true;
    };
    profiles.personal = {
      search.engines = {
        "bing".metaData.hidden = true;
        "google".metaData.hidden = true;
        "amazon.co.uk".metaData.hidden = true;
        "amazondotcom-us".metaData.hidden = true;
        "ddg".metaData.hidden = true;
        "perplexity".metaData.hidden = true;
        "ebay.co.uk".metaData.hidden = true;
        "Duckduckgohtml" = {
          name = "Duckduckgohtml";
          urls = [ { template = "https://html.duckduckgo.com/html?q={searchTerms}"; } ];
          iconMapObj."16" = "https://duckduckgo.com/favicon.ico";
          definedAliases = [ "@ddgh" ];
        };
        "noaiduckduckgo" = {
          name = "noaiduckduckgo";
          urls = [ { template = "https://noai.duckduckgo.com/?q={searchTerms}&noai=1"; } ];
          iconMapObj."16" = "https://noai.duckduckgo.com/favicon.ico";
          definedAliases = [ "@ddg" ];
        };
        "esearx" = {
          urls = [
            {
              template = "https://searx.tiekoetter.com/search";
              params = [
                {
                  name = "q";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconMapObj."16" = "https://searx.tiekoetter.com/static/themes/simple/img/favicon.png";
          updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@es" ];
        };
        "searx" = {
          urls = [
            {
              template = "https://searx.local/search";
              params = [
                {
                  name = "q";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconMapObj."16" = "https://searx.local/static/themes/simple/img/favicon.png";
          updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@s" ];
        };
        "NixosPackage" = {
          urls = [
            {
              template = "https://search.nixos.org/packages?channel=26.05&from=0&size=50&sort=relevance&type=packages";
              params = [
                {
                  name = "query";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          # iconUpdateURL = "https://search.nixos.org/favicon.png";
          # icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
          iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
          # updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@np" ];
        };
        "NixosOption" = {
          urls = [
            {
              template = "https://search.nixos.org/options?channel=26.05&from=0&size=50&sort=relevance&type=packages";
              params = [
                {
                  name = "query";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          # iconUpdateURL = "https://search.nixos.org/favicon.png";
          iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
          # updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@no" ];
        };

        "NixosWiki" = {
          urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
          # iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
          iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
          # updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@nw" ];
        };

        "HomemanagerSearch" = {
          urls = [
            {
              template = "https://home-manager-options.extranix.com/?query={searchTerms}&release=release-26.05";
            }
          ];
          # iconUpdateURL = "https://home-manager-options.extranix.com/images/favicon.png";
          iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
          # updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@hs" ];
        };
      };
      search.force = true;
      search.default = "Duckduckgohtml";
      search.order = [
        "searx"
        "esearx"
        "Duckduckgohtml"
        "noaiduckduckgo"
        "NixosPackage"
        "NixosOption"
        "HomemanagerSearch"
        "NixosWiki"
      ];
      userChrome = ''
        #TabsToolbar { visibility: collapse !important; }
        #titlebar { visibility: collapse; }

        /*
        #navigator-toolbox:hover #TabsToolbar {
            visibility: visible;
         }
        */
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

        /* https://gist.github.com/chris-vecchio/d6a47fc733559752cc3a09937381d7ae */
        /* Firefox userChrome.css */

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
        "brower.cache.disk.enable" = false;
        "browser.cache.memory.capacity" = -1;
        "browser.cache.memory.enable" = true;
        "browser.casting.enabled" = false;
        "browser.compactmode.show" = true;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
        "browser.discovery.enabled" = false;
        "browser.download.always_ask_before_handling_new_types" = true;
        "browser.download.useDownloadDir" = false;
        "browser.download.viewableInternally.enabledTypes" = "";
        "browser.formfill.enable" = false;
        "browser.link.open_newwindow" = 3;
        "browser.ml.chat.enabled" = false;
        "browser.ml.chat.menu" = false;
        "browser.ml.chat.page" = false;
        "browser.ml.chat.provider" = "";
        "browser.ml.chat.shortcuts" = false;
        "browser.ml.chat.sidebar" = false;
        "browser.ml.enable" = false;
        "browser.ml.linkPreview.enabled" = false;
        "browser.ml.linkPreview.optin" = false;
        "browser.ml.modelHubRootUrl" = "";
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.newtabpage.activity-stream.default.sites" = "";
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.newtabpage.enabled" = false;
        "browser.ping-centre.telemetry" = false;
        "browser.preferences.experimental" = false; # disables firefox labs
        "browser.preferences.moreFromMozilla" = false;
        "browser.search.isUS" = false;
        "browser.search.region" = "GB";
        "browser.search.suggest.enabled" = false;
        "browser.shell.shortcutFavicons" = true;
        "browser.shopping.experience2023.enabled" = false;
        "browser.startup.blankWindow" = true;
        "browser.startup.homepage" = "about:blank";
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.startup.page" = "about:blank";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.firefox-view-newIcon" = false;
        "browser.tabs.firefox-view-next" = false;
        "browser.tabs.hoverPreview.enabled" = false;
        "browser.tabs.inTitlebar" = 0;
        "browser.translations.enable" = true;
        "browser.uidensity" = 1;
        "browser.urlbar.quicksuggest.enabled" = false;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.history" = false;
        "browser.urlbar.suggest.openpage" = true;
        "browser.urlbar.suggest.quickactions" = false;
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
        "places.history.enabled" = false;
        "privacy.clearHistory.siteSettings" = true;
        "privacy.clearOnShutdown.cache" = true;
        "privacy.clearOnShutdown.cookies" = true;
        "privacy.clearOnShutdown.downloads" = true;
        "privacy.clearOnShutdown.formdata" = true;
        "privacy.clearOnShutdown.history" = true;
        "privacy.clearOnShutdown.offlineApps" = true;
        "privacy.clearOnShutdown.sessions" = true;
        "privacy.clearOnShutdown.siteSettings" = true;
        "privacy.clearOnShutdown_v2.formdata" = true;
        "privacy.clearOnShutdown_v2.siteSettings" = true;
        "privacy.clearSiteData.siteSettings" = true;
        "privacy.cpd.history" = true;
        "privacy.cpd.siteSettings" = true;
        "privacy.history.custom" = true;
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "reader.parse-on-load.enabled" = false;
        "sidebar.backupState" =
          "{\"panelOpen\":false,\"launcherWidth\":50,\"launcherExpanded\":false,\"launcherVisible\":true}";
        "sidebar.animation.duration-ms" = 200;
        "sidebar.animation.enabled" = false;
        "sidebar.animation.expand-on-hover.duration-ms" = 400;
        "sidebar.expandOnHover" = false;
        "sidebar.main.tools" = "history,bookmarks";
        "sidebar.new-sidebar.has-used" = true;
        "sidebar.old-sidebar.has-used" = false;
        "sidebar.position_start" = true;
        "sidebar.revamp" = true;
        "sidebar.revamp.defaultLauncherVisible" = true;
        "sidebar.revamp.round-content-area" = false;
        "sidebar.verticalTabs" = true;
        "sidebar.visibility" = "always-show";
        "signon.autofillForms" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "signon.rememberSignons" = false;
        "toolkit.cosmeticAnimations.enabled" = false;
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
    profiles."work" = {
      id = 1;
      search.engines = {
        "bing".metaData.hidden = true;
        "google".metaData.hidden = true;
        "perplexity".metaData.hidden = true;
        "amazon.co.uk".metaData.hidden = true;
        "amazondotcom-us".metaData.hidden = true;
        "ddg".metaData.hidden = true;
        "ebay".metaData.hidden = true;
        "searx" = {
          urls = [
            {
              template = "https://searx.local/search";
              params = [
                {
                  name = "q";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconMapObj."16" = "https://searx.local/static/themes/simple/img/favicon.png";
          updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = [ "@s" ];
        };
        "noaiduckduckgo" = {
          name = "noaiduckduckgo";
          urls = [ { template = "https://noai.duckduckgo.com/?q={searchTerms}&noai=1"; } ];
          iconMapObj."16" = "https://noai.duckduckgo.com/favicon.ico";
          definedAliases = [ "@ddg" ];
        };
      };
      search.force = true;
      search.default = "noaiduckduckgo";
      # search.default = "searx";
      settings = {
        # "network.trr.mode" = 3;
        # "network.trr.uri" = "https://all.dns.mullvad.net/dns-query";
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.startup.homepage" = "about:blank";
        "accessibility.force_disabled" = 1;
        "app.normandy.api_url" = "";
        "app.normandy.enabled" = false;
        "app.shield.optoutstudies.enabled" = false;
        "beacon.enabled" = false;
        "breakpad.reportURL" = "";
        "browser.cache.memory.capacity" = -1;
        "browser.cache.memory.enable" = true;
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
        "browser.casting.enabled" = false;
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
        "browser.ml.chat.enabled" = false;
        "browser.ml.chat.provider" = "";
        "browser.ml.chat.shortcuts" = false;
        "browser.ml.chat.sidebar" = false;
        "browser.ml.enable" = false;
        "browser.ml.chat.menu" = false;
        "browser.ml.chat.page" = false;
        "browser.ml.linkPreview.enabled" = false;
        "browser.ml.linkPreview.optin" = false;
        "browser.ml.modelHubRootUrl" = "";
        "browser.tabs.hoverPreview.enabled" = false;
        "browser.tabs.inTitlebar" = 0;
        "sidebar.animation.duration-ms" = 200;
        "sidebar.animation.enabled" = false;
        "sidebar.animation.expand-on-hover.duration-ms" = 400;
        "sidebar.backupState" =
          "{\"panelOpen\":false,\"launcherWidth\":50,\"launcherExpanded\":false,\"launcherVisible\":true}";
        "sidebar.expandOnHover" = false;
        "sidebar.main.tools" = "history,bookmarks";
        "sidebar.new-sidebar.has-used" = true;
        "sidebar.old-sidebar.has-used" = false;
        "sidebar.position_start" = true;
        "sidebar.revamp" = true;
        "sidebar.revamp.defaultLauncherVisible" = true;
        "sidebar.revamp.round-content-area" = false;
        "sidebar.verticalTabs" = true;
        "sidebar.visibility" = "always-show";
        "brower.cache.disk.enable" = false;
        "browser.urlbar.quicksuggest.enabled" = false;
        "browser.urlbar.suggest.quickactions" = false;
      };
    };
  };
}
