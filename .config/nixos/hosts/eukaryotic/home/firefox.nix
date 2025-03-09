{
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
              template = "https://searx.tiekoetter.com/search";
              params = [
                {
                  name = "q";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          iconUpdateURL = "https://searx.tiekoetter.com/static/themes/simple/img/favicon.png";
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
        #   /* hides the native tabs */
        #   #TabsToolbar {
        #     visibility: collapse;
        #   }

           /* hides that annoying extension button */
           #unified-extensions-button {
              display: none !important;
           }

          /* https://gist.github.com/chris-vecchio/d6a47fc733559752cc3a09937381d7ae */
          /* Firefox userChrome.css */

          /* Borders on tab scroll right and left buttons */
          #scrollbutton-up, #scrollbutton-down { /* 6/10/2021 */
            border-top-width: 1px !important;
            border-bottom-width: 0 !important;
          }

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
        "browser.tabs.inTitlebar" = 0;
        "sidebar.verticalTabs" = true;
        "browser.tabs.hoverPreview.enabled" = false;
        "privacy.resistFingerprinting.autoDeclineNoUserInputCanvasPrompts" = true;
        "browser.ml.chat.enabled" = false;
        "browser.ml.chat.provider" = "";
        "browser.ml.chat.shortcuts" = false;
        "browser.ml.chat.sidebar" = false;
        "browser.tabs.closeWindowWithLastTab" = false;
        "dom.security.https_only_mode" = true;
        "accessibility.force_disabled" = 1;
        "app.normandy.api_url" = "";
        "app.normandy.enabled" = false;
        "app.shield.optoutstudies.enabled" = false;
        "app.update.service.enabled" = false;
        "app.update.silent" = false;
        "app.update.staging.enabled" = false;
        "beacon.enabled" = false;
        "breakpad.reportURL" = "";
        "browser.bookmarks.max_backups" = 5;
        "browser.cache.disk.enable" = false; ## enable if too slow
        "browser.cache.memory.capacity" = 0;
        "browser.cache.memory.enable" = false; # enable if too slow
        "browser.cache.offline.enable" = false;
        "browser.compactmode.show" = true;
        "browser.contentanalysis.default_result" = 0;
        "browser.contentanalysis.enabled" = false;
        "browser.contentblocking.category" = "strict";
        "browser.contentblocking.report.lockwise.enabled" = false;
        "browser.contentblocking.report.monitor.enabled" = false;
        "browser.discovery.enabled" = false;
        "browser.display.use_document_fonts" = 0;
        "browser.download.always_ask_before_handling_new_types" = true;
        "browser.download.alwaysOpenPanel" = false;
        "browser.download.autohideButton" = true;
        "browser.download.folderList" = 1;
        "browser.download.forbid_open_with" = true;
        "browser.download.manager.addToRecentDocs" = false;
        "browser.download.useDownloadDir" = false;
        "browser.library.activity-stream.enabled" = false;
        "browser.link.open_newwindow" = 3;
        "browser.link.open_newwindow.override.external" = 3;
        "browser.link.open_newwindow.restriction" = 0;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.pagethumbnails.capturing_disabled" = true;
        "browser.ping-centre.telemetry" = false;
        "browser.places.speculativeConnect.enabled" = false;
        "browser.preferences.experimental" = false; # disables firefox labs
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
        "browser.search.isUS" = true;
        "browser.search.suggest.enabled" = false;
        "browser.search.widget.inNavBar" = false;
        "browser.sessionstore.cleanup.forget_closed_after" = 600;
        "browser.sessionstore.interval" = 9999999;
        "browser.sessionstore.interval.idle" = 9999999;
        "browser.sessionstore.max_tabs_undo" = 0;
        "browser.sessionstore.privacy_level" = 2; ## disable storing extra session data such as form content, cookies and POST data 0=everywhere, 1=unencrypted sites, 2=nowhere
        "browser.sessionstore.resume_from_crash" = false;
        "browser.shell.shortcutFavicons" = false;
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.startup.page" = "about:blank";
        "browser.tabs.allowTabDetach" = true;
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.loadDivertedInBackground" = true;
        "browser.tabs.loadInBackground" = true;
        "browser.translations.enable" = false;
        "browser.triple_click_selects_paragraph" = true;
        "browser.uidensity" = 1;
        "browser.uitour.enabled" = false;
        "browser.uitour.url" = "";
        "browser.urlbar.addons.featureGate" = false;
        "browser.urlbar.autoFill" = true;
        "browser.urlbar.formatting.enabled" = true;
        "browser.urlbar.mdn.featureGate" = false;
        "browser.urlbar.pocket.featureGate" = false;
        "browser.urlbar.speculativeConnect.enabled" = false;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.history" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.recentsearches" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.trimURLs" = false;
        "browser.urlbar.weather.featureGate" = false;
        "browser.urlbar.yelp.featureGate" = false;
        "captivedetect.canonicalURL" = false;
        "devtools.aboutdebugging.showSystemAddons" = true;
        "devtools.toolbox.zoomValue" = "=.2";
        "dom.allow_cut_copy" = false;
        "dom.battery.enabled" = false;
        "dom.event.clipboardevents.enabled" = false;
        "dom.push.connection.enabled" = false;
        "dom.push.userAge=tID" = "";
        "dom.security.https_only_mode_send_http_background_request" = false;
        "dom.serviceWorkers.enabled" = false;
        "dom.webnotifications.enabled" = false;
        "dom.webnotifications.serviceworker.enabled" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "extensions.getAddons.showPane" = false; #disable recommendation pane in about:addons (uses Google Analytics)
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
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
        "geo.provider.use_geoclue" = false;
        "geo.enabled" = false; #https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        "gfx.font_rendering.opentype_svg.enabled" = false;
        "identity.fxaccounts.enabled" = false;
        "javascript.options.asmjs" = true; # enable if too slow
        "javascript.options.baselinejit" = true; # enable if too slow
        "javascript.options.ion" = true; # enable if too slow
        "javascript.options.wasm" = true; # enable if too slow
        "keyword.enabled" = true; # false = no automatic search engine
        "layout.spellcheckDefault" = 0;
        "dom.private-attribution.submission.enabled" = false; # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
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
        "network.captive-portal-service.enabled" = false;
        "network.connectivity-service.enabled" = false;
        "network.cookie.cookieBehavior" = 2;
        "network.cookie.lifetimePolicy" = 2;
        "network.dns.disablePrefetch" = true;
        "network.dns.disablePrefetchFromHTTPS" = true;
        "network.dnsCacheEntries" = 0;
        "network.gio.supported-protocols" = "";
        "network.http.referer.XOriginTrimmingPolicy" = 2; #control the amount of cross-origin information to send. 0=send full URI (default), 1=scheme+host+port+path, 2=scheme+host+port
        "network.http.referer.XOriginPolicy" = 2; #https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        "network.IDN_show_punycode" = true;
        "network.manage-offline-status" = false;
        "network.predictor.enabled" = false;
        "network.prefetch-next" = false;
        "network.trr.mode" = 3;
        # "network.trr.uri" = "https://all.dns.mullvad.net/dns-query";
        "network.trr.uri" = "https://cloudflare-dns.com/dns-query"; #temp for current wifi situations
        "nglayout.enable_drag_images" = false;
        "pdfjs.enableScripting" = false;
        "permissions.default.camera" = 2;
        "permissions.default.desktop-notification" = 2;
        "permissions.default.geo" = 2;
        "permissions.default.microphone" = 2;
        "permissions.default.xr" = 2;
        "permissions.memory_only" = true;
        "places.history.enabled" = false;
        "privacy.antitracking.enableWebcompat" = false;
        "privacy.clearOnShutdown.cookies" = true;
        "privacy.clearOnShutdown.formdata" = true;
        "privacy.clearOnShutdown.history" = true;
        "privacy.clearOnShutdown_v2.cache" = true;
        "privacy.clearHistory.siteSettings" = true;
        "privacy.clearOnShutdown.siteSettings" = true;
        "privacy.clearOnShutdown_v2.siteSettings" = true;
        "privacy.clearSiteData.siteSettings" = true;
        "privacy.cpd.siteSettings" = true;
        "privacy.cpd.cookies" = true;
        "privacy.donottrackheader.enabled" = false;
        "privacy.resistFingerprinting" = true;
        "privacy.resistFingerprinting.letterboxing" = true;
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "privacy.spoof_english" = 1;
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
        "sidebar.animation.enabled" = false;
        "toolkit.cosmeticAnimations.enabled" = false;
        "webgl.disabled" = true;
      };
    };
    profiles."work" = {
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
      };
      search.force = true;
      search.default = "Startpage";
      userChrome = ''
        #   /* hides the native tabs */
        #   #TabsToolbar {
        #     visibility: collapse;
        #   }

           /* hides that annoying extension button */
           #unified-extensions-button {
              display: none !important;
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
        "network.trr.mode" = 3;
        "network.trr.uri" = "https://cloudflare-dns.com/dns-query";
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
        "sidebar.verticalTabs" = true;
        "browser.tabs.hoverPreview.enabled" = false;
        "browser.tabs.inTitlebar" = 0;
      };
    };
  };
}
