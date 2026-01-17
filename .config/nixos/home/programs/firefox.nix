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
      NoDefaultBookmarks = true;
      PDFjs = false;
      GenerativeAI = {
        Chatbot = false;
        LinkPreviews = false;
        TabGroups = false;
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
    profiles.priv = {
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
          urls = [{template = "https://html.duckduckgo.com/html?q={searchTerms}";}];
          iconMapObj."16" = "https://duckduckgo.com/favicon.ico";
          definedAliases = ["@ddgh"];
        };
        "noaiduckduckgo" = {
          name = "noaiduckduckgo";
          urls = [{template = "https://noai.duckduckgo.com/?q={searchTerms}&noai=1";}];
          iconMapObj."16" = "https://noai.duckduckgo.com/favicon.ico";
          definedAliases = ["@ddg"];
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
          definedAliases = ["@es"];
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
          definedAliases = ["@s"];
        };
        "NixosPackage" = {
          urls = [
            {
              template = "https://search.nixos.org/packages?channel=25.11&from=0&size=50&sort=relevance&type=packages";
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
          definedAliases = ["@np"];
        };
        "NixosOption" = {
          urls = [
            {
              template = "https://search.nixos.org/options?channel=25.11&from=0&size=50&sort=relevance&type=packages";
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
          definedAliases = ["@no"];
        };

        "NixosWiki" = {
          urls = [{template = "https://wiki.nixos.org/w/index.php?search={searchTerms}";}];
          # iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
          iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
          # updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = ["@nw"];
        };

        "HomemanagerSearch" = {
          urls = [
            {
              template = "https://home-manager-options.extranix.com/?query={searchTerms}&release=release-25.11";
            }
          ];
          # iconUpdateURL = "https://home-manager-options.extranix.com/images/favicon.png";
          iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
          # updateInterval = 24 * 60 * 60 * 1000; # every day
          definedAliases = ["@hs"];
        };
      };
      search.force = true;
      search.default = "searx";
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
        "browser.tabs.hoverPreview.enabled" = false;
        "privacy.resistFingerprinting.autoDeclineNoUserInputCanvasPrompts" = true;
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
        "browser.tabs.closeWindowWithLastTab" = false;
        "dom.security.https_only_mode" = true;
        "accessibility.force_disabled" = 1;
        "app.update.service.enabled" = false;
        "app.update.silent" = false;
        "app.update.staging.enabled" = false;
        "browser.cache.memory.capacity" = 0;
        "browser.cache.memory.enable" = false; # enable if too slow
        "browser.compactmode.show" = true;
        "browser.contentanalysis.default_result" = 0;
        "browser.contentanalysis.enabled" = false;
        "browser.contentblocking.category" = "strict";
        "browser.contentblocking.report.lockwise.enabled" = false;
        "browser.contentblocking.report.monitor.enabled" = false;
        "browser.download.always_ask_before_handling_new_types" = true;
        "browser.download.alwaysOpenPanel" = false;
        "browser.download.autohideButton" = true;
        "browser.download.forbid_open_with" = true;
        "browser.download.manager.addToRecentDocs" = false;
        "browser.library.activity-stream.enabled" = false;
        "browser.link.open_newwindow" = 3;
        "browser.link.open_newwindow.override.external" = 3;
        "browser.link.open_newwindow.restriction" = 0;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.ping-centre.telemetry" = false;
        "browser.places.speculativeConnect.enabled" = false;
        "browser.preferences.experimental" = false; # disables firefox labs
        "browser.preferences.moreFromMozilla" = false;
        "browser.region.networ=.url" = "";
        "browser.region.update.enabled" = false;
        "browser.safebrowsing.allowOverride" = false;
        "browser.safebrowsing.downloads.enabled" = false;
        "browser.safebrowsing.downloads.remote.block_dangerous" = false;
        "browser.safebrowsing.downloads.remote.block_dangerous_host" = false;
        "browser.safebrowsing.downloads.remote.block_potentially_unwanted" = false;
        "browser.safebrowsing.downloads.remote.block_uncommon" = false;
        "browser.safebrowsing.provider.google.getha=hURL" = "";
        "browser.safebrowsing.provider.google.repo=tURL" = "";
        "browser.safebrowsing.provider.google.upda=eURL" = "";
        "browser.safebrowsing.provider.google4.dataSharingURL" = "";
        "browser.safebrowsing.provider.google4.getha=hURL" = "";
        "browser.safebrowsing.provider.google4.repo=tURL" = "";
        "browser.safebrowsing.provider.google4.upda=eURL" = "";
        "browser.safebrowsing.reportPhi=hURL" = "";
        "browser.search.isUS" = true;
        "browser.search.widget.inNavBar" = false;
        "browser.sessionstore.cleanup.forget_closed_after" = 600;
        "browser.sessionstore.interval" = 9999999;
        "browser.sessionstore.interval.idle" = 9999999;
        "browser.sessionstore.resume_from_crash" = false;
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.startup.homepage" = "about:blank";
        "browser.startup.page" = 0;
        "browser.tabs.allowTabDetach" = true;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.loadDivertedInBackground" = true;
        "browser.tabs.loadInBackground" = true;
        "browser.translations.enable" = false;
        "browser.triple_click_selects_paragraph" = true;
        "browser.uidensity" = 1;
        "browser.uitour.url" = "";
        "browser.urlbar.addons.featureGate" = false;
        "browser.urlbar.formatting.enabled" = true;
        "browser.urlbar.mdn.featureGate" = false;
        "browser.urlbar.pocket.featureGate" = false;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.recentsearches" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.weather.featureGate" = false;
        "browser.urlbar.yelp.featureGate" = false;
        "captivedetect.canonicalURL" = false;
        "devtools.aboutdebugging.showSystemAddons" = true;
        "devtools.toolbox.zoomValue" = "=.2";
        "dom.push.connection.enabled" = false;
        "dom.push.userAge=tID" = "";
        "dom.security.https_only_mode_send_http_background_request" = false;
        "dom.webnotifications.serviceworker.enabled" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "extensions.getAddons.showPane" = false; #disable recommendation pane in about:addons (uses Google Analytics)
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
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
        "identity.fxaccounts.enabled" = false;
        "javascript.options.baselinejit" = true; # enable if too slow
        "javascript.options.ion" = true; # enable if too slow
        "layout.spellcheckDefault" = 0;
        "dom.private-attribution.submission.enabled" = false; # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        "mathml.disabled" = true;
        "media.autoplay.default" = 5;
        "media.gmp-widevinecdm.enabled" = false;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "middlemouse.paste" = true;
        "mousewheel.with_shift.action" = 3;
        "network.connectivity-service.enabled" = false;
        "network.dnsCacheEntries" = 0;
        "network.gio.supported-protocols" = "";
        "network.trr.mode" = 1; #3 for on
        # "network.trr.uri" = "https://all.dns.mullvad.net/dns-query";
        "nglayout.enable_drag_images" = false;
        "pdfjs.enableScripting" = false;
        "permissions.default.camera" = 2;
        "permissions.default.desktop-notification" = 2;
        "permissions.default.geo" = 2;
        "permissions.default.microphone" = 2;
        "permissions.default.xr" = 2;
        "permissions.memory_only" = true;
        "privacy.antitracking.enableWebcompat" = false;
        "privacy.clearOnShutdown_v2.cache" = true;
        "privacy.clearHistory.siteSettings" = true;
        "privacy.clearOnShutdown.siteSettings" = true;
        "privacy.clearOnShutdown_v2.siteSettings" = true;
        "privacy.clearSiteData.siteSettings" = true;
        "privacy.cpd.siteSettings" = true;
        "privacy.donottrackheader.enabled" = false;
        "privacy.resistFingerprinting.letterboxing" = true;
        "privacy.spoof_english" = 1;
        "privacy.trackingprotection.cryptomining.enabled" = true;
        "privacy.userContext.ui.enabled" = true;
        "reader.parse-on-load.enabled" = false;
        "security.cert_pinning.enforcement_level" = 2;
        "security.insecure_connection_text.enabled" = true;
        "security.osclientcerts.autoload" = true;
        "security.pki.sha1_enforcement_level" = 1;
        "security.ssl.require_safe_negotiation" = true;
        "security.tls.version.min" = 1;
        "signon.generation.enabled" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "toolkit.coverage.endpoint.base" = "";
        "toolkit.coverage.opt-out" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.server" = "data: =";
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.cosmeticAnimations.enabled" = false;
        "browser.display.use_system_colors" = false;
        ###https://github.com/pyllyukko/user.js

        # PREF: Disable Service Workers
        # https://developer.mozilla.org/en-US/docs/Web/API/Worker
        # https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorker_API
        # https://wiki.mozilla.org/Firefox/Push_Notifications#Service_Workers
        # NOTICE: Disabling ServiceWorkers breaks functionality on some sites (Google Street View...)
        # NOTICE: Disabling ServiceWorkers breaks Firefox Sync
        # Unknown security implications
        # CVE-2016-5259, CVE-2016-2812, CVE-2016-1949, CVE-2016-5287 (fixed)
        "dom.serviceWorkers.enabled" = false;

        # PREF: Disable web notifications
        # https://support.mozilla.org/en-US/questions/1140439
        "dom.webnotifications.enabled" = false;

        # PREF: Disable DOM timing API
        # https://wiki.mozilla.org/Security/Reviews/Firefox/NavigationTimingAPI
        # https://www.w3.org/TR/navigation-timing/#privacy
        # NOTICE: Disabling DOM timing API breaks item pages in AliExpress (https://github.com/pyllyukko/user.js/issues/561)
        "dom.enable_performance" = false;

        # PREF: Disable resource timing API
        # https://www.w3.org/TR/resource-timing/#privacy-security
        # NOTICE: Disabling resource timing API breaks some DDoS protection pages (Cloudflare)
        "dom.enable_resource_timing" = false;

        # PREF: Make sure the User Timing API does not provide a new high resolution timestamp
        # https://trac.torproject.org/projects/tor/ticket/16336
        # https://www.w3.org/TR/2013/REC-user-timing-20131212/#privacy-security
        "dom.enable_user_timing" = false;

        # PREF: Disable Web Audio API
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1288359
        # NOTICE: Web Audio API is required for Unity web player/games
        "dom.webaudio.enabled" = false;

        # PREF: Disable Location-Aware Browsing (geolocation)
        # https://www.mozilla.org/en-US/firefox/geolocation/
        "geo.enabled" = false; #https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks

        # PREF: When geolocation is enabled, use Mozilla geolocation service instead of Google
        # https://bugzilla.mozilla.org/show_bug.cgi?id=689252
        "geo.wifi.uri" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";

        # PREF: When geolocation is enabled, don't log geolocation requests to the console
        "geo.wifi.logging.enabled" = false;

        # PREF: Disable raw TCP socket support (mozTCPSocket)
        # https://trac.torproject.org/projects/tor/ticket/18863
        # https://www.mozilla.org/en-US/security/advisories/mfsa2015-97/
        # https://developer.mozilla.org/docs/Mozilla/B2G_OS/API/TCPSocket
        "dom.mozTCPSocket.enabled" = false;

        # PREF: Disable DOM storage (disabled)
        # https://kb.mozillazine.org/Dom.storage.enabled
        # https://html.spec.whatwg.org/multipage/webstorage.html
        # NOTICE-DISABLED: Disabling DOM storage is known to cause`TypeError: localStorage is null` errors
        #"dom.storage.enabled" = 		false;

        # PREF: Disable leaking network/browser connection information via Javascript
        # Network Information API provides general information about the system's connection type (WiFi, cellular, etc.)
        # https://developer.mozilla.org/en-US/docs/Web/API/Network_Information_API
        # https://wicg.github.io/netinfo/#privacy-considerations
        # https://bugzilla.mozilla.org/show_bug.cgi?id=960426
        "dom.netinfo.enabled" = false;

        # PREF: Disable network API (Firefox < 32)
        # https://developer.mozilla.org/en-US/docs/Web/API/Connection/onchange
        # https://www.torproject.org/projects/torbrowser/design/#fingerprinting-defenses
        "dom.network.enabled" = false;

        # PREF: Disable WebRTC entirely to prevent leaking internal IP addresses (Firefox < 42)
        # NOTICE: Disabling WebRTC breaks peer-to-peer file sharing tools (reep.io ...)
        "media.peerconnection.enabled" = false;

        # PREF: Don't reveal your internal IP when WebRTC is enabled (Firefox >= 42)
        # https://wiki.mozilla.org/Media/WebRTC/Privacy
        # https://github.com/beefproject/beef/wiki/Module%3A-Get-Internal-IP-WebRTC
        "media.peerconnection.ice.default_address_only" = true; # Firefox 42-51
        "media.peerconnection.ice.no_host" = true; # Firefox >= 52

        # PREF: Disable WebRTC getUserMedia, screen sharing, audio capture, video capture
        # https://wiki.mozilla.org/Media/getUserMedia
        # https://blog.mozilla.org/futurereleases/2013/01/12/capture-local-camera-and-microphone-streams-with-getusermedia-now-enabled-in-firefox/
        # https://developer.mozilla.org/en-US/docs/Web/API/Navigator
        "media.navigator.enabled" = false;
        "media.navigator.video.enabled" = false;
        "media.getusermedia.screensharing.enabled" = false;
        "media.getusermedia.audiocapture.enabled" = false;

        # PREF: Disable battery API (Firefox < 52)
        # https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1313580
        "dom.battery.enabled" = false;

        # PREF: Disable telephony API
        # https://wiki.mozilla.org/WebAPI/Security/WebTelephony
        "dom.telephony.enabled" = false;

        # PREF: Disable "beacon" asynchronous HTTP transfers (used for analytics)
        # https://developer.mozilla.org/en-US/docs/Web/API/navigator.sendBeacon
        "beacon.enabled" = false;

        # PREF: Disable clipboard event detection (onCut/onCopy/onPaste) via Javascript
        # https://web.archive.org/web/20210416195937/https://developer.mozilla.org/en-US/docs/Mozilla/Preferences/Preference_reference/dom.event.clipboardevents.enabled
        # https://github.com/pyllyukko/user.js/issues/287
        # NOTICE: Disabling clipboard events breaks Ctrl+C/X/V copy/cut/paste functionaility in JS-based web applications (Google Docs...)
        "dom.event.clipboardevents.enabled" = false;

        # PREF: Disable "copy to clipboard" functionality via Javascript (Firefox >= 41)
        # https://hg.mozilla.org/mozilla-central/rev/2f9f8ea4b9c3
        # https://github.com/pyllyukko/user.js/issues/287
        # NOTICE: Disabling clipboard operations will break legitimate JS-based "copy to clipboard" functionality
        "dom.allow_cut_copy" = false;

        # PREF: Disable speech recognition
        # https://dvcs.w3.org/hg/speech-api/raw-file/tip/speechapi.html
        # https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognition
        # https://wiki.mozilla.org/HTML5_Speech_API
        "media.webspeech.recognition.enable" = false;

        # PREF: Disable speech synthesis
        # https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesis
        "media.webspeech.synth.enabled" = false;

        # PREF: Disable sensor API
        # https://wiki.mozilla.org/Sensor_API
        "device.sensors.enabled" = false;

        # PREF: Disable pinging URIs specified in HTML <a> ping= attributes
        # https://kb.mozillazine.org/Browser.send_pings
        "browser.send_pings" = false;

        # PREF: When browser pings are enabled, only allow pinging the same host as the origin page
        # https://kb.mozillazine.org/Browser.send_pings.require_same_host
        "browser.send_pings.require_same_host" = true;

        # PREF: Disable IndexedDB (disabled)
        # https://developer.mozilla.org/en-US/docs/IndexedDB
        # https://en.wikipedia.org/wiki/Indexed_Database_API
        # https://wiki.mozilla.org/Security/Reviews/Firefox4/IndexedDB_Security_Review
        # https://forums.mozillazine.org/viewtopic.php?p=13842047
        # https://github.com/pyllyukko/user.js/issues/8
        # NOTICE-DISABLED: IndexedDB could be used for tracking purposes, but is required for some add-ons to work (notably uBlock), so is left enabled
        #"dom.indexedDB.enabled" = 		false;

        # TODO: "Access Your Location" "Maintain Offline Storage" "Show Notifications"

        # PREF: Disable gamepad API to prevent USB device enumeration
        # https://www.w3.org/TR/gamepad/
        # https://trac.torproject.org/projects/tor/ticket/13023
        "dom.gamepad.enabled" = false;

        # PREF: Disable virtual reality devices APIs
        # https://developer.mozilla.org/en-US/Firefox/Releases/36#Interfaces.2FAPIs.2FDOM
        # https://developer.mozilla.org/en-US/docs/Web/API/WebVR_API
        "dom.vr.enabled" = false;

        # PREF: Disable vibrator API
        "dom.vibrator.enabled" = false;

        # PREF: Disable Archive API (Firefox < 54)
        # https://wiki.mozilla.org/WebAPI/ArchiveAPI
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1342361
        "dom.archivereader.enabled" = false;

        # PREF: Disable webGL
        # https://en.wikipedia.org/wiki/WebGL
        # https://www.contextis.com/resources/blog/webgl-new-dimension-browser-exploitation/
        # NOTICE: Disabling WebGL breaks WebGL-based websites/applications (windy, meteoblue...)
        "webgl.disabled" = true;
        # PREF: When webGL is enabled, use the minimum capability mode
        "webgl.min_capability_mode" = true;
        # PREF: When webGL is enabled, disable webGL extensions
        # https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API#WebGL_debugging_and_testing
        "webgl.disable-extensions" = true;
        # PREF: When webGL is enabled, force enabling it even when layer acceleration is not supported
        # https://trac.torproject.org/projects/tor/ticket/18603
        "webgl.disable-fail-if-major-performance-caveat" = true;
        # PREF: When webGL is enabled, do not expose information about the graphics driver
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1171228
        # https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_debug_renderer_info
        "webgl.enable-debug-renderer-info" = false;
        # somewhat related...
        #"pdfjs.enableWebGL" = 					false;

        # PREF: Spoof dual-core CPU
        # https://trac.torproject.org/projects/tor/ticket/21675
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1360039
        "dom.maxHardwareConcurrency" = 2;

        # PREF: Disable WebAssembly
        # https://webassembly.org/
        # https://en.wikipedia.org/wiki/WebAssembly
        # https://trac.torproject.org/projects/tor/ticket/21549
        # NOTICE: WebAssembly is required for Unity web player/games
        "javascript.options.wasm" = false; # enable if too slow

        /**
         ****************************************************************************
        * SECTION: Misc                                                              *
        *****************************************************************************
        */

        # PREF: Disable face detection
        "camera.control.face_detection.enabled" = false;

        # PREF: Disable GeoIP lookup on your address to set default search engine region
        # https://trac.torproject.org/projects/tor/ticket/16254
        # https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_geolocation-for-default-search-engine
        "browser.search.countryCode" = "US";
        "browser.search.region" = "US";
        "browser.search.geoip.url" = "";

        # PREF: Set Accept-Language HTTP header to en-US regardless of Firefox localization
        # https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Language
        "intl.accept_languages" = "en-US, en";

        # PREF: Don't use OS values to determine locale, force using Firefox locale setting
        # https://kb.mozillazine.org/Intl.locale.matchOS
        "intl.locale.matchOS" = false;

        # Use LANG environment variable to choose locale (disabled)
        #pref("intl.locale.requested" =  "";

        # PREF: Don't use Mozilla-provided location-specific search engines
        "browser.search.geoSpecificDefaults" = false;

        # PREF: Do not automatically send selection to clipboard on some Linux platforms
        # https://kb.mozillazine.org/Clipboard.autocopy
        "clipboard.autocopy" = false;

        # PREF: Prevent leaking application locale/date format using JavaScript
        # https://bugzilla.mozilla.org/show_bug.cgi?id=867501
        # https://hg.mozilla.org/mozilla-central/rev/52d635f2b33d
        "javascript.use_us_english_locale" = true;

        # PREF: Do not submit invalid URIs entered in the address bar to the default search engine
        # https://kb.mozillazine.org/Keyword.enabled
        "keyword.enabled" = true; # false = no automatic search engine

        # PREF: Don't trim HTTP off of URLs in the address bar.
        # https://bugzilla.mozilla.org/show_bug.cgi?id=665580
        "browser.urlbar.trimURLs" = false;

        # PREF: Disable preloading of autocomplete URLs.
        # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        "browser.urlbar.speculativeConnect.enabled" = false;

        # PREF: Don't try to guess domain names when entering an invalid domain name in URL bar
        # https://www-archive.mozilla.org/docs/end-user/domain-guessing.html
        "browser.fixup.alternate.enabled" = false;

        # PREF: When browser.fixup.alternate.enabled is enabled, strip password from 'user:password@...' URLs
        # https://github.com/pyllyukko/user.js/issues/290#issuecomment-303560851
        "browser.fixup.hide_user_pass" = true;

        # PREF: Send DNS request through SOCKS when SOCKS proxying is in use
        # https://trac.torproject.org/projects/tor/wiki/doc/TorifyHOWTO/WebBrowsers
        "network.proxy.socks_remote_dns" = true;

        # PREF: Don't monitor OS online/offline connection state
        # https://trac.torproject.org/projects/tor/ticket/18945
        "network.manage-offline-status" = false;

        # PREF: Enforce Mixed Active Content Blocking
        # https://support.mozilla.org/t5/Protect-your-privacy/Mixed-content-blocking-in-Firefox/ta-p/10990
        # https://developer.mozilla.org/en-US/docs/Site_Compatibility_for_Firefox_23#Non-SSL_contents_on_SSL_pages_are_blocked_by_default
        # https://blog.mozilla.org/tanvi/2013/04/10/mixed-content-blocking-enabled-in-firefox-23/
        "security.mixed_content.block_active_content" = true;

        # PREF: Enforce Mixed Passive Content blocking (a.k.a. Mixed Display Content)
        # NOTICE: Enabling Mixed Display Content blocking can prevent images/styles... from loading properly when connection to the website is only partially secured
        "security.mixed_content.block_display_content" = true;

        # PREF: Disable JAR from opening Unsafe File Types
        # https://kb.mozillazine.org/Network.jar.open-unsafe-types
        # CIS Mozilla Firefox 24 ESR v1.0.0 - 3.7
        "network.jar.open-unsafe-types" = false;

        # CIS 2.7.4 Disable Scripting of Plugins by JavaScript
        # https://forums.mozillazine.org/viewtopic.php?f=7&t=153889
        "security.xpconnect.plugin.unrestricted" = false;

        # PREF: Set File URI Origin Policy
        # https://kb.mozillazine.org/Security.fileuri.strict_origin_policy
        # CIS Mozilla Firefox 24 ESR v1.0.0 - 3.8
        "security.fileuri.strict_origin_policy" = true;

        # PREF: Disable Displaying Javascript in History URLs
        # https://kb.mozillazine.org/Browser.urlbar.filter.javascript
        # CIS 2.3.6
        "browser.urlbar.filter.javascript" = true;

        # PREF: Disable asm.js
        # https://asmjs.org/
        # https://www.mozilla.org/en-US/security/advisories/mfsa2015-29/
        # https://www.mozilla.org/en-US/security/advisories/mfsa2015-50/
        # https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2015-2712
        "javascript.options.asmjs" = false; # enable if too slow

        # PREF: Disable SVG in OpenType fonts
        # https://wiki.mozilla.org/SVGOpenTypeFonts
        # https://github.com/iSECPartners/publications/tree/master/reports/Tor%20Browser%20Bundle
        "gfx.font_rendering.opentype_svg.enabled" = false;

        # PREF: Disable in-content SVG rendering (Firefox >= 53) (disabled)
        # NOTICE-DISABLED: Disabling SVG support breaks many UI elements on many sites
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1216893
        # https://github.com/iSECPartners/publications/raw/master/reports/Tor%20Browser%20Bundle/Tor%20Browser%20Bundle%20-%20iSEC%20Deliverable%201.3.pdf#16
        #"svg.disabled" =  true;

        # PREF: Disable video stats to reduce fingerprinting threat
        # https://bugzilla.mozilla.org/show_bug.cgi?id=654550
        # https://github.com/pyllyukko/user.js/issues/9#issuecomment-100468785
        # https://github.com/pyllyukko/user.js/issues/9#issuecomment-148922065
        "media.video_stats.enabled" = false;

        # PREF: Don't reveal build ID
        # Value taken from Tor Browser
        # https://bugzilla.mozilla.org/show_bug.cgi?id=583181
        "general.buildID.override" = "20100101";
        "browser.startup.homepage_override.buildID" = "20100101";

        # PREF: Don't use document specified fonts to prevent installed font enumeration (fingerprinting)
        # https://github.com/pyllyukko/user.js/issues/395
        # https://browserleaks.com/fonts
        # https://github.com/pyllyukko/user.js/issues/120
        "browser.display.use_document_fonts" = 0;

        # PREF: Enable only whitelisted URL protocol handlers
        # https://kb.mozillazine.org/Network.protocol-handler.external-default
        # https://kb.mozillazine.org/Network.protocol-handler.warn-external-default
        # https://kb.mozillazine.org/Network.protocol-handler.expose.%28protocol%29
        # https://news.ycombinator.com/item?id=13047883
        # https://bugzilla.mozilla.org/show_bug.cgi?id=167475
        # https://github.com/pyllyukko/user.js/pull/285#issuecomment-298124005
        # NOTICE: Disabling nonessential protocols breaks all interaction with custom protocols such as mailto:, irc:, magnet: ... and breaks opening third-party mail/messaging/torrent/... clients when clicking on links with these protocols
        # TODO: Add externally-handled protocols from Windows 8.1 and Windows 10 (currently contains protocols only from Linux and Windows 7) that might pose a similar threat (see e.g. https://news.ycombinator.com/item?id=13044991)
        # TODO: Add externally-handled protocols from Mac OS X that might pose a similar threat (see e.g. https://news.ycombinator.com/item?id=13044991)
        # If you want to enable a protocol, set network.protocol-handler.expose.(protocol) to true and network.protocol-handler.external.(protocol) to:
        #   * true, if the protocol should be handled by an external application
        #   * false, if the protocol should be handled internally by Firefox
        "network.protocol-handler.warn-external-default" = true;
        "network.protocol-handler.external.http" = false;
        "network.protocol-handler.external.https" = false;
        "network.protocol-handler.external.javascript" = false;
        "network.protocol-handler.external.moz-extension" = false;
        "network.protocol-handler.external.ftp" = false;
        "network.protocol-handler.external.file" = false;
        "network.protocol-handler.external.about" = false;
        "network.protocol-handler.external.chrome" = false;
        "network.protocol-handler.external.blob" = false;
        "network.protocol-handler.external.data" = false;
        "network.protocol-handler.expose-all" = false;
        "network.protocol-handler.expose.http" = true;
        "network.protocol-handler.expose.https" = true;
        "network.protocol-handler.expose.javascript" = true;
        "network.protocol-handler.expose.moz-extension" = true;
        "network.protocol-handler.expose.ftp" = true;
        "network.protocol-handler.expose.file" = true;
        "network.protocol-handler.expose.about" = true;
        "network.protocol-handler.expose.chrome" = true;
        "network.protocol-handler.expose.blob" = true;
        "network.protocol-handler.expose.data" = true;

        /**
         ****************************************************************************
        * SECTION: Extensions / plugins                                                       *
        *****************************************************************************
        */

        # PREF: Ensure you have a security delay when installing add-ons (milliseconds)
        # https://kb.mozillazine.org/Disable_extension_install_delay_-_Firefox
        # https://www.squarefree.com/2004/07/01/race-conditions-in-security-dialogs/
        "security.dialog_enable_delay" = 1000;

        # PREF: Require signatures
        # https://wiki.mozilla.org/Addons/Extension_Signing
        #"xpinstall.signatures.required" = 		true;

        # PREF: Opt-out of add-on metadata updates
        # https://blog.mozilla.org/addons/how-to-opt-out-of-add-on-metadata-updates/
        "extensions.getAddons.cache.enabled" = false;

        # PREF: Opt-out of themes (Persona) updates
        # https://support.mozilla.org/t5/Firefox/how-do-I-prevent-autoamtic-updates-in-a-50-user-environment/td-p/144287
        "lightweightThemes.update.enabled" = false;

        # PREF: Disable Flash Player NPAPI plugin
        # https://kb.mozillazine.org/Flash_plugin
        "plugin.state.flash" = 0;

        # PREF: Disable Java NPAPI plugin
        "plugin.state.java" = 0;

        # PREF: Disable sending Flash Player crash reports
        "dom.ipc.plugins.flash.subprocess.crashreporter.enabled" = false;

        # PREF: When Flash crash reports are enabled, don't send the visited URL in the crash report
        "dom.ipc.plugins.reportCrashURL" = false;

        # PREF: When Flash is enabled, download and use Mozilla SWF URIs blocklist
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1237198
        # https://github.com/mozilla-services/shavar-plugin-blocklist
        "browser.safebrowsing.blockedURIs.enabled" = true;

        # PREF: Disable Gnome Shell Integration NPAPI plugin
        "plugin.state.libgnome-shell-browser-plugin" = 0;

        # PREF: Disable the bundled OpenH264 video codec (disabled)
        # https://forums.mozillazine.org/viewtopic.php?p=13845077&sid=28af2622e8bd8497b9113851676846b1#p13845077
        #"media.gmp-provider.enabled" = 		false;

        # PREF: Enable plugins click-to-play
        # https://wiki.mozilla.org/Firefox/Click_To_Play
        # https://blog.mozilla.org/security/2012/10/11/click-to-play-plugins-blocklist-style/
        "plugins.click_to_play" = true;

        # PREF: Updates addons automatically
        # https://blog.mozilla.org/addons/how-to-turn-off-add-on-updates/
        "extensions.update.enabled" = true;

        # PREF: Enable add-on and certificate blocklists (OneCRL) from Mozilla
        # https://wiki.mozilla.org/Blocklisting
        # https://blocked.cdn.mozilla.net/
        # https://kb.mozillazine.org/Extensions.blocklist.enabled
        # https://kb.mozillazine.org/Extensions.blocklist.url
        # https://blog.mozilla.org/security/2015/03/03/revoking-intermediate-certificates-introducing-onecrl/
        # Updated at interval defined in extensions.blocklist.interval (default: 86400)
        "extensions.blocklist.enabled" = true;
        "services.blocklist.update_enabled" = true;

        # PREF: Decrease system information leakage to Mozilla blocklist update servers
        # https://trac.torproject.org/projects/tor/ticket/16931
        "extensions.blocklist.url" = "https://blocklist.addons.mozilla.org/blocklist/3/%APP_ID%/%APP_VERSION%/";

        # PREF: Disable system add-on updates (hidden & always-enabled add-ons from Mozilla)
        # https://firefox-source-docs.mozilla.org/toolkit/mozapps/extensions/addon-manager/SystemAddons.html
        # https://blog.mozilla.org/data/2018/08/20/effectively-measuring-search-in-firefox/
        # https://github.com/pyllyukko/user.js/issues/419
        # https://dxr.mozilla.org/mozilla-central/source/toolkit/mozapps/extensions/AddonManager.jsm#1248-1257
        # NOTICE: Disabling system add-on updates prevents Mozilla from "hotfixing" your browser to patch critical problems (one possible use case from the documentation)
        "extensions.systemAddon.update.enabled" = false;

        /**
         ****************************************************************************
        * SECTION: Firefox (anti-)features / components                              *                            *
        *****************************************************************************
        */

        # PREF: Disable Extension recommendations (Firefox >= 65)
        # https://support.mozilla.org/en-US/kb/extension-recommendations
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr" = false;

        # PREF: Trusted Recursive Resolver (DNS-over-HTTPS) (disabled)
        # https://wiki.mozilla.org/Trusted_Recursive_Resolver
        #"network.trr.mode" = 					0;

        # PREF: Disable WebIDE
        # https://trac.torproject.org/projects/tor/ticket/16222
        # https://developer.mozilla.org/docs/Tools/WebIDE
        "devtools.webide.enabled" = false;
        "devtools.webide.autoinstallADBHelper" = false;
        "devtools.webide.autoinstallFxdtAdapters" = false;

        # PREF: Disable remote debugging
        # https://developer.mozilla.org/en-US/docs/Tools/Remote_Debugging/Debugging_Firefox_Desktop
        # https://developer.mozilla.org/en-US/docs/Tools/Tools_Toolbox#Advanced_settings
        "devtools.debugger.remote-enabled" = false;
        "devtools.chrome.enabled" = false;
        "devtools.debugger.force-local" = true;

        # PREF: Disable Mozilla telemetry/experiments
        # https://wiki.mozilla.org/Platform/Features/Telemetry
        # https://wiki.mozilla.org/Privacy/Reviews/Telemetry
        # https://wiki.mozilla.org/Telemetry
        # https://www.mozilla.org/en-US/legal/privacy/firefox.html#telemetry
        # https://support.mozilla.org/t5/Firefox-crashes/Mozilla-Crash-Reporter/ta-p/1715
        # https://wiki.mozilla.org/Security/Reviews/Firefox6/ReviewNotes/telemetry
        # https://gecko.readthedocs.io/en/latest/browser/experiments/experiments/manifest.html
        # https://wiki.mozilla.org/Telemetry/Experiments
        # https://support.mozilla.org/en-US/questions/1197144
        # https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html#id1
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.archive.enabled" = false;
        "experiments.supported" = false;
        "experiments.enabled" = false;
        "experiments.manifest.uri" = "";

        # PREF: Disallow Necko to do A/B testing
        # https://trac.torproject.org/projects/tor/ticket/13170
        "network.allow-experiments" = false;

        # PREF: Disable sending Firefox crash reports to Mozilla servers
        # https://wiki.mozilla.org/Breakpad
        # https://kb.mozillazine.org/Breakpad
        # https://dxr.mozilla.org/mozilla-central/source/toolkit/crashreporter
        # https://bugzilla.mozilla.org/show_bug.cgi?id=411490
        # A list of submitted crash reports can be found at about:crashes
        "breakpad.reportURL" = "";

        # PREF: Disable sending reports of tab crashes to Mozilla (about:tabcrashed), don't nag user about unsent crash reports
        # https://hg.mozilla.org/mozilla-central/file/tip/browser/app/profile/firefox.js
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.crashReports.unsubmittedCheck.enabled" = false;

        # PREF: Disable FlyWeb (discovery of LAN/proximity IoT devices that expose a Web interface)
        # https://wiki.mozilla.org/FlyWeb
        # https://wiki.mozilla.org/FlyWeb/Security_scenarios
        # https://docs.google.com/document/d/1eqLb6cGjDL9XooSYEEo7mE-zKQ-o-AuDTcEyNhfBMBM/edit
        # https://www.ghacks.net/2016/07/26/firefox-flyweb
        "dom.flyweb.enabled" = false;

        # PREF: Disable the UITour backend
        # https://trac.torproject.org/projects/tor/ticket/19047#comment:3
        "browser.uitour.enabled" = false;

        # PREF: Enable Firefox Tracking Protection
        # https://wiki.mozilla.org/Security/Tracking_protection
        # https://support.mozilla.org/en-US/kb/tracking-protection-firefox
        # https://support.mozilla.org/en-US/kb/tracking-protection-pbm
        # https://kontaxis.github.io/trackingprotectionfirefox/
        # https://feeding.cloud.geek.nz/posts/how-tracking-protection-works-in-firefox/
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.pbmode.enabled" = true;

        # PREF: Enable contextual identity Containers feature (Firefox >= 52)
        # NOTICE: Containers are not available in Private Browsing mode
        # https://wiki.mozilla.org/Security/Contextual_Identity_Project/Containers
        "privacy.userContext.enabled" = true;

        # PREF: Enable Firefox's anti-fingerprinting mode ("resist fingerprinting" or RFP) (Tor Uplift project)
        # https://wiki.mozilla.org/Security/Tor_Uplift/Tracking
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1333933
        # https://wiki.mozilla.org/Security/Fingerprinting
        # NOTICE: RFP breaks some keyboard shortcuts used in certain websites (see #443)
        # NOTICE: RFP changes your time zone
        # NOTICE: RFP breaks some DDoS protection pages (Cloudflare)
        "privacy.resistFingerprinting" = true;

        # PREF: disable mozAddonManager Web API [FF57+]
        # https://bugzilla.mozilla.org/buglist.cgi?bug_id=1384330
        # https://bugzilla.mozilla.org/buglist.cgi?bug_id=1406795
        # https://bugzilla.mozilla.org/buglist.cgi?bug_id=1415644
        # https://bugzilla.mozilla.org/buglist.cgi?bug_id=1453988
        # https://trac.torproject.org/projects/tor/ticket/26114
        "privacy.resistFingerprinting.block_mozAddonManager" = true;
        "extensions.webextensions.restrictedDomains" = "";

        # PREF: enable RFP letterboxing / resizing of inner window [FF67+] (disabled)
        # https://bugzilla.mozilla.org/1407366
        #"privacy.resistFingerprinting.letterboxing" =  true;
        #"privacy.resistFingerprinting.letterboxing.dimensions" =  "800x600, 1000x1000, 1600x900";

        # PREF: disable showing about:blank/maximized window as soon as possible during startup [FF60+]
        # https://bugzilla.mozilla.org/1448423
        "browser.startup.blankWindow" = false;

        # PREF: Disable the built-in PDF viewer
        # https://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2015-2743
        # https://blog.mozilla.org/security/2015/08/06/firefox-exploit-found-in-the-wild/
        # https://www.mozilla.org/en-US/security/advisories/mfsa2015-69/
        "pdfjs.disabled" = true;

        # PREF: Disable collection/sending of the health report (healthreport.sqlite*)
        # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
        # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.healthreport.service.enabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        # "Allow Firefox to make personalized extension recommendations"
        "browser.discovery.enabled" = false;

        # PREF: Disable Shield/Heartbeat/Normandy (Mozilla user rating telemetry)
        # https://wiki.mozilla.org/Advocacy/heartbeat
        # https://trac.torproject.org/projects/tor/ticket/19047
        # https://trac.torproject.org/projects/tor/ticket/18738
        # https://wiki.mozilla.org/Firefox/Shield
        # https://github.com/mozilla/normandy
        # https://support.mozilla.org/en-US/kb/shield
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1370801
        # https://wiki.mozilla.org/Firefox/Normandy/PreferenceRollout
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "extensions.shield-recipe-client.enabled" = false;
        "app.shield.optoutstudies.enabled" = false;

        # PREF: Disable Firefox Hello (disabled) (Firefox < 49)
        # https://wiki.mozilla.org/Loop
        # https://support.mozilla.org/t5/Chat-and-share/Support-for-Hello-discontinued-in-Firefox-49/ta-p/37946
        # NOTICE-DISABLED: Firefox Hello requires setting `media.peerconnection.enabled` and `media.getusermedia.screensharing.enabled` to true, `security.OCSP.require` to false to work.
        #"loop.enabled" = 		false;

        # PREF: Disable Firefox Hello metrics collection
        # https://groups.google.com/d/topic/mozilla.dev.platform/nyVkCx-_sFw/discussion
        "loop.logDomains" = false;

        # PREF: Enable Auto Update (disabled)
        # NOTICE: Fully automatic updates are disabled and left to package management systems on Linux. Windows users may want to change this setting.
        # CIS 2.1.1
        #"app.update.auto" = 					true;

        # PREF: Enforce checking for Firefox updates
        # https://kb.mozillazine.org/App.update.enabled
        # NOTICE: Update check page might incorrectly report Firefox ESR as out-of-date
        "app.update.enabled" = true;

        # PREF: Enable blocking reported web forgeries
        # https://wiki.mozilla.org/Security/Safe_Browsing
        # https://kb.mozillazine.org/Safe_browsing
        # https://support.mozilla.org/en-US/kb/how-does-phishing-and-malware-protection-work
        # https://forums.mozillazine.org/viewtopic.php?f=39&t=2711237&p=12896849#p12896849
        # CIS 2.3.4
        "browser.safebrowsing.enabled" = true; # Firefox < 50
        "browser.safebrowsing.phishing.enabled" = true; # firefox >= 50

        # PREF: Enable blocking reported attack sites
        # https://kb.mozillazine.org/Browser.safebrowsing.malware.enabled
        # CIS 2.3.5
        "browser.safebrowsing.malware.enabled" = true;

        # PREF: Disable querying Google Application Reputation database for downloaded binary files
        # https://www.mozilla.org/en-US/firefox/39.0/releasenotes/
        # https://wiki.mozilla.org/Security/Application_Reputation
        "browser.safebrowsing.downloads.remote.enabled" = false;

        # PREF: Disable Pocket
        # https://support.mozilla.org/en-US/kb/save-web-pages-later-pocket-firefox
        # https://github.com/pyllyukko/user.js/issues/143
        "browser.pocket.enabled" = false;
        "extensions.pocket.enabled" = false;

        # PREF: Disable "Recommended by Pocket" in Firefox Quantum
        "browser.newtabpage.activity-stream.feeds.section.topstories" = false;

        # PREF: Enable Global Privacy Control (GPC) (Firefox >= 120)
        # https://support.mozilla.org/1/firefox/126.0/Linux/en-US/global-privacy-control
        # https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Sec-GPC
        # https://globalprivacycontrol.org/
        "privacy.globalprivacycontrol.enabled" = true;

        # PREF: Hide weather on New Tab
        "browser.newtabpage.activity-stream.showWeather" = false;

        /**
         ****************************************************************************
        * SECTION: Automatic connections                                             *
        *****************************************************************************
        */

        # PREF: Limit the connection keep-alive timeout to 15 seconds (disabled)
        # https://github.com/pyllyukko/user.js/issues/387
        # https://kb.mozillazine.org/Network.http.keep-alive.timeout
        # https://httpd.apache.org/docs/current/mod/core.html#keepalivetimeout
        #"network.http.keep-alive.timeout" = 			15;

        # PREF: Disable prefetching of <link rel="next"> URLs
        # https://kb.mozillazine.org/Network.prefetch-next
        # https://developer.mozilla.org/en-US/docs/Web/HTTP/Link_prefetching_FAQ#Is_there_a_preference_to_disable_link_prefetching.3F
        "network.prefetch-next" = false;

        # PREF: Disable DNS prefetching
        # https://kb.mozillazine.org/Network.dns.disablePrefetch
        # https://developer.mozilla.org/en-US/docs/Web/HTTP/Controlling_DNS_prefetching
        "network.dns.disablePrefetch" = true;
        "network.dns.disablePrefetchFromHTTPS" = true;

        # PREF: Disable the predictive service (Necko)
        # https://wiki.mozilla.org/Privacy/Reviews/Necko
        "network.predictor.enabled" = false;

        # PREF: Reject .onion hostnames before passing the to DNS
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1228457
        # RFC 7686
        "network.dns.blockDotOnion" = true;

        # PREF: Disable search suggestions in the search bar
        # https://kb.mozillazine.org/Browser.search.suggest.enabled
        "browser.search.suggest.enabled" = false;

        # PREF: Disable "Show search suggestions in location bar results"
        "browser.urlbar.suggest.searches" = false;
        # PREF: When using the location bar, don't suggest URLs from browsing history
        "browser.urlbar.suggest.history" = false;
        # PREF: Disable Firefox Suggest
        # https://www.ghacks.net/2021/09/09/how-to-disable-firefox-suggest/
        # https://support.mozilla.org/en-US/kb/navigate-web-faster-firefox-suggest
        "browser.urlbar.groupLabels.enabled" = false; # Firefox >= 93

        # PREF: Disable SSDP
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1111967
        "browser.casting.enabled" = false;

        # PREF: Disable automatic downloading of OpenH264 codec
        # https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_media-capabilities
        # https://andreasgal.com/2014/10/14/openh264-now-in-firefox/
        "media.gmp-gmpopenh264.enabled" = false;
        "media.gmp-manager.url" = "";

        # PREF: Disable speculative pre-connections
        # https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_speculative-pre-connections
        # https://bugzilla.mozilla.org/show_bug.cgi?id=814169
        "network.http.speculative-parallel-limit" = 0;

        # PREF: Disable downloading homepage snippets/messages from Mozilla
        # https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_mozilla-content
        # https://wiki.mozilla.org/Firefox/Projects/Firefox_Start/Snippet_Service
        "browser.aboutHomeSnippets.updateUrl" = "";

        # PREF: Never check updates for search engines
        # https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_auto-update-checking
        "browser.search.update" = false;

        # PREF: Disable automatic captive portal detection (Firefox >= 52.0)
        # https://support.mozilla.org/en-US/questions/1157121
        "network.captive-portal-service.enabled" = false;

        # PREF: Disable (parts of?) "TopSites"
        "browser.topsites.contile.enabled" = false;
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;

        /**
         ****************************************************************************
        * SECTION: HTTP                                                              *
        *****************************************************************************
        */

        # PREF: Disallow NTLMv1
        # https://bugzilla.mozilla.org/show_bug.cgi?id=828183
        "network.negotiate-auth.allow-insecure-ntlm-v1" = false;
        # it is still allowed through HTTPS. uncomment the following to disable it completely.
        #"network.negotiate-auth.allow-insecure-ntlm-v1-https" = 		false;

        # PREF: Enable CSP 1.1 script-nonce directive support
        # https://bugzilla.mozilla.org/show_bug.cgi?id=855326
        "security.csp.experimentalEnabled" = true;

        # PREF: Enable Content Security Policy (CSP)
        # https://developer.mozilla.org/en-US/docs/Web/Security/CSP/Introducing_Content_Security_Policy
        # https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP
        "security.csp.enable" = true;

        # PREF: Enable Subresource Integrity
        # https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity
        # https://wiki.mozilla.org/Security/Subresource_Integrity
        "security.sri.enable" = true;

        # PREF: DNT HTTP header (disabled)
        # https://www.mozilla.org/en-US/firefox/dnt/
        # https://en.wikipedia.org/wiki/Do_not_track_header
        # https://dnt-dashboard.mozilla.org
        # https://github.com/pyllyukko/user.js/issues/11
        # NOTICE: Do No Track must be enabled manually
        #"privacy.donottrackheader.enabled" = 		true;

        # PREF: Send a referer header with the target URI as the source (disabled)
        # https://bugzilla.mozilla.org/show_bug.cgi?id=822869
        # https://github.com/pyllyukko/user.js/issues/227
        # NOTICE-DISABLED: Spoofing referers breaks functionality on websites relying on authentic referer headers
        # NOTICE-DISABLED: Spoofing referers breaks visualisation of 3rd-party sites on the Lightbeam addon
        # NOTICE-DISABLED: Spoofing referers disables CSRF protection on some login pages not implementing origin-header/cookie+token based CSRF protection
        # TODO: https://github.com/pyllyukko/user.js/issues/94, commented-out XOriginPolicy/XOriginTrimmingPolicy" = 2 prefs
        #"network.http.referer.spoofSource" = 			true;

        # PREF: Don't send referer headers when following links across different domains
        # https://github.com/pyllyukko/user.js/issues/227
        # https://github.com/pyllyukko/user.js/issues/328
        # https://feeding.cloud.geek.nz/posts/tweaking-referrer-for-privacy-in-firefox/
        # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        # NOTICE: Blocking referers across same eTLD sites breaks some login flows relying on them, consider lowering this pref to 1
        "network.http.referer.XOriginPolicy" = 2; #https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks

        # PREF: Trim HTTP referer headers to only send the scheme, host, and port
        # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        "network.http.referer.trimmingPolicy" = 2;

        # PREF: When sending Referer across domains, only send scheme, host, and port in the Referer header
        # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
        "network.http.referer.XOriginTrimmingPolicy" = 2; #control the amount of cross-origin information to send. 0=send full URI (default), 1=scheme+host+port+path, 2=scheme+host+port

        # PREF: Accept Only 1st Party Cookies
        # https://kb.mozillazine.org/Network.cookie.cookieBehavior#1
        # NOTICE: Blocking 3rd-party cookies breaks a number of payment gateways
        # CIS 2.5.1
        "network.cookie.cookieBehavior" = 1;

        # PREF: Enable first-party isolation
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1299996
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1260931
        # https://wiki.mozilla.org/Security/FirstPartyIsolation
        # NOTICE: First-party isolation breaks Microsoft Teams
        # NOTICE: First-party isolation causes HTTP basic auth to ask for credentials for every new tab (see #425)
        "privacy.firstparty.isolate" = true;

        # PREF: Make sure that third-party cookies (if enabled) never persist beyond the session.
        # https://feeding.cloud.geek.nz/posts/tweaking-cookies-for-privacy-in-firefox/
        # https://kb.mozillazine.org/Network.cookie.thirdparty.sessionOnly
        # https://developer.mozilla.org/en-US/docs/Cookies_Preferences_in_Mozilla#network.cookie.thirdparty.sessionOnly
        "network.cookie.thirdparty.sessionOnly" = true;

        # PREF: Spoof User-agent (disabled)
        #"general.useragent.override" = 				"Mozilla/5.0 (Windows NT 6.1; rv:45.0) Gecko/20100101 Firefox/45.0";
        #"general.appname.override" = 				"Netscape";
        #"general.appversion.override" = 			"5.0 (Windows)";
        #"general.platform.override" = 				"Win32";
        #"general.oscpu.override" = 				"Windows NT 6.1";

        /**
         *****************************************************************************
        * SECTION: Caching                                                            *
        *****************************************************************************
        */

        # PREF: Permanently enable private browsing mode
        # https://support.mozilla.org/en-US/kb/Private-Browsing
        # https://wiki.mozilla.org/PrivateBrowsing
        # NOTICE: You can not view or inspect cookies when in private browsing: https://bugzilla.mozilla.org/show_bug.cgi?id=823941
        # NOTICE: When Javascript is enabled, Websites can detect use of Private Browsing mode
        # NOTICE: Private browsing breaks Kerberos authentication
        # NOTICE: Disables "Containers" functionality (see below)
        # NOTICE: "Always use private browsing mode" (browser.privatebrowsing.autostart) disables the possibility to use password manager: https://support.mozilla.org/en-US/kb/usernames-and-passwords-are-not-saved#w_private-browsing
        "browser.privatebrowsing.autostart" = true;

        # PREF: Do not download URLs for the offline cache
        # https://kb.mozillazine.org/Browser.cache.offline.enable
        "browser.cache.offline.enable" = false;

        # PREF: Clear history when Firefox closes
        # https://support.mozilla.org/en-US/kb/Clear%20Recent%20History#w_how-do-i-make-firefox-clear-my-history-automatically
        # NOTICE: Installing user.js will remove your browsing history, caches and local storage.
        # NOTICE: Installing user.js **will remove your saved passwords** (https://github.com/pyllyukko/user.js/issues/27)
        # NOTICE: Clearing open windows on Firefox exit causes 2 windows to open when Firefox starts https://bugzilla.mozilla.org/show_bug.cgi?id=1334945
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "privacy.clearOnShutdown.cache" = true;
        "privacy.clearOnShutdown.cookies" = true;
        "privacy.clearOnShutdown.downloads" = true;
        "privacy.clearOnShutdown.formdata" = true;
        "privacy.clearOnShutdown.history" = true;
        "privacy.clearOnShutdown.offlineApps" = true;
        "privacy.clearOnShutdown.sessions" = true;
        "privacy.clearOnShutdown.openWindows" = true;

        # PREF: Set time range to "Everything" as default in "Clear Recent History"
        "privacy.sanitize.timeSpan" = 0;

        # PREF: Clear everything but "Site Preferences" in "Clear Recent History"
        "privacy.cpd.offlineApps" = true;
        "privacy.cpd.cache" = true;
        "privacy.cpd.cookies" = true;
        "privacy.cpd.downloads" = true;
        "privacy.cpd.formdata" = true;
        "privacy.cpd.history" = true;
        "privacy.cpd.sessions" = true;

        # PREF: Don't remember browsing history
        "places.history.enabled" = false;

        # PREF: Don't remember recently closed tabs
        "browser.sessionstore.max_tabs_undo" = 0;

        # PREF: Disable disk cache
        # https://kb.mozillazine.org/Browser.cache.disk.enable
        "browser.cache.disk.enable" = false; ## enable if too slow

        # PREF: Disable memory cache (disabled)
        # https://kb.mozillazine.org/Browser.cache.memory.enable
        #"browser.cache.memory.enable" = 		false;

        # PREF: Disable Caching of SSL Pages
        # CIS Version 1.2.0 October 21st, 2011 2.5.8
        # https://kb.mozillazine.org/Browser.cache.disk_cache_ssl
        "browser.cache.disk_cache_ssl" = false;

        # PREF: Disable download history
        # CIS Version 1.2.0 October 21st, 2011 2.5.5
        "browser.download.manager.retention" = 0;

        # PREF: Disable password manager (use an external password manager!)
        # CIS Version 1.2.0 October 21st, 2011 2.5.2
        "signon.rememberSignons" = false;

        # PREF: Disable form autofill, don't save information entered in web page forms and the Search Bar
        "browser.formfill.enable" = false;

        # PREF: Cookies expires at the end of the session (when the browser closes)
        # https://kb.mozillazine.org/Network.cookie.lifetimePolicy#2
        "network.cookie.lifetimePolicy" = 2;

        # PREF: Require manual intervention to autofill known username/passwords sign-in forms
        # https://kb.mozillazine.org/Signon.autofillForms
        # https://www.torproject.org/projects/torbrowser/design/#identifier-linkability
        "signon.autofillForms" = false;

        # PREF: Disable formless login capture
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1166947
        "signon.formlessCapture.enabled" = false;

        # PREF: When username/password autofill is enabled, still disable it on non-HTTPS sites
        # https://hg.mozilla.org/integration/mozilla-inbound/rev/f0d146fe7317
        "signon.autofillForms.http" = false;

        # PREF: Show in-content login form warning UI for insecure login fields
        # https://hg.mozilla.org/integration/mozilla-inbound/rev/f0d146fe7317
        "security.insecure_field_warning.contextual.enabled" = true;

        # PREF: Disable the password manager for pages with autocomplete=off (disabled)
        # https://bugzilla.mozilla.org/show_bug.cgi?id=956906
        # OWASP ASVS V9.1
        # Does not prevent any kind of auto-completion (see browser.formfill.enable, signon.autofillForms)
        #"signon.storeWhenAutocompleteOff" = 			false;

        # PREF: Delete Search and Form History
        # CIS Version 1.2.0 October 21st, 2011 2.5.6
        "browser.formfill.expire_days" = 0;

        # PREF: Clear SSL Form Session Data
        # https://kb.mozillazine.org/Browser.sessionstore.privacy_level#2
        # Store extra session data for unencrypted (non-HTTPS) sites only.
        # CIS Version 1.2.0 October 21st, 2011 2.5.7
        # NOTE: CIS says 1, we use 2
        "browser.sessionstore.privacy_level" = 2; ## disable storing extra session data such as form content, cookies and POST data 0=everywhere, 1=unencrypted sites, 2=nowhere

        # PREF: Delete temporary files on exit
        # https://bugzilla.mozilla.org/show_bug.cgi?id=238789
        "browser.helperApps.deleteTempFileOnExit" = true;

        # PREF: Do not create screenshots of visited pages (relates to the "new tab page" feature)
        # https://support.mozilla.org/en-US/questions/973320
        # https://developer.mozilla.org/en-US/docs/Mozilla/Preferences/Preference_reference/browser.pagethumbnails.capturing_disabled
        "browser.pagethumbnails.capturing_disabled" = true;

        # PREF: Don't fetch and permanently store favicons for Windows .URL shortcuts created by drag and drop
        # NOTICE: .URL shortcut files will be created with a generic icon
        # Favicons are stored as .ico files in $profile_dir\shortcutCache
        "browser.shell.shortcutFavicons" = true; #this is enabled

        # PREF: Disable bookmarks backups (default: 15)
        # https://kb.mozillazine.org/Browser.bookmarks.max_backups
        "browser.bookmarks.max_backups" = 0;

        # PREF: Export bookmarks to HTML automatically when closing Firefox (disabled)
        # https://support.mozilla.org/en-US/questions/1176242
        #"browser.bookmarks.autoExportHTML" =  				true;
        #"browser.bookmarks.file" = 	'/path/to/bookmarks-export.html';

        # PREF: Disable downloading of favicons in response to favicon fingerprinting techniques
        # https://github.com/jonasstrehle/supercookie
        # https://kb.mozillazine.org/Browser.chrome.site_icons
        # https://blog.mozilla.org/security/2021/01/26/supercookie-protections/
        "browser.chrome.site_icons" = true; #this is enabled

        /**
         *****************************************************************************
        * SECTION: UI related                                                         *
        ******************************************************************************
        */

        # PREF: Enable insecure password warnings (login forms in non-HTTPS pages)
        # https://blog.mozilla.org/tanvi/2016/01/28/no-more-passwords-over-http-please/
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1319119
        # https://bugzilla.mozilla.org/show_bug.cgi?id=1217156
        "security.insecure_password.ui.enabled" = true;

        # PREF: Disable right-click menu manipulation via JavaScript (disabled)
        #"dom.event.contextmenu.enabled" = 		false;

        # PREF: Disable "Are you sure you want to leave this page?" popups on page close
        # https://support.mozilla.org/en-US/questions/1043508
        # NOTICE: disabling "beforeunload" events may lead to losing data entered in web forms
        # Does not prevent JS leaks of the page close event.
        # https://developer.mozilla.org/en-US/docs/Web/Events/beforeunload
        #"dom.disable_beforeunload" =     true;

        # PREF: Disable Downloading on Desktop
        # CIS 2.3.2
        "browser.download.folderList" = 2;

        # PREF: Always ask the user where to download
        # https://developer.mozilla.org/en/Download_Manager_preferences (obsolete)
        "browser.download.useDownloadDir" = false;

        # PREF: Disable the "new tab page" feature and show a blank tab instead
        # https://wiki.mozilla.org/Privacy/Reviews/New_Tab
        # https://support.mozilla.org/en-US/kb/new-tab-page-show-hide-and-customize-top-sites#w_how-do-i-turn-the-new-tab-page-off
        "browser.newtabpage.enabled" = false;
        "browser.newtab.url" = "about:blank";

        # PREF: Disable Snippets
        # https://wiki.mozilla.org/Firefox/Projects/Firefox_Start/Snippet_Service
        # https://support.mozilla.org/en-US/kb/snippets-firefox-faq
        "browser.newtabpage.activity-stream.feeds.snippets" = false;

        # PREF: Disable Activity Stream
        # https://wiki.mozilla.org/Firefox/Activity_Stream
        "browser.newtabpage.activity-stream.enabled" = false;

        # PREF: Disable new tab tile ads & preload
        # https://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
        # https://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
        # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
        # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
        # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
        # TODO: deprecated? not in DXR, some dead links
        "browser.newtabpage.enhanced" = false;
        "browser.newtab.preload" = false;
        "browser.newtabpage.directory.ping" = "";
        "browser.newtabpage.directory.source" = "data:text/plain,{}";

        # PREF: Disable Mozilla VPN ads on the about:protections page
        # https://support.mozilla.org/en-US/kb/what-mozilla-vpn-and-how-does-it-work
        # https://en.wikipedia.org/wiki/Mozilla_VPN
        # https://blog.mozilla.org/security/2021/08/31/mozilla-vpn-security-audit/
        # https://www.mozilla.org/en-US/security/advisories/mfsa2021-31/
        "browser.vpn_promo.enabled" = false;

        # PREF: Enable Auto Notification of Outdated Plugins (Firefox < 50)
        # https://wiki.mozilla.org/Firefox3.6/Plugin_Update_Awareness_Security_Review
        # CIS Version 1.2.0 October 21st, 2011 2.1.2
        # https://hg.mozilla.org/mozilla-central/rev/304560
        "plugins.update.notifyUser" = true;

        # PREF: Force Punycode for Internationalized Domain Names
        # https://kb.mozillazine.org/Network.IDN_show_punycode
        # https://www.xudongz.com/blog/2017/idn-phishing/
        # https://wiki.mozilla.org/IDN_Display_Algorithm
        # https://en.wikipedia.org/wiki/IDN_homograph_attack
        # https://www.mozilla.org/en-US/security/advisories/mfsa2017-02/
        # CIS Mozilla Firefox 24 ESR v1.0.0 - 3.6
        "network.IDN_show_punycode" = true;

        # PREF: Disable inline autocomplete in URL bar
        # https://kb.mozillazine.org/Inline_autocomplete
        "browser.urlbar.autoFill" = false;
        "browser.urlbar.autoFill.typed" = false;

        # PREF: Disable CSS :visited selectors
        # https://blog.mozilla.org/security/2010/03/31/plugging-the-css-history-leak/
        # https://dbaron.org/mozilla/visited-privacy
        "layout.css.visited_links_enabled" = false;

        # PREF: Disable URL bar autocomplete and history/bookmarks suggestions dropdown
        # https://kb.mozillazine.org/Disabling_autocomplete_-_Firefox#Firefox_3.5
        "browser.urlbar.autocomplete.enabled" = false;

        # PREF: Do not check if Firefox is the default browser
        "browser.shell.checkDefaultBrowser" = false;

        # PREF: When password manager is enabled, lock the password storage periodically
        # CIS Version 1.2.0 October 21st, 2011 2.5.3 Disable Prompting for Credential Storage
        "security.ask_for_password" = 2;

        # PREF: Lock the password storage every 1 minutes (default: 30)
        "security.password_lifetime" = 1;

        # PREF: Display a notification bar when websites offer data for offline use
        # https://kb.mozillazine.org/Browser.offline-apps.notify
        "browser.offline-apps.notify" = true;

        ####sidebar####
        "sidebar.animation.enabled" = false;
        "sidebar.backupState" = "{\"panelOpen\":false,\"launcherWidth\":50,\"launcherExpanded\":false,\"launcherVisible\":true}";
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
        ####sidebar####
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
          definedAliases = ["@s"];
        };
        "noaiduckduckgo" = {
          name = "noaiduckduckgo";
          urls = [{template = "https://noai.duckduckgo.com/?q={searchTerms}&noai=1";}];
          iconMapObj."16" = "https://noai.duckduckgo.com/favicon.ico";
          definedAliases = ["@ddg"];
        };
      };
      search.force = true;
      # search.default = "noaiduckduckgo";
      search.default = "searx";
      userChrome = ''
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
        # "network.trr.mode" = 3;
        # "network.trr.uri" = "https://all.dns.mullvad.net/dns-query";
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
        "sidebar.backupState" = "{\"panelOpen\":false,\"launcherWidth\":50,\"launcherExpanded\":false,\"launcherVisible\":true}";
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
      };
    };
  };
}
