{
  pkgs,
  modulesPath,
  wrapper-manager,
  lib,
  config,
  ...
}: {
  imports = [(modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")];
  #TODO: get https://github.com/viperML/wrapper-manager working
  networking.wireless.enable = false;
  networking.networkmanager = {enable = true;};

  console = {
    useXkbConfig = true;
    font = "Lat2-Terminus16";
  };
  services = {
    xserver = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      xkb = {
        layout = "gb";
        variant = "";
        options = "altwin:ctrl_alt_win";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    emacs
    tmux
    cryptsetup
    git
    xterm
    dmenu
    alacritty
    redshift
  ];
  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
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
    preferences = {
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
}
