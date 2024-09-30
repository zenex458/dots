/**
 *   _ | _  )  _ ) \ \  / __ __| __|   __|      _ \  _ \   __|
 *     |   /   _ \  \  /     |   _|  \__ \     (   |   /  (_ |
 *    _| ___| ___/   _|     _|  ___| ____/ _) \___/ _|_\ \___|
 *                      USER-OVERRIDES.JS FOR ARKENFOX USER.JS
 */

user_pref("_user.js.parrot", "syntax error @ README"); // troubleshooting pref - do not edit

/**
 * name     : Firefox user-overrides.js supplement for arkenfox user.js
 * descrip. : this file may be appended to the 'arkenfox' user.js for Firefox
 * version  : 87r1
 * author   : 12bytes.org
 * website  : The Firefox Privacy Guide For Dummies!
 *          : https://12bytes.org/articles/tech/firefox/the-firefox-privacy-guide-for-dummies
 * website  : Firefox Configuration Guide for Privacy Freaks and Performance Buffs
 *          : http://12bytes.org/articles/tech/firefox-gecko-config-for-privacy-freaks-and-and-performance-buffs
 * code     : https://codeberg.org/12bytes.org/Firefox-user.js-supplement
 * credit   : the 'arkenfox' crew
 *          : https://github.com/arkenfox/user.js
 *
 * NOTE TO SELF: search for *TODO*
 */

/**
 * !!! IMPORTANT !!!        HOW TO WORK WITH THIS FILE         !!! IMPORTANT !!!
 * =============================================================================
 *
 * this file is an optional supplement that may be appended to the 'arkenfox'
 * user.js and used in conjunction with the 'Firefox Configuration Guide for
 * Privacy Freaks and Performance Buffs' or 'The Firefox Privacy Guide For
 * Dummies!' (https://12bytes.org/articles/tech/firefox)
 *
 * the versioning scheme for this file is 'NrN' where the first 'N' is a
 * number corresponding to the major version of Firefox for which this file is
 * intended, the 'r' stands for 'revision' and the last 'N' is the revision
 * number, so '12r3' would indicate this file is for Firefox 12.x and it is the
 * 3rd revision of the file
 *
 * preferences may be tagged with one or more of the following:
 *
 * [SET]..............the value must be checked!!!
 * [UNBREAK=(value)]..least likely to cause web breakage but more likely to compromise privacy
 * [SAFE=(value)].....a safe value
 * [PRIV=(value)].....a value which is more protective of privacy but may cause web breakage
 *
 * suggested values are marked with an asterisk (ex: [*PRIV=(value)] )
 *
 * THIS FILE CONTAINS MY PERSONAL SETTINGS, SOME OF WHICH MAY NOT WORK FOR YOU
 * AND THEREFORE IT IS IMPORTANT TO GO THROUGH IT AND MAKE THE REQUIRED CHANGES
 * - AT A MINIMUM YOU SHOULD EVALUATE ALL SECTIONS AND PREFERENCES TAGGED WITH
 * [SET]
 *
 * TO MAKE UPDATING THIS FILE EASIER, DO NOT EDIT ANY EXISTING PREFERENCES
 * EXCEPT THOSE IN THE USER CUSTOMIZATION SECTION - instead, copy the entire
 * line you want to change in this file or the 'arkenfox' user.js file to the
 * USER CUSTOMIZATION section and change the preference value there, then when
 * you update this file you can replace everything except your custom
 * preferences - to make checking for updates easy you can use the Firefox
 * Config Update Notifier (Linux):
 * https://codeberg.org/12bytes.org/Firefox-user.js-supplement/src/branch/master/misc/user.js-notify.sh
 *
 * CUSTOM PREFERENCES THAT YOU ADD AND LATER REMOVE WILL REMAIN ACTIVE IN
 * prefs.js unless they are duplicated elsewhere in this file or the 'arkenfox'
 * user.js - to reset/remove a custom preference, the suggested method is to
 * comment it out by preceding it with 2 forward slashes ( // ) and then run
 * the prefsCleaner.sh (Linux) or prefsCleaner.bat (Windows) script - make sure
 * Firefox is closed when you run the prefsCleaner script - see:
 * https://github.com/arkenfox/user.js/wiki/3.1-Resetting-Inactive-Prefs-[Scripts]
 *
 * WHEN YOU ARE FINISHED EDITING, append this file to the 'arkenfox' user.js
 * using their updater script and then run their prefsCleaner script
 *
 * YOU MUST PERFORM THE FOLLOWING INTEGRITY CHECKS IF YOU MAKE ANY CHANGES TO
 * THIS FILE (you may want to disable your network connection):
 *
 * Firefox reads the user.js file, to which this file will be appended, from top
 * to bottom and if there is a syntax error in the file, no preferences beyond
 * that point are read, thus preforming these integrity checks is crucial
 *
 * INTEGRITY CHECK 1: start Firefox and open the Browser Console from the
 * Firefox Web Developer toolbox (Ctrl+Shift+J might work) and check for any
 * error messages related to preferences - to find such errors, filter the
 * output using "user.js" - following is a sample of what you might see in the
 * console if an error is found:
 *
 * /home/[user]/.mozilla/[profile name]/user.js:[line no.]: prefs parse error: [error description]
 *
 * [line no.] will be a line number corresponding to the line in user.js where
 * the error lies - after the error is corrected, run the 'arkenfox' updater
 * script and restart Firefox
 *
 * INTEGRITY CHECK 2: load about:config in the address bar, find the
 * "_user.js.parrot" troubleshooting preference and check that its value is
 * "SUCCESS! USER SETTINGS LOADED" - if it is not then there is a syntax error
 * in which case you need to search this file for the value of the
 * "_user.js.parrot" troubleshooting preference (ex: "syntax error @ TESTING") -
 * the error will be between that point and the very next "_user.js.parrot"
 * troubleshooting preference - if you're comfortable with regular expressions
 * the following may help locate the error - this expression will highlight all/
 * most lines beginning with 'user_pref' except those containing errors:
 *
 * ^user_pref\("[a-zA-Z0-9._-]*", (?:true|false|""|\d*|"[!a-zA-Z0-9]*[ \w:/.%-@]*[a-zA-Z0-9]*"|"#[A-Z0-9]+")\);
 */

/**
 * === NEW PREFS *TODO* ===
 */
// nothing here at the moment

/**
 * === TESTING *TODO* ===
 *
 * [SET] these are preferences i'm testing or which may appear in the 'arkenfox'
 * user.js in the future
 *
 * if the value of the "_user.js.parrot" pref in about:config is
 * "syntax error @ TESTING" then there is a syntax
 * error between this point and the very next "_user.js.parrot" pref
 */
user_pref("_user.js.parrot", "syntax error @ TESTING"); // troubleshooting pref - do not edit
/**/

/**
 * === ARKENFOX DIFFS ===
 *
 * [SET] ALL OF THE PREFS IN THIS SECTION NEED TO BE CHECKED!
 *
 * these prefs are duplicates of active 'arkenfox' user.js prefs, however with
 * different values
 *
 * if the value of the "_user.js.parrot" pref in about:config is
 * "syntax error @ ARKENFOX DIFFS" then there is a syntax error
 * between this point and the very next "_user.js.parrot" pref
 */
user_pref("_user.js.parrot", "syntax error @ ARKENFOX DIFFS"); // troubleshooting pref - do not edit
/**/
// NOTICE
user_pref("browser.link.open_newwindow", 3);                        // controls when new window/tab should be opened - 1=open links that open new windows in current tab, 2=open links that open new windows in new window, 3=open links that open new windows in new tab
user_pref("browser.sessionstore.interval", 9999999);                // [UNBREAK=(default value)] interval in seconds at which session data is stored (restore session after browser crash) - '9999999' essentially disables session store to reduce disk writes - previous opened tabs will still be restored on startup (see also: 'browser.startup.page')
user_pref("browser.startup.page", 0);                               // what to load when Firefox starts - 0=a blank page, 1=your home page, 2=the last visited page, 3=restore the previous session - note that the previous session will not be restored if 'privacy.clearOnShutdown.history' is set to 'false' and this should never be set to 'true' unless you use a storage cleaner like Cookie AutoDelete
user_pref("dom.allow_cut_copy", false);                              // [UNBREAK=true] [*PRIV=false] whether to allow JavaScript to manipullate the clipboard (cut/copy) - 'false' can break some sites (d.tube copy embed code)
user_pref("dom.security.https_only_mode_send_http_background_request", false);  // [PRIV=false] whether to send HTTP requests to the server to test if it supports HTTPS if the server doesn't respond within 3 seconds
user_pref("dom.serviceWorkers.enabled", false);                      // [UNBREAK=true] [*PRIV=false] Service Workers are scripts that run in the background - disabling this will disable some/most crypto-currency miners and potentially prevent other baddies, however this may also break some websites such as Google Maps - can set to 'true' and control workers per-domain with uBlock or uMatrix
user_pref("gfx.font_rendering.opentype_svg.enabled", false);         // [UNBREAK=true] [*PRIV=false] whether to allow rendering of SVG OpenType fonts - their use is not widespread but they can be used for such things as graphs
user_pref("privacy.clearOnShutdown.cookies",  true);                // [PRIV=true] [*SAFE=false] whether to clear cookies on shutdown - as long as 'privacy.firstparty.isolate' is set to 'true' you can set this to 'false' - set to 'true' if sharing Firefox with another user
user_pref("privacy.clearOnShutdown.formdata", true);               // [*PRIV=true] whether to clear Form & Search History data on shutdown - set to 'true' if sharing Firefox with another user
user_pref("privacy.clearOnShutdown.history", true);                // [*PRIV=true] whether to clear history on shutdown - set to 'true' if sharing Firefox with another user
user_pref("privacy.cpd.cookies", true);                            // whether to select cookies when clearing storage manually
user_pref("privacy.donottrackheader.enabled", false);               // whether to send the 'do not track' HTTP header - this is essentially useless
user_pref("privacy.resistFingerprinting.letterboxing", true);      // [*PRIV=true] whether to use a generic viewport size to reduce fingerprinting entropy - the result will be that webpage content will not/may not fill the entire viewport (the part of the browser that displayes web content) - this is an important seting regarding privacy - setting to 'false' is likely to greatly increase ability of websites to fingerprint the browser
user_pref("privacy.userContext.enabled", true);                    // whether to enable Container tabs - personally i disable this in favor of FPI (privacy.firstparty.isolate)
user_pref("privacy.userContext.ui.enabled", true);                 // whether to enable the UI for Container tabs
user_pref("security.OCSP.enabled", 0);                              // [PRIV=0] [*SAFE=1] when to use OCSP fetching to confirm validity of certificates - 0=disabled, 1=enabled, 2=enabled for EV certificates only - you should typically NOT disable this
user_pref("security.ask_for_password", 0);                          // [PRIV=1] when to ask for the master password - 0=the first time it's needed, 1=every time it's needed, 2=every n minutes where n is the value of security.password_lifetime.
user_pref("security.cert_pinning.enforcement_level", 0);            // [PRIV=0] [*SAFE=(1 or 2)] whether Firefox can check which certificate authorities issued SSL certificates for the site - 0=disabled 1=allow user MiTM (such as your antivirus), 2=strict - 2 may cause key pinning (HPKP) errors; MOZILLA_PKIX_ERROR_KEY_PINNING_FAILURE
user_pref("security.insecure_connection_text.enabled", true);      // whether to display "Not Secure" label in address bar in addition to insecure icon when visiting an insecure site
user_pref("security.mixed_content.block_display_content", true);   // [UNBREAK=false] [PRIV=true] whether to allow insecure (http) static content, such as images, on secure pages (https)
user_pref("security.pki.sha1_enforcement_level", 1);                // [UNBREAK=0] [*PRIV=1] how to handle depreciated SHA-1 certificates
user_pref("security.ssl.require_safe_negotiation", true);          // [UNBREAK=false] [*PRIV=true] whether to allow connections to servers that don't support SSL
user_pref("signon.formlessCapture.enabled", false);                  // [*SAFE=false] whether password manager can capture login credentials when a proper login form is not detected

/**
 * -----------------------
 * USER CUSTOM PREFERENCES
 * -----------------------
 */

/**
 *  !!! IMPORTANT !!!  !!! IMPORTANT !!!  !!! IMPORTANT !!!  !!! IMPORTANT !!!
 *  ==========================================================================
 *
 * TO CHANGE THE VALUE OF A PREFERENCE IN THE 'ARKENFOX' USER.JS OR THE
 * --------------------------------------------------------------------
 * 'ARKENFOX DIFFS' SECTION ABOVE:
 * -------------------------------
 * 1. exit Firefox
 * 2. copy the entire preference line to the CUSTOM CODE section below
 * 3. change the pref value in the CUSTOM CODE section and save your changes
 * 4. run the 'arkenfox' updater script
 *
 * TO RESET/REMOVE/DELETE A CUSTOM PREFERENCE:
 * -------------------------------------------
 * 1. exit Firefox
 * 2. comment out the preference(s) by prefixing it with 2 forward slashes (//)
 *    and save your changes (do not move it to the DEPRECIATED/REMOVED PREFS
 *    section below)
 * 3. run the 'arkenfox' updater script
 * 4. run the 'arkenfox' prefsCleaner script
 *
 * TO FIND THE DEFAULT VALUE OF A PREFERENCE:
 * ------------------------------------------
 * find the preference in about:config and reset it
 * alternatively, https://searchfox.org/ may be of help
 */

/**
 * if the value of the "_user.js.parrot" pref in about:config is
 * "syntax error @ USER CUSTOM PREFERENCES" then there is a syntax
 * error between this point and the very next "_user.js.parrot" pref
 */
user_pref("_user.js.parrot", "syntax error @ USER CUSTOM PREFERENCES");
/**/
/**
 * YOUR CUSTOM CODE GOES BELOW THIS LINE
 * -------------------------------------
 */
user_pref("keyword.enabled", false);
user_pref("network.cookie.cookieBehavior", 2);
user_pref("dom.event.clipboardevents.enabled", false);
user_pref("media.gmp-widevinecdm.enabled", false);
user_pref("media.navigator.enabled", false);
user_pref("permissions.default.geo", 2);
user_pref("permissions.default.camera", 2);
user_pref("permissions.default.microphone", 2);
user_pref("permissions.default.desktop-notification", 2);
user_pref("permissions.default.xr", 2);
user_pref("media.peerconnection.enabled", false);
user_pref("media.videocontrols.picture-in-picture.enabled", false);
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", false);
user_pref("privacy.clearOnShutdown.cookies", true);
user_pref("places.history.enabled", false);
user_pref("signon.rememberSignons", false);
user_pref("browser.cache.memory.enable", false);
user_pref("browser.cache.memory.capacity", 0);
user_pref("permissions.memory_only", true);
user_pref("browser.sessionstore.max_tabs_undo", 0);
user_pref("browser.sessionstore.resume_from_crash", false);
user_pref("browser.urlbar.suggest.history", false);
user_pref("browser.urlbar.suggest.bookmark", true);
user_pref("browser.urlbar.suggest.openpage", true);
user_pref("browser.urlbar.suggest.topsites", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("layout.spellcheckDefault", 0);
user_pref("extensions.screenshots.disabled", true);
user_pref("identity.fxaccounts.enabled", false);
user_pref("reader.parse-on-load.enabled", false);
user_pref("javascript.options.ion", false);
user_pref("javascript.options.baselinejit", false);
user_pref("javascript.options.wasm", false);
user_pref("javascript.options.asmjs", false);
user_pref("browser.tabs.firefox-view", false);
user_pref("browser.compactmode.show", false);
user_pref("browser.uidensity", 1);

/**
 * misc. personal preferences
 */
user_pref("accessibility.tabfocus", 3);                             // which elements can be focused using the Tab key - 1=text fields, 2=all form fields except text, 4=links ony (values can be added together)
user_pref("app.update.service.enabled", false);                     // [SET] [UNBREAK=true] whether to enable Firefox update service (Windows only)
user_pref("app.update.silent", false);                              // [SET] whether to show notifications when updates are applied
user_pref("app.update.staging.enabled", false);                     // [SET] [UNBREAK=true] whether to enable Firefox update staging - *TODO* - better description
user_pref("browser.backspace_action", 2);                           // what action to take when the backspace key is pressed - 0=previous page, 1=scroll up, 2=do nothing
user_pref("browser.bookmarks.editDialog.maxRecentFolders", 12);     // how many recent folders to display when adding a bookmark
user_pref("browser.bookmarks.max_backups", 5);                      // how many backups of bookmark to keep
user_pref("browser.cache.memory.enable", true);                     // [SET] [SAFE=true] whether to enable memory cache
user_pref("browser.cache.memory.capacity", -1);                     // memory cache size (KB) see: http://kb.mozillazine.org/Browser.cache.memory.capacity
user_pref("browser.cache.offline.enable", false);                   // [PRIV=false] whether to allow off-line caching
user_pref("browser.contentblocking.report.lockwise.enabled", false);    // [SET] [SAFE=true] [*PRIV=false] whether to enable Lockwise reporting of sites visited to check if they've been breached
user_pref("browser.contentblocking.report.monitor.enabled", false);     // [SET] [UNBREAK=true] *TODO* unsure - assumend to affect reporting of blocked content
user_pref("browser.display.use_document_fonts", 0);                 // [UNBREAK=1] [*PRIV=1] whether to allow websites to use fonts they specify - 0=no, 1=yes - setting this to '0' will uglify many websites, however this value can be easily flipped per-host with the Enforce Browser Fonts add-on - WARNING: setting this to '0' may increase entropy
user_pref("browser.download.autohideButton", true);                // whether to auto-hide the Downloads button
user_pref("browser.download.folderList", 1);                        // where to save downloaded files - 0=desktop 1=downloads 2=last used
user_pref("browser.download.forbid_open_with", false);              // whether to allow the `open with` option when downloading a file
user_pref("browser.library.activity-stream.enabled", false);        // whether to enable Activity Stream recent Highlights in the Library
user_pref("browser.link.open_newwindow.override.external", 3);      // open links from external programs in: 1=the current tab, 2=a new window, 3=a new tab
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);   // disable recommended add-ons
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false); // disable recommended features
user_pref("browser.safebrowsing.allowOverride", false);             // whether to enable a prompt on safe browsing warnings
user_pref("browser.safebrowsing.blockedURIs.enabled", false);       // [SET] [SAFE=true] whether to use Mozilla's blocklist for known Flash tracking/fingerprinting - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.downloads.enabled", false);         // [SET] [SAFE=true] whether to enable 'Safe Browsing', downloads (list of sites provided by Google) - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.downloads.remote.block_dangerous", false);              // [SET] [SAFE=true] whether to block dangerous downloads - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.downloads.remote.block_dangerous_host", false);         // [SET] [SAFE=true] whether to block dangerous websites - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", false);   // [SET] [SAFE=true] whether to block potentially unwanted downloads - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", false);               // [SET] [SAFE=true] whether to block uncommon downloads - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.malware.enabled", false);           // [SET] [SAFE=true] whether to enable 'Safe Browsing', malware (list of sites provided by Google) - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.phishing.enabled", false);          // [SET] [SAFE=true] whether to enable 'Safe Browsing', phishing (list of sites provided by Google) - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("browser.safebrowsing.provider.google.gethashURL", "");   // [SET] [SAFE=(default value)] [PRIV=""] URL used to check integrity of safe browsing lists
user_pref("browser.safebrowsing.provider.google.reportURL", "");    // [SAFE=(default value)] [PRIV=""] URL where safe browsing reports are sent
user_pref("browser.safebrowsing.provider.google.updateURL", "");    // [SET] [SAFE=(default value)] [PRIV=""] URL where safe browsing lists are located
user_pref("browser.safebrowsing.provider.google4.gethashURL", "");  // [SET] [SAFE=(default value)] [PRIV=""] URL used to check integrity of safe browsing lists
user_pref("browser.safebrowsing.provider.google4.reportURL", "");   // [PRIV=""] URL where safe browsing reports are sent
user_pref("browser.safebrowsing.provider.google4.updateURL", "");   // [SET] [SAFE=(default value)] [PRIV=""] URL where safe browsing lists are located
user_pref("browser.safebrowsing.reportPhishURL", "");               // [PRIV=""] URL where safe browsing reports are sent
user_pref("browser.search.widget.inNavBar", false);                  // [SET] whether to show the search bar on the navigation bar
user_pref("browser.sessionstore.cleanup.forget_closed_after", 600); // time in seconds after which Firefox 'forgets' about closed tabs
user_pref("browser.sessionstore.interval.idle", 9999999);           // [UNBREAK=(default value)] interval in seconds at which session data is stored when browser is idle (restore session after browser crash) - '9999999' essentially disables session store to reduce disk writes - previous opened tabs will still be restored on startup
user_pref("browser.sessionstore.max_tabs_undo", 11);                 // how many tabs that can be restored when restarting the browser if session restore is enabled, as well as how many closed tabs can be re-opened while browsing
user_pref("browser.startup.homepage_override.mstone", "ignore");    // [SAFE=(default value)] whether to open a special page after a major browser update
user_pref("browser.tabs.allowTabDetach", true);                    // whether to enable the ability to 'detach' a tab by dragging it downward and having it open in a new window
user_pref("browser.tabs.closeWindowWithLastTab", false);            // whether to close the browser when the last tab is closed
user_pref("browser.tabs.loadDivertedInBackground", true);           // whether to keep Firefox in the background when loading a link from an external application
user_pref("browser.tabs.loadInBackground", true);                  // whether to focus new tabs opened from a link
user_pref("browser.tabs.warnOnClose", false);                       // whether you want to be bugged when you close multiple tabs
user_pref("browser.tabs.warnOnCloseOtherTabs", false);              // whether you want to be bugged when you close multiple tabs other than the one from which the option was invoked
user_pref("browser.tabs.warnOnOpen", false);                        // whether to warn when too many tabs are opened
user_pref("browser.triple_click_selects_paragraph", true);         // whether to select paragraphs on triple click
user_pref("browser.urlbar.autoFill", false);                         // whether to allow auto-complete of text entered in the location bar
user_pref("browser.urlbar.formatting.enabled", true);              // whether to highlight the base domain by dimming the rest of the URL
user_pref("browser.urlbar.suggest.openpage", false);                // whether to suggest currently open pages when entering text in the address bar
user_pref("devtools.aboutdebugging.showSystemAddons", true);        // whether to show system add-ons in about:debugging
user_pref("devtools.toolbox.zoomValue", "1.2");                     // size of content (fonts, etc.) in developer tool box
user_pref("dom.battery.enabled", false);                            // whether to allow websites to read battery status (dependant on JS)
user_pref("dom.push.connection.enabled", false);                    // [UNBREAK=true] [*PRIV=false] *TODO* unknown - assumed to be related to push notifications
user_pref("dom.push.userAgentID", "");                              // user-agent ID for push services
user_pref("dom.webnotifications.enabled", false);                   // [UNBREAK=true] [*PRIV=false] whether to enable web notifications
user_pref("dom.webnotifications.serviceworker.enabled", false);     // [UNBREAK=true] [*PRIV=false] whether to enable background web notifications
user_pref("extensions.pocket.enabled", false);                      // [SET] [*PRIV=false] set to 'true' if you use the Pocket service, a "save for later" cloud service
user_pref("extensions.screenshots.disabled", true);                 // [SET] [PRIV=true] disable the screenshots system add-on
user_pref("extensions.screenshots.upload-disabled", true);          // [SET] [*PRIV=true] disable screenshots uploading
user_pref("extensions.update.autoUpdateDefault", false);            // [SET] [*PRIV=false] whether to allow automatic installation of updated add-ons - i HIGHLY recommend setting this to 'false' and reading all change logs, etc., before installing add-on updates
user_pref("extensions.update.enabled", false);                      // [SET] [*SAFE=true] [UNBREAK=true] whether to enable automatic checking (not installation) for extension updates
user_pref("extensions.webextensions.restrictedDomains", "");        // [*UNBREAK=(default value)] [PRIV=""] a list of domains where WebExtensions (add-ons) are not allowed to run
user_pref("extensions.webextensions.userScripts.enabled", true);    // whether to enable the WebExtension API for user scripts (see: https://wiki.mozilla.org/WebExtensions/UserScripts)
user_pref("findbar.highlightAll", true);                            // whether to highlight all instances of search terms entered in the Find Bar
user_pref("font.name.serif.x-unicode", "Bitstream Vera Sans");      // font preference
user_pref("font.name.serif.x-western", "Bitstream Vera Sans");      // font preference
user_pref("full-screen-api.warning.delay", 0);                      // how long wait before displaying full screen warning
user_pref("full-screen-api.warning.timeout", 0);                    // how long to display a warning when a page enters full-screen mode
user_pref("general.autoScroll", false);                             // whether to enable auto-scrolling - WARNING: see note in 'arkenfox' user.js about this pref
user_pref("identity.fxaccounts.enabled", false);                    // [SET] [UNBREAK=true] [*PRIV=false] whether to enable Firefox Accounts and Sync - if you want to sync browser data between devices, consider using a self-hosted solution like NextCloud
user_pref("image.animation_mode", "normal");                          // [SET] how to display animated GIF images - none=do not animate, once=play animation once, normal=play the animation normally
user_pref("layout.css.font-loading-api.enabled", false);             // [UNBREAK=true] [*PRIV=false] whether to enable CSS Font Loading API
user_pref("layout.css.scrollbar-color.enabled", true);             // whether to allow web pages to style scroll bars
user_pref("layout.css.scrollbar-width.enabled", true);             // whether to allow web pages to style scroll bars
user_pref("layout.spellcheckDefault", 0);                           // what to spell-check - 0=disabled, 1=enable for multi-line text controls, 2=enable for single and multi-line text controls
user_pref("mathml.disabled", true);                                 // [UNBREAK=false] [*PRIV=true] mathematical markup language - suggested to disable because of security concerns
user_pref("media.autoplay.default", 5);                             // [SET] media playback - 0=allow all, 1=block non-muted, 2=prompt (removed in FF66), 5=block all (FF69+)
user_pref("media.videocontrols.picture-in-picture.enabled", false); // [SET] whether to enable picture-in-picture functionality for video
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", false); // [SET] whether to enable picture-in-picture control for video
user_pref("middlemouse.paste", true);                               // whether to allow pasting with middle mouse button (Linux) - WARNING: do NOT set to 'true' if 'general.autoScroll' is enabled
user_pref("mousewheel.with_shift.action", 3);                       // what to do when Shift key is used with the mouse wheel - 0=do nothing, 1=scroll contents, 2=go back or forward in history, 3=zoom contents in or out
user_pref("network.manage-offline-status", false);                  // whether to auto-enter work off-line mode if network drops
user_pref("network.cookie.lifetimePolicy", 2);                      // how long to keep cookies - 0=until they expire, 2=until browser close
user_pref("network.trr.mode", 3);                                   // [SET] [UNBREAK=(default value)] whether to enable Trusted Recursive Resolver (DNS over HTTPS) - 0=standard mode, 1=let Firefox choose fastest method, 2=encrypted DNS with unencrypted fallback, 3=encrypted DNS only, 4=for testing, 5=essentially same as '0' - it is strongly suggested to encrypt DNS lookups by setting this to 2 or 3 unless you are encrypting DNS another way or using an encrypted VPN which also offers DNS - IMPORTANT! enabling this alone does not provide DNS encryption, see: https://tinyurl.com/ycp3cbbp
user_pref("nglayout.enable_drag_images", false);                    // whether to allow image dragging - also seems to have an effect on highlighting and dragging text - this feature can be very annoying
user_pref("privacy.trackingprotection.cryptomining.enabled", true); // [SET] [*SAFE=true] whether to enable cryptomining protection - this is probably better handled by uBlock and appropiate filter lists
user_pref("privacy.trackingprotection.enabled", true);             // [SET] [*SAFE=true] whether to enable tracking protection - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("privacy.trackingprotection.pbmode.enabled", true);      // [SET] [*SAFW=true] whether to enable tracking protection in Private Browsing mode - can set to 'false' if using uBlock with appropriate lists enabled
user_pref("reader.parse-on-load.enabled", false);                   // whether to create the Reader View version of page when page is first loaded
user_pref("security.osclientcerts.autoload", true);                 // [SET] whether to use security certificates provided by the OS (Windows, Mac - Linux support is unknown at this time)
user_pref("security.tls.version.min", 1);                           // [SET] [*SAFE=(default value)] set the minimum version of TLS for encrypted sites using it
user_pref("signon.generation.enabled", false);                      // [*SAFE=true] whether to suggest and generate strong passwords
user_pref("signon.management.page.breach-alerts.enabled", false);   // [SET] [*SAFE=true] [PRIV=false] whether to display an alert when you visit a website for which log-on credentials are stored which has been previously breached
user_pref("startup.homepage_override_url", "");                     // [PRIV=""] 'What's New' page after browser update
user_pref("startup.homepage_welcome_url", "");                      // [PRIV=""] 'Welcome' URL
user_pref("startup.homepage_welcome_url.additional", "");           // [PRIV=""] 'Welcome' URL, additional
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true); // (ff 69+) whether to allow styling chrome with userChrome.css
user_pref("ui.caretWidth", 2);                                      // width of the blinking caret in edit controls
user_pref("ui.popup.disable_autohide", false);                      // (for developers) useful if you forget to disable 'Disable Popup Auto-Hide' option in Browser Toolbox
user_pref("view_source.wrap_long_lines", true);                     // whether to wrap long lines when viewing page source
user_pref("widget.disable-dark-scrollbar", false);                   // whether to disable dynamically colored scroll bars

/**
 * [SET] the following preferences adjusts the smooth scrolling feature of
 * Firefox when using a mouse wheel or keyboard keys to scroll
 */
user_pref("general.smoothScroll.lines.durationMaxMS", 400);         // smooth the start/end of line scrolling operations in ms (up/down arrow/page keys)
user_pref("general.smoothScroll.lines.durationMinMS", 200);         // smooth the start/end of line scrolling operations in ms (up/down arrow/page keys)
user_pref("general.smoothScroll.mouseWheel.durationMaxMS", 600);    // smooth the start/end of scrolling operations in ms
user_pref("general.smoothScroll.mouseWheel.durationMinMS", 300);    // smooth the start/end of scrolling operations in ms
user_pref("general.smoothScroll.other.durationMaxMS", 400);         // smooth the start/end of other scrolling operations in ms
user_pref("general.smoothScroll.other.durationMinMS", 200);         // smooth the start/end of other scrolling operations in ms
user_pref("general.smoothScroll.pages.durationMaxMS", 400);         // smooth the start/end of page scrolling operations in ms (PgUp/PgDn keys)
user_pref("general.smoothScroll.pages.durationMinMS", 200);         // smooth the start/end of page scrolling operations in ms (PgUp/PgDn keys)
user_pref("mousewheel.acceleration.factor", 10);                    // sets acceleration factor if mouse wheel.acceleration.start > -1
user_pref("mousewheel.acceleration.start", 0);                      // when to apply mouse wheel.acceleration.factor (after how many scroll clicks of mouse wheel) - value must be greater than -1
user_pref("mousewheel.default.delta_multiplier_x", 85);             // sets the x-axis step size
user_pref("mousewheel.default.delta_multiplier_y", 85);             // sets the y-axis step size
user_pref("mousewheel.default.delta_multiplier_z", 85);             // sets the z-axis step size
user_pref("mousewheel.min_line_scroll_amount", 10);                 // if the CSS line height is smaller than this value in pixels, each scroll click will scroll this amount

/*
 * -------------------------------------
 * YOUR CUSTOM CODE GOES ABOVE THIS LINE
 */

/**
 * === DEPRECATED/REMOVED/RESET ===
 *
 * DO NOT EDIT THIS SECTION - prefs here are used to reset values when running
 * the 'arkenfox' prefsCleaner script
 */
// user_pref("browser.urlbar.clickSelectsAll", );
// user_pref("browser.urlbar.doubleClickSelectsAll", );
// user_pref("clipboard.plainTextOnly", );
// user_pref("dom.forms.datetime", );
// user_pref("dom.push.serverURL", );
// user_pref("extensions.content_script_csp.enabled", );
// user_pref("extensions.content_script_csp.report_only", );
// user_pref("intl.locale.requested", );
// user_pref("layout.css.visited_links_enabled", );
// user_pref("network.connectivity-service.DNSv4.domain", );
// user_pref("network.connectivity-service.DNSv6.domain", );
// user_pref("network.connectivity-service.IPv4.url", );
// user_pref("network.connectivity-service.IPv6.url", );
// user_pref("network.dnsCacheExpiration", );
// user_pref("network.dnsCacheExpirationGracePeriod", );
// user_pref("network.http.referer.defaultPolicy", );
// user_pref("privacy.spoof_english", );
// user_pref("signon.showAutoCompleteFooter", );
// user_pref("webgl.dxgl.enabled", );
// user_pref("widget.content.gtk-theme-override", );
// user_pref("gfx.webrender.all", );
// user_pref("gfx.webrender.software", );
// user_pref("gfx.webrender.enabled", );
// user_pref("gfx.webrender.compositor", );

/*
 * all depreciated prefs from arkenfox up to, but not including, the current
 * Firefox version (current version depreciated prefs are in the current
 * user.js)
 */
// user_pref("security.ssl.errorReporting.automatic", false);
// user_pref("security.ssl.errorReporting.enabled", false);
// user_pref("security.ssl.errorReporting.url", "");
// user_pref("browser.download.hide_plugins_without_extensions", false);
// user_pref("browser.search.geoSpecificDefaults", false);
// user_pref("browser.search.geoSpecificDefaults.url", "");
// user_pref("intl.charset.fallback.override", "windows-1252");
// user_pref("media.autoplay.enabled.user-gestures-needed", false);
// user_pref("toolkit.cosmeticAnimations.enabled", false);
// user_pref("browser.urlbar.oneOffSearches", false);
// user_pref("browser.tabs.remote.allowLinkedWebInFileUriProcess", false);
// user_pref("extensions.blocklist.url", "https://blocklists.settings.services.mozilla.com/v1/blocklist/3/%APP_ID%/%APP_VERSION%/");
// user_pref("dom.disable_window_open_feature.close", true);
// user_pref("dom.disable_window_open_feature.location", true)
// user_pref("dom.disable_window_open_feature.menubar", true);
// user_pref("dom.disable_window_open_feature.minimizable", true);
// user_pref("dom.disable_window_open_feature.personalbar", true)
// user_pref("dom.disable_window_open_feature.resizable", true)
// user_pref("dom.disable_window_open_feature.status", true)
// user_pref("dom.disable_window_open_feature.titlebar", true);
// user_pref("dom.disable_window_open_feature.toolbar", true);
// user_pref("geo.wifi.uri", "blah blah");
// user_pref("geo.wifi.logging.enabled", true)
// user_pref("privacy.userContext.longPressBehavior", 2);
// user_pref("webgl.disable-extensions", true);
// user_pref("browser.newtabpage.activity-stream.telemetry.ping.endpoint", "");
// user_pref("toolkit.telemetry.hybridContent.enabled", false);
// user_pref("dom.indexedDB.enabled", true);
// user_pref("devtools.webide.enabled", false);
// user_pref("devtools.webide.autoinstallADBExtension", false);
// user_pref("offline-apps.allow_by_default", false);
// user_pref("gfx.downloadable_fonts.woff2.enabled", false);
// user_pref("plugins.click_to_play", true);
// user_pref("media.autoplay.allow-muted", false);
// user_pref("browser.aboutHomeSnippets.updateUrl", "");
// user_pref("browser.newtabpage.activity-stream.disableSnippets", true);
// user_pref("lightweightThemes.update.enabled", false);
// user_pref("security.csp.experimentalEnabled", true);
// user_pref("browser.cache.disk.smart_size.first_run", false);
// user_pref("dom.event.highrestimestamp.enabled", true);
// user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr", false);
// user_pref("browser.chrome.errorReporter.enabled", false);
// user_pref("browser.chrome.errorReporter.submitUrl", "");
// user_pref("network.allow-experiments", false);
// user_pref("browser.urlbar.autocomplete.enabled", false);
// user_pref("browser.fixup.hide_user_pass", true)
// user_pref("browser.onboarding.enabled", false);
// user_pref("devtools.webide.autoinstallADBHelper", false)
// user_pref("devtools.webide.adbAddonURL", "")
// user_pref("security.csp.enable_violation_events", false);
// user_pref("browser.search.countryCode", "US")
// user_pref("app.update.enabled", false);
// user_pref("services.blocklist.update_enabled", true)
// user_pref("shield.savant.enabled", false);
// user_pref("browser.chrome.favicons", false);
// user_pref("media.autoplay.enabled", false);
// user_pref("network.cookie.lifetime.days", 90);
// user_pref("browser.ctrlTab.previews", true);
// user_pref("plugin.state.java", 0);
// user_pref("services.blocklist.signing.enforced", true);
// user_pref("experiments.enabled", false);
// user_pref("experiments.manifest.uri", "");
// user_pref("experiments.supported", false);
// user_pref("experiments.activeExperiment", false);
// user_pref("network.jar.block-remote-files", true);
// user_pref("network.jar.open-unsafe-types", false);
// user_pref("browser.storageManager.enabled", false);
// user_pref("browser.newtabpage.directory.source", "data:text/plain,");
// user_pref("browser.newtabpage.enhanced", false);
// user_pref("browser.newtabpage.introShown", true);
// user_pref("extensions.shield-recipe-client.enabled", false);
// user_pref("extensions.shield-recipe-client.api_url", "");
// user_pref("browser.newtabpage.activity-stream.enabled", false);
// user_pref("dom.workers.enabled", false);
// user_pref("view_source.tab", false);
// user_pref("intl.locale.matchOS", false);
// user_pref("general.useragent.locale", "en-US");
// user_pref("datareporting.healthreport.about.reportUrl", "data:,");
// user_pref("dom.flyweb.enabled", false);
// user_pref("browser.cache.frecency_experiment", -1);
// user_pref("security.mixed_content.use_hsts", true);
// user_pref("security.mixed_content.send_hsts_priming", false);
// user_pref("network.http.referer.userControlPolicy", 3);
// user_pref("security.xpconnect.plugin.unrestricted", false);
// user_pref("media.getusermedia.screensharing.allowed_domains", "");
// user_pref("dom.disable_window_status_change", true);
// user_pref("dom.idle-observers-api.enabled", false);
// user_pref("browser.crashReports.unsubmittedCheck.autoSubmit", false)
// user_pref("extensions.e10sBlocksEnabling", false);
// user_pref("social.share.activationPanelEnabled", false);
// user_pref("social.shareDirectory", "");
// user_pref("social.whitelist", "");
// user_pref("social.toast-notifications.enabled", false);
// user_pref("social.remote-install.enabled", false);
// user_pref("social.directories", "");
// user_pref("social.enabled", false)
// user_pref("media.eme.chromium-api.enabled", false)
// user_pref("devtools.webide.autoinstallFxdtAdapters", false);
// user_pref("devtools.webide.adaptersAddonURL", "");
// user_pref("browser.casting.enabled", false);
// user_pref("browser.bookmarks.showRecentlyBookmarked", false);
// user_pref("extensions.screenshots.system-disabled", true)
// user_pref("extensions.formautofill.experimental", false)
// user_pref("geo.security.allowinsecure", false);
// user_pref("browser.selfsupport.url", "");
// user_pref("browser.selfsupport.enabled", false)
// user_pref("browser.newtabpage.directory.ping", "data:text/plain,");
// user_pref("browser.formfill.saveHttpsForms", false);
// user_pref("browser.formautofill.enabled", false);
// user_pref("dom.enable_user_timing", false);
// user_pref("dom.keyboardevent.code.enabled", false);
// user_pref("browser.tabs.animate", false);
// user_pref("browser.fullscreen.animate", false);
// user_pref("browser.safebrowsing.reportMalwareMistakeURL", "")
// user_pref("browser.safebrowsing.reportPhishMistakeURL", "")
// user_pref("media.eme.apiVisible", false);
// user_pref("dom.archivereader.enabled", false);
// user_pref("security.tls.unrestricted_rc4_fallback", false);
// user_pref("plugin.scan.Acrobat", "99999");
// user_pref("plugin.scan.Quicktime", "99999");
// user_pref("plugin.scan.WindowsMediaPlayer", "99999");
// user_pref("media.getusermedia.screensharing.allow_on_old_platforms", false);
// user_pref("dom.beforeAfterKeyboardEvent.enabled", false);
// user_pref("network.http.sendSecureXSiteReferrer", false);
// user_pref("media.gmp-eme-adobe.enabled", false);
// user_pref("media.gmp-eme-adobe.visible", false);
// user_pref("media.gmp-eme-adobe.autoupdate", false);
// user_pref("dom.telephony.enabled", false);
// user_pref("media.block-play-until-visible", true);
// user_pref("dom.vr.oculus050.enabled", false);
// user_pref("network.http.spdy.enabled.v3-1", false);
// user_pref("browser.usedOnWindows10.introURL", "");
// user_pref("plugins.update.notifyUser", false);
// user_pref("browser.safebrowsing.enabled", false);
// user_pref("security.ssl3.ecdhe_rsa_rc4_128_sha", false);
// user_pref("security.ssl3.ecdhe_ecdsa_rc4_128_sha", false);
// user_pref("security.ssl3.rsa_rc4_128_sha", false);
// user_pref("security.ssl3.rsa_rc4_128_md5", false);
// user_pref("plugins.update.url", "");
// user_pref("loop.enabled", false);
// user_pref("loop.server", "");
// user_pref("loop.feedback.formURL", "");
// user_pref("loop.feedback.manualFormURL", "");
// user_pref("loop.facebook.appId", "");
// user_pref("loop.facebook.enabled", false);
// user_pref("loop.facebook.fallbackUrl", "");
// user_pref("loop.facebook.shareUrl", "");
// user_pref("loop.logDomains", false);
// user_pref("dom.disable_window_open_feature.scrollbars", true);
// user_pref("dom.push.udp.wakeupEnabled", false);
// user_pref("browser.urlbar.unifiedcomplete", false);
// user_pref("toolkit.telemetry.unifiedIsOptIn", true)
// user_pref("datareporting.healthreport.about.reportUrlUnified", "data:text/plain,");
// user_pref("browser.history.allowPopState", false);
// user_pref("browser.history.allowPushState", false);
// user_pref("browser.history.allowReplaceState", false);
// user_pref("datareporting.healthreport.service.enabled", false)
// user_pref("datareporting.healthreport.documentServerURI", "")
// user_pref("datareporting.policy.dataSubmissionEnabled.v2", false);
// user_pref("browser.pocket.enabled", false);
// user_pref("browser.pocket.api", "");
// user_pref("browser.pocket.site", "");
// user_pref("browser.pocket.oAuthConsumerKey", "");
// user_pref("browser.safebrowsing.appRepURL", "")
// user_pref("browser.polaris.enabled", false);
// user_pref("browser.sessionstore.privacy_level_deferred", 2);
// user_pref("browser.safebrowsing.provider.google.appRepURL", "")
// user_pref("security.tls.insecure_fallback_hosts.use_static_list", false);
// user_pref("dom.workers.sharedWorkers.enabled", false);
// user_pref("dom.disable_image_src_set", true);
// user_pref("browser.safebrowsing.gethashURL", "")
// user_pref("browser.safebrowsing.updateURL", "")
// user_pref("browser.safebrowsing.malware.reportURL", "")
// user_pref("browser.trackingprotection.gethashURL", "")
// user_pref("browser.trackingprotection.updateURL", "")
// user_pref("pfs.datasource.url", "");
// user_pref("browser.search.showOneOffButtons", false);
// user_pref("privacy.clearOnShutdown.passwords", false);
// user_pref("full-screen-api.approval-required", false);
// user_pref("browser.safebrowsing.reportErrorURL", "")
// user_pref("browser.safebrowsing.reportGenericURL", "")
// user_pref("browser.safebrowsing.reportMalwareErrorURL", "")
// user_pref("browser.safebrowsing.reportMalwareURL", "")
// user_pref("browser.safebrowsing.reportURL", "")
// user_pref("plugins.enumerable_names", "");
// user_pref("network.http.spdy.enabled.http2draft", false);
// user_pref("camera.control.autofocus_moving_callback.enabled", false);
// user_pref("privacy.donottrackheader.value", 1);
// user_pref("network.websocket.enabled", false);
// user_pref("dom.network.enabled", false);
// user_pref("pageThumbs.enabled", false);
// user_pref("dom.mozTCPSocket.enabled", false);

/**
 *  !!! IMPORTANT !!!  !!! IMPORTANT !!!  !!! IMPORTANT !!!  !!! IMPORTANT !!!
 *  ==========================================================================
 *
 * below is the "_user.js.parrot" preference you must check in about:config -
 * if the value is "SUCCESS! USER SETTINGS LOADED" then there was no syntax
 * error above this point
 */
user_pref("_user.js.parrot", "SUCCESS! USER SETTINGS LOADED"); // troubleshooting pref - do not edit
