{pkgs, ...}: {
  home.packages = with pkgs; [
    # android-tools
    # bsdgames
    # entr # run a command when files change
    # fq # jq for binary formats
    # gojq
    # gron # json grepper
    # https://viric.name/soft/ts/
    # https://www.gnu.org/software/parallel
    # imhex
    # kismet
    # macchanger
    # mpvScripts.mpris
    # pynitrokey
    # rlwrap # for the readline
    # sigrok-cli
    # wl-color-picker
    # yewtube
    age
    anki
    alejandra
    alsa-utils
    amdgpu_top
    aria2
    nautilus
    bc
    bemenu
    bfs
    cliphist
    cryptsetup
    dig
    exfatprogs
    exif
    feather
    ffmpeg-full
    ffmpegthumbnailer
    file
    fuse3
    fzy
    gh
    gimp3-with-plugins
    grc
    grim
    html-tidy
    htop
    hunspell
    hunspellDicts.en-gb-large
    imagemagick
    imv
    jmtpfs
    irssi
    jq
    libnotify
    libreoffice
    lsof
    magic-wormhole
    man-pages
    wakeonlan
    man-pages-posix
    moreutils
    mpc
    mupdf
    nemo-with-extensions
    nitrokey-app2
    nixd
    nodePackages.bash-language-server
    nodePackages_latest.prettier
    p7zip
    pandoc
    pciutils
    pcsc-tools
    pulsemixer
    restic
    ripgrep
    ripgrep-all
    rsync
    samba4Full
    sbctl
    shellcheck
    shfmt
    unstable.signal-desktop
    slurp
    swaybg
    syncthing
    tarsnap
    typst
    typstyle
    typst-live
    texlab
    texliveFull
    bibtex-tidy
    traceroute
    trash-cli
    tree
    unstable.simplex-chat-desktop
    unzip
    usbutils
    vesktop
    virt-viewer
    wdisplays
    wl-clip-persist
    wl-clipboard
    wlr-randr
    wlsunset
    xdg-utils
    xmlformat
    tinymist
    xwayland-satellite
    yamlfmt
    yt-dlp
    zip
    hujsonfmt
    zotero
    seclists
    cutter
    jetbrains.clion
    cmake-format
    wl-color-picker
    picard
    (aspellWithDicts (
      dicts:
        with dicts; [
          en
          en-computers
          en-science
        ]
    ))
  ];
}
