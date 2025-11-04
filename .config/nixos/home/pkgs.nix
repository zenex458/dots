{pkgs, ...}: {
  home.packages = with pkgs; [
    # bsdgames
    # entr # run a command when files change
    # ffmpegthumbnailer
    # fq # jq for binary formats
    # gron # json grepper
    # https://viric.name/soft/ts/
    # https://www.gnu.org/software/parallel
    # imhex
    # mpvScripts.mpris
    # kismet
    # macchanger
    # rlwrap # for the readline
    # sigrok-cli
    # android-tools
    # gojq
    # wl-color-picker
    #ciscoPacketTracer8
    #yewtube
    age
    alacritty
    alejandra
    alsa-utils
    amdgpu_top
    anki-bin
    aria2
    astyle
    basedpyright
    bc
    bemenu
    bfs
    ccls
    cliphist
    cryptsetup
    cutter
    dig
    exfatprogs
    exif
    feather
    ffmpeg-full
    file
    fuse3
    fzy
    gcc
    gdb
    gh
    gimp3-with-plugins
    gns3-gui #an alternative to packettracer
    gnumake
    grim
    html-tidy
    htop
    hunspell
    hunspellDicts.en-gb-large
    imagemagick
    imv
    irssi
    jq
    keepassxc
    libnotify
    libreoffice
    lsof
    magic-wormhole
    man-pages
    man-pages-posix
    moreutils
    mpc-cli
    mpv
    mupdf
    nautilus
    nitrokey-app2
    nixd
    nodePackages.bash-language-server
    p7zip
    pandoc
    pciutils
    pcsc-tools
    pipenv
    pulsemixer
    # pynitrokey
    python3Full
    restic
    ripgrep
    ripgrep-all
    rsync
    ruff
    samba4Full
    sbctl
    shellcheck
    shfmt
    signal-desktop
    slurp
    syncthing
    tarsnap
    tcpdump
    texlab
    texliveFull
    traceroute
    trash-cli
    tree
    unstable.simplex-chat-desktop
    xwayland-satellite
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
    yamlfmt
    yt-dlp
    zip
    zotero
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
