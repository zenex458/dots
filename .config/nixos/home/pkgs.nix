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
    # yewtube
    age
    alejandra
    alsa-utils
    amdgpu_top
    anki
    aria2
    bc
    bemenu
    bfs
    bibtex-tidy
    binwalk
    burpsuite
    cliphist
    cmake-format
    cryptsetup
    cutter
    dig
    exfatprogs
    exif
    feather
    ffmpeg-full
    ffmpegthumbnailer
    ffuf
    file
    fuse3
    fzy
    gcc
    gdb
    gef
    gh
    ghidra-extensions.findcrypt
    ghidra-extensions.ghidra-golanganalyzerextension
    ghidra-extensions.ghidraninja-ghidra-scripts
    gimp3-with-plugins
    grc
    grim
    hashcat
    html-tidy
    htop
    hujsonfmt
    hunspell
    hunspellDicts.en-gb-large
    imagemagick
    imv
    inetutils
    irssi
    jetbrains.clion
    jmtpfs
    john
    jq
    libnotify
    libreoffice
    lsof
    magic-wormhole
    man-pages
    man-pages-posix
    moreutils
    mpc
    multimon-ng
    mupdf
    nautilus
    nemo-with-extensions
    netcat-gnu
    nitrokey-app2
    nixd
    nmap
    nmap
    nodePackages.bash-language-server
    nodePackages_latest.prettier
    openvpn
    p7zip
    pandoc
    pciutils
    pcsc-tools
    picard
    profanity
    pulsemixer
    python313
    python313Packages.numpy
    python313Packages.requests
    python313Packages.z3-solver
    restic
    ripgrep
    ripgrep-all
    rsync
    samba4Full
    sbctl
    seclists
    shellcheck
    shfmt
    slurp
    sonic-visualiser
    spek
    steghide
    stegseek
    swaybg
    syncthing
    tarsnap
    tenacity
    texlab
    texliveFull
    tinymist
    traceroute
    trash-cli
    tree
    typst
    typst-live
    typstyle
    unstable.signal-desktop
    unstable.simplex-chat-desktop
    unzip
    usbutils
    vesktop
    virt-viewer
    wakeonlan
    wdisplays
    wl-clip-persist
    wl-clipboard
    wl-color-picker
    wlr-randr
    wlsunset
    xdg-utils
    xmlformat
    xwayland-satellite
    yamlfmt
    yt-dlp
    zip
    zotero
    (pkgs.wordlists.override {lists = with pkgs; [rockyou seclists];})
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
