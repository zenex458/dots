{ pkgs, ... }: {
  home.packages = with pkgs; [
    # android-tools
    # bsdgames
    # entr # run a command when files change
    # fq # jq for binary formats
    # gojq
    # gron # json grepper
    # https://viric.name/soft/ts/
    # https://www.gnu.org/software/parallel
    # kismet
    # macchanger
    # mpvScripts.mpris
    # pynitrokey
    # rlwrap # for the readline
    # sigrok-cli
    # yewtube
    # moreutils
    age
    # alsa-utils
    amdgpu_top
    anki
    aria2
    bc
    bemenu
    brightnessctl
    bfs
    bibtex-tidy
    binwalk
    burpsuite
    cliphist
    cmake-format
    cryptsetup
    dig
    exfatprogs
    exif
    feather
    ffmpeg-full
    ffmpegthumbnailer
    ffuf
    file
    fq
    fuse3
    fzy
    gcc
    gdb
    gef
    gh
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
    jmtpfs
    john
    jq
    libnotify
    lsof
    magic-wormhole
    man-pages
    man-pages-posix
    mpc
    multimon-ng
    mupdf
    nautilus
    netcat-gnu
    nitrokey-app2
    nixd
    nixfmt
    nmap
    bash-language-server
    prettier
    openvpn
    p7zip
    pandoc
    pciutils
    pcsc-tools
    picard
    profanity
    pulsemixer
    pwntools
    python313
    python313Packages.ipython
    python313Packages.pygments
    python313Packages.z3-solver
    restic
    ripgrep
    ripgrep-all
    rsync
    sbctl
    seclists
    shellcheck
    shfmt
    slurp
    smartmontools
    sonic-visualiser
    spek
    steghide
    stegseek
    swaybg
    syncthing
    tailspin
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
    unzip
    usbutils
    virt-viewer
    wakeonlan
    wayscriber
    qemu
    wdisplays
    wireguard-tools
    wl-clip-persist
    wl-clipboard
    wl-color-picker
    pre-commit
    wlr-randr
    wlsunset
    xdg-utils
    xmlformat
    xwayland-satellite
    yamlfmt
    zip
    zotero
    go
    terraform_1
    forgejo-cli
    ghidra-extensions.findcrypt
    ghidra-extensions.ghidra-golanganalyzerextension
    ghidra-extensions.ghidraninja-ghidra-scripts
    binaryninja-free
    gurk-rs
    nchat
    #signal-desktop
    rustdesk-flutter
    # kmscon
    yt-dlp
    libreoffice-fresh
    androidenv.androidPkgs.platform-tools
    (discord.override {
      withOpenASAR = true;
      withEquicord = true;
    })
    (cutter.withPlugins (
      p: with p; [
        rz-ghidra
        jsdec
        sigdb
      ]
    ))
    (wordlists.override {
      lists = with pkgs; [
        rockyou
        seclists
      ];
    })
    (aspellWithDicts (
      dicts: with dicts; [
        en
        en-computers
        en-science
      ]
    ))
  ];
}
