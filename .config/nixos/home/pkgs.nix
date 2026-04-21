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
    # kismet
    # macchanger
    # mpvScripts.mpris
    # pynitrokey
    # rlwrap # for the readline
    # sigrok-cli
    # yewtube
    # moreutils
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
    dig
    desmume
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
    unstable.ghidra-extensions.findcrypt
    unstable.ghidra-extensions.ghidra-golanganalyzerextension
    unstable.ghidra-extensions.ghidraninja-ghidra-scripts
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
    imhex
    imv
    inetutils
    irssi
    jmtpfs
    john
    jq
    libnotify
    libreoffice-fresh
    lsof
    magic-wormhole
    man-pages
    man-pages-posix
    mpc
    multimon-ng
    mupdf
    nautilus
    nemo-with-extensions
    netcat-gnu
    nitrokey-app2
    nixd
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
    pwntools
    unstable.binaryninja-free
    python313
    python313Packages.ipython
    python313Packages.z3-solver
    python313Packages.pygments
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
    smartmontools
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
    unstable.kmscon
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
    wayscriber
    kdePackages.polkit-kde-agent-1
    (unstable.cutter.withPlugins (p: with p; [rz-ghidra jsdec sigdb]))
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
