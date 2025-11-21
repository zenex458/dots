{
  inputs,
  config,
  pkgs,
  ...
}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk; #use just `emacs' if you want it the daemon to survive after the gui terminates
    #package = pkgs.emacs;
    extraPackages = epkgs:
      with pkgs.unstable.emacs.pkgs; [
        ace-window
        apheleia
        async
        auctex
        bongo
        mingus
        cape
        consult
        corfu
        diminish
        dired-subtree
        edwina
        eglot
        elfeed
        elfeed-org
        embark
        embark-consult
        expreg
        flymake
        gcmh
        golden-ratio
        haskell-mode
        hungry-delete
        indent-guide
        key-chord
        magit
        magit-delta
        markdown-mode
        multi-vterm
        multiple-cursors
        nix-ts-mode
        orderless
        org-bullets
        org-make-toc
        pdf-tools
        rainbow-delimiters
        rainbow-mode
        sudo-edit
        undo-fu
        undo-fu-session
        vertico
        vlf
        vterm
        yasnippet
        zoxide
      ];
    extraConfig = ''
      (use-package pdf-tools
          :magic ("%PDF" . pdf-view-mode)
          :hook (pdf-view-mode . pdf-view-themed-minor-mode)
          :config
            (setq pdf-info-epdfinfo-program "${pkgs.emacs.pkgs.pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-20240429.407/epdfinfo")
           (pdf-tools-install))
    '';
  };
}
