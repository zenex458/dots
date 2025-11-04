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
        vterm
        pdf-tools
        multi-vterm
        ace-window
        apheleia
        edwina
        evil
        async
        auctex
        cape
        consult
        corfu
        diminish
        golden-ratio
        dired-subtree
        eglot
        elfeed
        elfeed-org
        embark
        embark-consult
        expreg
        flymake
        gcmh
        hungry-delete
        haskell-mode
        key-chord
        indent-guide
        magit
        markdown-mode
        multiple-cursors
        nix-ts-mode
        orderless
        org-bullets
        org-make-toc
        rainbow-delimiters
        rainbow-mode
        sudo-edit
        undo-fu
        undo-fu-session
        vertico
        zoxide
        vlf
        yasnippet
        pipenv
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
