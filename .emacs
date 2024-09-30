(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("032882f74149876e565578ca5b1f3b7bd36c3e7f301dd796e148b7ca1ff3768d" "8ef5ff996c2b6ef9c55c126096cc730a101dbe5bc158123e284b16012f03e678" "362a0eb4788dfe2cfd9de6fa9ef577757438593a353a399dce77fab76165e4c1" "21cdf4e6d2a427366a03f2301197a926a5353092c3b578240c677c4060882b50" "73b77444f90e5b54f228e6995f749e33b5d15c9893ac31805c0e52378fd9b1d6" "6e10eef95f0fe96866dd7acfcab2d86519c7aa97fd18d945b8490f54dde5ad0a" "410b602bd104a30a6e4ee7cd562538d5adfc0e42feb58ac4667816352fb147d9" "74b5d0e408ad6bcc8499ff58376210605fd939ff805c3c3d3c036a6e78cb4570" "ddde64e799e24ca5fe6ec409b88843452db4e1d6698795e3434fd8d6dd383a13" "a77735fe0193d57476298d982de95c51f1625da7aa4a07473be8143cf3326dc2" default))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(lsp-treemacs lsp-ui lsp-mode evil async pretty-mode company-shell slime-company slime company flycheck yasnippet-snippets yasnippet rainbow-delimiters rainbow-mode use-package))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)   

(global-subword-mode 1)

(defun efs/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Hack Nerd Font" :height 98)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height 98)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height 98))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                ;; (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (efs/set-font-faces))))
    (efs/set-font-faces))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'nyx)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (load-file (expand-file-name "~/.emacs")))
(global-set-key (kbd "C-c r") 'config-reload)


(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))
(electric-pair-mode t)

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-center-content t)
(setq dashboard-items '((recents)))


 (require 'evil)
  (evil-mode 1)
