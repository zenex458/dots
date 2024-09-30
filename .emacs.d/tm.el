(setq gc-cons-threshold 20000000) ;take of a zero if runtime is slow
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 110)

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

(use-package nyx-theme
  :ensure t
  :config
  (enable-theme 'nyx))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode 1)
(display-battery-mode 1)
(size-indication-mode 1)
(save-place-mode 1)

(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.emacs.d/saves/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/saves" t)))

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


(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(setq confirm-kill-emacs 'y-or-n-p)
