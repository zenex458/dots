;;; init.el --- Config
;;; Commentary:
;;; Code:

;;(setq gc-cons-threshold 20000000) ;take of a zero if runtime is slow
(setq gc-cons-threshold 2000000)
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


(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))


;;(setq use-packagenn-compute-statistics t) ;then do (use-package-report)

;;(use-package nyx-theme
;;:ensure t
;;:config
;; (enable-theme 'nyx))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'morest t)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 12)
(menu-bar-mode -1)
;;(column-number-mode 1)
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

(defun config-reload ()
  "This will load my Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'config-reload)


;;smartparens
;;paredit
;;(defvar electric-pair-pairs '(
;;                              (?\{ . ?\})
;;                              (?\( . ?\))
;;                              (?\[ . ?\])
;;                              (?\" . ?\")
;;                              ))
;;
;;(electric-pair-mode t)


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
;;(setq ring-bell-function 'ignore)

(defvar display-time-24hr-format t)
(display-time-mode 1)

(setq scroll-conservatively 100)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(defun split-and-follow-horizontally ()
  "This will split horizontally and focus will follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "This split vertically and focus will follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-current-buffer ()
  "This will kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;;(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c b") 'counsel-switch-buffer)

;;(use-package avy ;;switch windows
;;  :ensure t
;;  :bind
;;    ("M-s" . avy-goto-char))

(defun config-visit ()
  "Vist Emacs config."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun edit-todo ()
  "Vist Org-mode Todo list."
  (interactive)
  (find-file "~/Documents/2. Notes/Org/todo.org"))
(global-set-key (kbd "C-c t") 'edit-todo)


(global-set-key (kbd "C-c c") 'flyspell-buffer)
(global-set-key (kbd "C-c s") 'ispell)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

;;(with-eval-after-load 'company
;;  (define-key company-active-map (kbd "M-n") nil)
;;  (define-key company-active-map (kbd "M-p") nil)
;;  (define-key company-active-map (kbd "C-n") #'company-select-next)
;;  (define-key company-active-map (kbd "C-p") #'company-select-previous)
;;  (define-key company-active-map (kbd "SPC") #'company-abort))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :init
    (require 'company)
    (slime-setup '(slime-fancy slime-company)))

;(add-hook 'shell-mode-hook 'flycheck-mode)
;(add-hook 'shell-mode-hook 'company-mode)

(use-package yasnippet
  :init
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))


(setq-default mode-line-format
	          '("%e" mode-line-front-space mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position (vc-mode vc-mode) " " mode-line-misc-info mode-line-end-spaces))


(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  (define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete))

(use-package counsel
  :init
  :ensure t
  :config
  (counsel-mode 1))

(use-package ivy
  :init
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))


(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(ido-mode t)

(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-start-on-weekday t
      org-log-done 'time
      org-enforce-todo-dependencies t
      org-agenda-files (list "~/Documents/2. Notes/Org/todo.org"))

(setq-default org-display-custom-times t)
(defvar org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#ffffff")
  (setq beacon-blink-duration 1.3))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))
  
(defvar ispell-dictionary "british")
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default dired-listing-switches "-alh")
(global-font-lock-mode t)
(global-auto-revert-mode t)
(setq visible-bell t)
(setq-default fill-column 80)
(global-visual-line-mode t)

(use-package gcmh
  :demand t
  :defer t
  :init
  (setq gcmh-idle-delay 5
  	    gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode))

;;(setq frame-inhibit-implied-resize t)
(setq delete-by-moving-to-trash t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backup-by-copying t)

(use-package haskell-mode
  :hook (haskell-mode . (lambda ()
                          (haskell-doc-mode)
                          (turn-on-haskell-indent))))

(defvar my-linum-current-line-number 0)

(defvar linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((test2 (- line-number my-linum-current-line-number)))
    (propertize
     (number-to-string (cond ((<= test2 0) (* -1 test2))
                             ((> test2 0) test2)))
     'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(global-linum-mode t)
;;https://www.mycpu.org/emacs-relative-linum/

;;(use-package elfeed)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(paredit-everywhere vbasense ivy-rich gcmh omnisharp yasnippet slime-company slime company flycheck which-key rainbow-delimiters use-package))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
