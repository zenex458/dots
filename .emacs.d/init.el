(setq gc-cons-threshold 20000000)
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 110)

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
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'config-reload)


(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))

(electric-pair-mode t)

;;(use-package rainbow-mode
;;  :ensure t
;;  :init
;;  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(defalias 'yes-or-no-p 'y-or-n-p)

;;(use-package async
;;  :ensure t
;;  :init (dired-async-mode 1))

(use-package evil
  :ensure t)
(require 'evil)
(evil-mode 1)

(setq evil-insert-state-cursor '(hbar)
      evil-replace-state-cursor '(box))
(setq evil-want-minibuffer t)
;(setq ring-bell-function 'ignore)

(setq display-time-24hr-format t)
(display-time-mode 1)

(setq scroll-conservatively 100)

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

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
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c b") 'counsel-switch-buffer)

;;(use-package avy ;;switch windows
;;  :ensure t
;;  :bind
;;    ("M-s" . avy-goto-char))

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun edit-todo ()
  (interactive)
  (find-file "~/org/todo.org"))
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
  (setq company-minimum-prefix-length 3))

(with-eval-after-load 'company
;  (define-key company-active-map (kbd "M-n") nil)
;  (define-key company-active-map (kbd "M-p") nil)
;  (define-key company-active-map (kbd "C-n") #'company-select-next)
;  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))


(add-hook 'emacs-lisp-mode-hook 'company-mode)

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
  :init)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;;(setq-default mode-line-format
;;	       '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position (vc-mode vc-mode) "" mode-line-misc-info mode-line-end-spaces))

(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))
(define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete)

(use-package ivy
  :config
(ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(ido-mode t)

;;(require 'org)
;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)
;;(setq org-log-done t)
;;
;;(setq org-agenda-files (list "~/org/todo.org"))

(setq ispell-dictionary "british")
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default dired-listing-switches "-alh")
(global-font-lock-mode t)
(global-auto-revert-mode t)
(setq visible-bell t)
(setq-default fill-column 80)
(use-package gcmh
  :demand t
  :defer t
  
  :init
  (setq gcmh-idle-delay 5
  			gcmh-high-cons-threshold (* 16 1024 1024))
  :config
		(gcmh-mode))
(setq frame-inhibit-implied-resize t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy-rich gcmh omnisharp yasnippet slime-company slime company flycheck which-key nyx-theme evil rainbow-delimiters use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
