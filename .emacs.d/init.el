(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(wheatgrass))
 '(display-battery-mode t)
 '(display-line-numbers-type 'relative)
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(omnisharp htmlize slime-company slime company flycheck avy which-key async rainbow-delimiters rainbow-mode evil use-package))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "CTDB" :slant normal :weight normal :height 98 :width normal)))))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)
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
  "Reloads ~/.emacs.d/init.el at runtime"
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

 (require 'evil)
  (evil-mode 1)

(setq evil-insert-state-cursor '(hbar)
      evil-replace-state-cursor '(box))
(setq evil-want-minibuffer t)
(setq ring-bell-function 'ignore)

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
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(global-set-key (kbd "C-x b") 'ibuffer)

(use-package avy ;;switch windows
  :ensure t
  :bind
    ("M-s" . avy-goto-char))

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
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
  :config
  (yas-global-mode 1))
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(setq yas-snippet-dir "~/.emacs.d/snippets")

(setq-default mode-line-format
	       '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position (vc-mode vc-mode) mode-line-misc-info mode-line-end-spaces))

(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))
(define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete)
