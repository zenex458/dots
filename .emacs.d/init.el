;;; init.el --- Config
;;; Commentary:
;;; Code:

;;(server-start)
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
  :defer 100
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
;;(setq use-packagenn-compute-statistics t) ;then do (use-package-report)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'morest t)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 8)
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default fringes-outside-margins nil)
(menu-bar-mode -1)
(display-battery-mode 1)
(size-indication-mode 1)
(save-place-mode 1)
;;(setq display-line-numbers-type 'relative)

(defun my-enable-line-numbers ()
  (when (derived-mode-p 'prog-mode)
    (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook 'my-enable-line-numbers)

(setq display-line-numbers-type 'absolute)

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

;;probably should remove this and use the build in instead
(use-package diminish
  :init
  :ensure t)
(diminish 'eldoc-mode)
(diminish 'visual-line-mode)

(use-package smartparens
  :init
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode))

(global-set-key (kbd "C-c p d") 'sp-splice-sexp)
(global-set-key (kbd "C-c p r") 'sp-rewrap-sexp)
(global-set-key (kbd "C-c p b d") 'sp-backward-unwrap-sexp)
(sp-local-pair 'smartparens-strict-mode "'" nil :actions nil)

(defun my-disable-elisp-smartparens ()
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))
(add-hook 'emacs-lisp-mode 'my-disable-elisp-smartparens)
(diminish 'smartparens-mode)

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(diminish 'rainbow-mode)


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
(diminish 'which-key-mode)

(global-set-key (kbd "C-c b") 'ibuffer)

(defun copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
	(kill-new
	 (buffer-substring
	  (point-at-bol)
	  (point-at-eol)))))
(global-set-key (kbd "C-c l c") 'copy-whole-line)
(global-set-key (kbd "C-c l k") 'kill-whole-line)

(use-package hungry-delete
  :config
  (require 'hungry-delete))
(global-set-key [C-backspace] 'hungry-delete-backward)
(global-set-key [C-delete] 'hungry-delete-forward)

(defun config-visit ()
  "Vist Emacs config."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun edit-todo ()
  "Vist Org-mode Todo list."
  (interactive)
  (find-file "~/Documents/2.Notes/Org/todo.org"))
(global-set-key (kbd "C-c t") 'edit-todo)


(global-set-key (kbd "C-c c") 'flyspell-mode)
(global-set-key (kbd "C-c s") 'ispell)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(diminish 'flycheck-mode)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))
(diminish 'company-mode)

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)


(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))
(use-package company-shell
  :ensure t
  :config
  (require 'company)
  (add-hook 'shell-mode-hook 'shell-mode-company-init))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))
(diminish 'yas-minor-mode)

(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  (define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete))
;;(setq omnisharp-server-executable-path "/etc/profiles/per-user/zenex/bin/OmniSharp")

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
;;(setq marginalia-align-offset 10)
(use-package vertico
  :init

  (vertico-mode))

(use-package savehist
:init
(savehist-mode))

(use-package emacs
:init
;; Emacs 28: Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)
(setq enable-recursive-minibuffers t))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-start-on-weekday t
      org-log-done 'time
      org-enforce-todo-dependencies t
      calendar-week-start-day 1
      org-agenda-files (list "~/Documents/2.Notes/Org/todo.org"))

(setq-default org-display-custom-times t)
(defvar org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#ffffff")
  (setq beacon-blink-duration 1.3))
(diminish 'beacon-mode)


(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package markdown-mode
  :ensure t
  :mode ("*\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package expand-region
  :config
  (require 'expand-region)
  (pending-delete-mode t))
(global-set-key (kbd "C-@") 'er/expand-region)

(use-package multiple-cursors
  :defer 5
  :config
  (require 'multiple-cursors))
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-@") 'mc/mark-all-like-this)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(use-package ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
	(?m aw-swap-window "Swap Windows")
	(?M aw-move-window "Move Window")
	(?c aw-copy-window "Copy Window")
	(?j aw-switch-buffer-in-window "Select Buffer")
	(?n aw-flip-window)
	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	(?c aw-split-window-fair "Split Fair Window")
	(?v aw-split-window-vert "Split Vert Window")
	(?b aw-split-window-horz "Split Horz Window")
	(?o delete-other-windows "Delete Other Windows")
	(?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")


(defvar ispell-dictionary "british")
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default dired-listing-switches "-alh")
(global-font-lock-mode t)
(global-auto-revert-mode t)
(setq visible-bell t)
;;(setq-default fill-column 80)
(global-visual-line-mode t)
;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)

(defvar ml-selected-window nil)

(defvar ml-total-lines nil
  "Previously recorded total lines in a buffer -- used for inactive windows.")
(make-variable-buffer-local 'ml-total-lines)

(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(defun ml-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'ml-record-selected-window)

(add-hook 'buffer-list-update-hook 'ml-update-all)

(setq-default mode-line-format '(
                                 "%e"
                                 mode-line-front-space mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
                                 "(%l/"
                                 (:eval
                                  (let ((win (selected-window)))
                                    (with-current-buffer (window-buffer win)
                                      (if (or (eq ml-selected-window win) (null ml-total-lines))
                                          (save-excursion
                                            (goto-char (point-max))
                                            (setq ml-total-lines (format-mode-line "%l")))
                                        ml-total-lines))))
                                 ":%C %I) "
                                 (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

(use-package gcmh
  :demand t
  :defer t
  :init
  (setq gcmh-idle-delay 5
  	    gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode 1))

(setq frame-inhibit-implied-resize t)
(setq delete-by-moving-to-trash t)
;;(setq-default indicate-empty-lines t)
;;(setq-default indicate-buffer-boundaries 'left)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq tab-always-indent 'complete)
(setq backup-by-copying t)
(setq history-length 20)
(defvar comp-async-report-warnings-errors nil)
(savehist-mode 1)

(use-package centered-cursor-mode
  :init
  :config
  (global-centered-cursor-mode 1))
(diminish 'centered-cursor-mode)

(use-package writeroom-mode)

(setq writeroom-header-line "")
(setq writeroom-global-effects nil)
(setq writeroom-maximize-window nil)


(use-package volatile-highlights
  :config
  (require 'volatile-highlights)
  (volatile-highlights-mode t))
(diminish 'volatile-highlights-mode)

(use-package haskell-mode
  :hook (haskell-mode . (lambda ()
                          (haskell-doc-mode)
                          (turn-on-haskell-indent))))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(writeroom-mode centered-window centered-window-mode orderless orderlesss haskell-mode gcmh ace-window multiple-cursors expand-region markdown-mode aggressive-indent beacon omnisharp yasnippet company-shell company flycheck hungry-delete which-key async rainbow-delimiters rainbow-mode diminish use-package smartparens auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
