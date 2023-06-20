(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode))

(use-package smartparens
  :bind (("C-c p d" . sp-splice-sexp)
         ("C-c p r" . sp-rewrap-sexp)
         ("C-c p b d" . sp-backward-unwrap-sexp))
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (diminish 'smartparens-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (diminish 'which-key-mode))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  :config
  (diminish 'aggressive-indent-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (diminish 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package async
  :config (dired-async-mode 1))

(use-package hungry-delete
  :bind ([C-backspace] . hungry-delete-backward)
	([C-delete] . hungry-delete-forward))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package corfu
   :custom
   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
   (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
     :hook ((prog-mode . corfu-mode)
          (shell-mode . corfu-mode)
          (eshell-mode . corfu-mode)))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-reload-all)
  (diminish 'yas-minor-mode))

(use-package orderless
  :custom
  (completion-styles '(flex orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
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
	 ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))
(setq user-home-dir (expand-file-name "~/"))
(setq consult-find-args (concat "find " user-home-dir " -type f -not ( -wholename */.cache/* -prune -o -wholename */.dotnet/* -prune -wholename */.templateengine/* -prune -o -wholename */.ghc/* -prune -o -wholename */.icons/* -prune -o -wholename */.nuget/* -prune -o -wholename */.omnisharp/* -prune -o -wholename */.pki/* -prune -o -wholename */.local/share/* -prune -o -wholename */.git/* -prune -o -wholename */obsidian/* -prune -o -wholename */.java/* -prune -o -wholename */flutter/* )"))

(savehist-mode 1)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("*" "+")))

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

(use-package markdown-mode
  :mode ("*\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package expand-region
  :config
  (pending-delete-mode t))
(global-set-key (kbd "C-@") 'er/expand-region) ;;change to :bind

;;(use-package multiple-cursors
;;  :config
;;  (require 'multiple-cursors))
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-c C-@") 'mc/mark-all-like-this)

(use-package ace-window
  :bind ("C-x o" . ace-window))

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

(use-package centered-cursor-mode
  :init
  (global-centered-cursor-mode 1)
  :config
  (diminish 'centered-cursor-mode))

(use-package writeroom-mode
  :config
  (setq writeroom-header-line "")
  (setq writeroom-global-effects nil)
  (setq writeroom-maximize-window nil))

(use-package volatile-highlights
  :init
  (volatile-highlights-mode t)
  :config
  (diminish 'volatile-highlights-mode))

(use-package haskell-mode
  :hook ((haskell-mode . haskell-dock-mode)
	 (haskell-mode . turn-on-haskell-indent)))

(use-package gcmh
  :init
  (setq gcmh-idle-delay 20
        gcmh-high-cons-threshold (* 20 1024 1024)) ;;20mb
  (gcmh-mode 1)
  :config
  (diminish 'gcmh-mode))

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

(defun copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
	(kill-new
	 (buffer-substring
	  (point-at-bol)
	  (point-at-eol)))))

(defun edit-todo ()
  "Vist Org-mode Todo list."
  (interactive)
  (find-file "~/Documents/2.Notes/Org/todo.org"))

(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c l c") 'copy-whole-line)
(global-set-key (kbd "C-c l k") 'kill-whole-line)
(global-set-key (kbd "C-c c") 'flyspell-mode)
(global-set-key (kbd "C-c s") 'ispell)
(global-set-key (kbd "C-c t") 'edit-todo)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(defvar ispell-dictionary "british")
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default dired-listing-switches "-alh")
(put 'dired-find-alternate-file 'disabled nil)
(setq scroll-conservatively 100)
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
(defalias 'yes-or-no-p 'y-or-n-p)
(setq ffap-machine-p-known 'reject)
(setq delete-by-moving-to-trash t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq backup-by-copying t)
(setq history-length 20)
(defvar comp-async-report-warnings-errors nil)
;;(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
