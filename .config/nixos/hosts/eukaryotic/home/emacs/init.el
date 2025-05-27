;; -*- lexical-binding: t; -*-
(load-theme 'saturn t)
;; (setq custom-file (make-temp-file "emacs-custom-"))
(setq custom-file (expand-file-name (format "'%s'emacs-custom.el"user-emacs-directory)))
(setq auth-sources '((format "'%s'.authinfo.gpg"user-emacs-directory)))
;; (setq debug-on-error t)
;; (setq ffap-machine-p-known 'reject)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(defvar ispell-dictionary "british")
(setq auto-save-default t)
(setq sort-fold-case t)
(setq comment-multi-line t)
(setq sentence-end-double-space nil)
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(setq fill-column 80)
(setq confirm-kill-emacs 'y-or-n-p)
(setq dired-listing-switches "-AlF --si -Gg --group-directories-first")
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program '"firefox")
;; browse-url-generic-args '("-P" "priv"))
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(setq scroll-conservatively 100)
(setq dired-movement-style t)
(setq auto-save-timeout 5)
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
(setq backup-directory-alist
	    `((".*" . ,(format "'%s'saves/"user-emacs-directory))t))
(setq auto-save-file-name-transforms
	    `((".*" ,(format "'%s'saves/"user-emacs-directory)t)))
(setq isearch-lazy-count t)
;; (setq line-number-display-limit-width 2000000) ;;stop the question marks from showing in a large file
(setq
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 eww-search-prefix "https://html.duckduckgo.com/?q=")    ; Use another engine for searching

;; (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))

(defalias 'yes-or-no-p 'y-or-n-p)
;; (setq save-interprogram-paste-before-kill nil)
(setq delete-by-moving-to-trash t)
(setq-default tab-width 2)
;;;(setopt ediff-window-setup-function #'ediff-setup-windows-plain)
(setq dired-dwim-target t)
(setq-default indent-tabs-mode nil)
(electric-indent-mode)
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq password-cache-expiry 3600)
(setq history-length 2000)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)
(setq read-file-name-completion-ignore-case t
	    read-buffer-completion-ignore-case t
	    completion-ignore-case t)

(add-hook 'prog-mode-hook #'(lambda ()
							                (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'prog-mode-hook  #'(lambda ()(setq show-trailing-whitespace t)))

(setq display-time-default-load-average nil)
(setq display-time-format "%H:%M")
(display-time-mode 1)
(display-battery-mode 1)
(global-subword-mode 1)
(pending-delete-mode t)
(savehist-mode 1)
(add-hook 'prog-mode-hook 'electric-pair-mode)

(defun close-or-kill-emacs ()
  "Close the current frame if there are multiple visible frames otherwise kill Emacs."
  (interactive)
  (require 'cl-lib)
  (let ((visible-frames (cl-remove-if-not 'frame-visible-p (frame-list))))
	  (if (> (length visible-frames) 1)
		    (if (y-or-n-p (format "Are you sure you want to close this frame? "))
			      (delete-frame)
		      (save-buffers-kill-emacs))
	    (save-buffers-kill-emacs))))
(global-set-key (kbd "C-x C-c") 'close-or-kill-emacs)

(setq org-startup-indented t
	    org-pretty-entities t
	    ;;	  org-hide-emphasis-markers t
	    ;;org-agenda-start-on-weekday t
	    org-ellipsis "~"
	    org-log-done 'time
	    org-html-validation-link nil
	    org-enforce-todo-dependencies t
	    calendar-week-start-day 1)
(setq org-agenda-files '("~/Documents/Notes/Org/todo.org"))
(setq org-todo-keywords '((type "TODO" "IN PROGRESS(I!)" "CANCELED(C@/!)" "|" "DONE")))
(setq org-todo-keyword-faces
	    '(("TODO" .  "#bdae93" )("IN PROGRESS" . "#BD8700") ("CANCELED" . "red")))
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%A %d/%m/%Y>" . "<%A %d %B %Y %H:%M>"))

(use-package diminish)
(diminish 'subword-mode)
(diminish 'visual-line-mode)
(diminish 'eldoc-mode)

(use-package vertico
  :config
  (vertico-mode 1)
  (vertico-flat-mode 1)
  (vertico-indexed-mode 1)
  (setq vertico-count 40))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic substring initials partial-completion))
  ;; (completion-styles '(substring orderless))
  ;; (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  ;; (completion-category-overrides '((file (initials))))
  )

(use-package consult
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
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
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
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-ripgrep)
         ("M-s G" . consult-git-grep)
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
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
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

(use-package apheleia
  :init
  :hook ((prog-mode . apheleia-mode)
		     (LaTeX-mode . apheleia-mode))
  :config
  (setq apheleia-remote-algorithm 'local)
  (setf (alist-get 'astyle apheleia-formatters)
		    '("astyle" "--mode=c" "--style=google" "-s2"))
  (add-to-list 'apheleia-mode-alist '(c-ts-mode . astyle))
  (setf (alist-get 'shfmt apheleia-formatters)
		    '("shfmt"))
  (add-to-list 'apheleia-mode-alist '(bash-ts-mode . shfmt))
  (setf (alist-get 'ruff apheleia-formatters)
		    '("ruff format"))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (setf (alist-get 'tidy apheleia-formatters)
		    '("tidy" "-i" "-q" "--tidy-mark" "no"))
  (add-to-list 'apheleia-mode-alist '(html-mode . tidy))
  (setf (alist-get 'nixfmt apheleia-formatters)
		    '("alejandra"))
  (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))
  (setf (alist-get 'xmlformat apheleia-formatters)
		    '("xmlformat"))
  (add-to-list 'apheleia-mode-alist '(nxml-mode . xmlformat))
  (setf (alist-get 'ormolu apheleia-formatters)
		    '("ormolu" "--stdin-input-file" "--"))
  (add-to-list 'apheleia-mode-alist '(haskell-mode . ormolu)))

(setq treesit-language-source-alist
	    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
		    (c "https://github.com/tree-sitter/tree-sitter-c")
		    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
		    (css "https://github.com/tree-sitter/tree-sitter-css")
		    (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
		    (html "https://github.com/tree-sitter/tree-sitter-html")
		    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		    (json "https://github.com/tree-sitter/tree-sitter-json")
		    (make "https://github.com/alemuller/tree-sitter-make")
		    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		    (nix "https://github.com/nix-community/tree-sitter-nix")
		    (python "https://github.com/tree-sitter/tree-sitter-python")
		    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(if (file-directory-p (format "'%s'tree-sitter/"user-emacs-directory))
	  (message "")
  (message "Downloading treesiter grammers")
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(setq major-mode-remap-alist
	    '((yaml-mode . yaml-ts-mode)
		    (bash-mode . bash-ts-mode)
		    (js2-mode . js-ts-mode)
		    (typescript-mode . typescript-ts-mode)
		    (json-mode . json-ts-mode)
		    (c-mode . c-ts-mode)
		    ("c++-mode" . "c++-ts-mode")
		    (csharp-mode . csharp-ts-mode)
		    (css-mode . css-ts-mode)
		    (python-mode . python-ts-mode)))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (diminish 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(use-package elfeed
  :config
  ;; (setq elfeed-search-title-max-width '130)
  (setq elfeed-search-filter "@3-days-ago +unread")
  (setq elfeed-db-directory (format "'%s'elfeed/"user-emacs-directory)))

(use-package elfeed-org
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list (format "'%s'elfeed.org"user-emacs-directory))))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :bind ("C-c r" . updnix))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook ((prog-mode . corfu-mode)
		     (prog-mode . corfu-popupinfo-mode)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (setq-local completion-at-point-functions
			        (mapcar #'cape-company-to-capf
					            (list #'company-files #'company-keywords #'company-dabbrev))))

(use-package eglot
  :hook ((haskell-mode . eglot-ensure)
		     (dart-mode . eglot-ensure)
		     (clojure-mode . eglot-ensure)
		     (bash-ts-mode . eglot-ensure)
		     (sh-mode . eglot-ensure)
		     (c-ts-mode . eglot-ensure)
 		     (csharp-ts-mode . eglot-ensure)
		     (python-ts-mode . eglot-ensure)
		     (nix-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
    		       `(nix-ts-mode  . ("nixd"))))

(use-package flymake
  :hook ((emacs-lisp-mode . flymake-mode)
		     (LaTeX-mode . flymake-mode))
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  (setq flymake-indicator-type 'fringes))


(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
		     (org-mode . yas-minor-mode)
		     (LaTeX-mode . yas-minor-mode))
  :config
  (yas-reload-all)
  (diminish 'yas-minor-mode))

(use-package async
  :config
  (dired-async-mode t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("*" "+")))

(use-package org-make-toc)

(use-package markdown-mode
  :magic ("%md" . markdown-mode)
  :init (setq markdown-command "pandoc"))

(use-package auctex
  :hook (tex-mode . LaTeX-mode))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; (setq-default TeX-master nil)

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
		    gcmh-high-cons-threshold (* 20 1024 1024)) ;;20mb
  (gcmh-mode 1)
  (diminish 'gcmh-mode))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package hungry-delete
  :bind (([C-backspace] . hungry-delete-backward)
		     ([C-delete] . hungry-delete-forward)))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-minibuffer-flag t))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		     ("C->" . mc/mark-next-like-this)
		     ("C-<" . mc/mark-previous-like-this)
		     ("C-C C-@" . mc/mark-all-like-this)
		     ("C-C M-v" . mc/edit-beggineing-of-lines)))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-compression 'zst)
  (setq undo-fu-session-incompatible-files
        '("COMMIT_EDITMSG"
          "NOTES_EDITMSG"
          "MERGE_MSG"
          "TAG_EDITMSG"
          "\\.gpg\\'"
          "/tmp"
          file-remote-p))
  (undo-fu-session-global-mode))

(use-package expreg
  :bind ("C-@" . expreg-expand)
  ("C-'" . expreg-contract))

(use-package vlf
  :init
  (require 'vlf-setup))

(use-package dired-subtree
  :init
  (setq dired-subtree-use-backgrounds nil)
  (define-key dired-mode-map (kbd "TAB") #'dired-subtree-toggle)
  (define-key dired-mode-map [backtab] #'dired-subtree-cycle))

(use-package sudo-edit
  :bind
  ("M-s e" . sudo-edit)
  ("M-s f" . sudo-edit-find-file))

(use-package magit)

(use-package indent-guide
  :hook (python-ts-mode . indent-guide-mode))



(require 'updnix)
(require 'upmu)
(global-set-key (kbd "<f5>") 'recompile)
(define-prefix-command 'vterm-n-map)
(global-set-key (kbd "C-x v") 'vterm-n-map)
(global-set-key (kbd "C-x v v") 'multi-vterm)
(global-set-key (kbd "C-x v n") 'multi-vterm-next)
(global-set-key (kbd "C-x v p") 'multi-vterm-prev)
(global-set-key (kbd "C-x v d") 'multi-vterm-dedicated-toggle)
(global-set-key (kbd "C-c u") 'upmu-add-entry)
;;(global-set-key (kbd "<C-S-D>") 'backward-delete-char) ;;https://www.emacswiki.org/emacs/ShiftedKeys and https://www.emacswiki.org/emacs/TheMysteriousCaseOfShiftedFunctionKeys to fix
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-M-x") 'embark-export)
(define-prefix-command 'avy-n-map);; add this? http://yummymelon.com/devnull/announcing-casual-avy.html
(global-set-key (kbd "M-j") 'avy-n-map)
(global-set-key (kbd "M-j l") 'avy-copy-line)
(global-set-key (kbd "M-j k") 'avy-kill-whole-line)
(global-set-key (kbd "M-j m") 'avy-move-line)
(global-set-key (kbd "M-j c") 'avy-goto-char)
(global-set-key (kbd "M-j g") 'avy-goto-line)
(global-set-key (kbd "C-c c") 'flyspell-buffer) ;; maybe use this? https://github.com/minad/jinx
(global-set-key (kbd "C-c s") 'ispell-buffer)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-c e")(lambda ()"opens init.el"(interactive)(find-file (format "'%s'init.el"user-emacs-directory))))
(global-set-key (kbd "C-x K")(lambda ()"kills curent buffer without confirmation"(interactive)(kill-buffer (current-buffer))))
(global-set-key (kbd "C-c r")(lambda ()"reloads emacs config"(interactive)(load-file (format "'%s'init.el"user-emacs-directory))))
(global-set-key (kbd "C-c t")(lambda ()"opens todo"(interactive)(find-file "~/Documents/Notes/Org/todo.org")))
(global-set-key (kbd "C-c C-l")
				        (lambda ()
				          "Copies the whole line without moving the cursor"
				          (interactive)
				          (save-excursion
					          (kill-new
					           (buffer-substring
					            (line-beginning-position)
					            (line-end-position))))
                  (message "Line copied.")))
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

(setq mode-line-position (list " (%l:%C %P %I) "))
;;https://www.emacs.dyerdwelling.family/emacs/20230902114449-emacs--my-evolving-modeline/
(setq-default mode-line-format '((:eval
								                  (if (and (buffer-file-name) (buffer-modified-p))
									                    (propertize "**" 'face
												                          '(:background "#e20023" :foreground "#000000" :inherit bold)) " "))
								                 (:eval
								                  (if (and buffer-read-only (mode-line-window-selected-p))
									                    (propertize "%%%%" 'face
												                          '(:background "#bdae93" :foreground "#000000" :inherit bold)) " "))
								                 " "
								                 (:eval
								                  (if (mode-line-window-selected-p)
									                    (propertize (buffer-name) 'face '(:foreground "#bdae93" :inherit bold))
									                  (propertize (buffer-name) 'face '(:foreground "#222222" :inherit bold))))
								                 mode-line-position
								                 (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

;;(server-start)

(provide 'init)
;;; init.el ends here
