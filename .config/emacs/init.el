;; -*- lexical-binding: t; -*-

(use-package emacs
  :init
  (setq-default org-time-stamp-custom-formats '("<%A %d/%m/%Y>" . "<%A %d/%m/%Y %H:%M>"))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-prefix-command 'vterm-n-map)
  (define-prefix-command 'avy-n-map);; add this? http://yummymelon.com/devnull/announcing-casual-avy.html
  (define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)
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
  (setq org-latex-classes
        '(("IEEEtran" "\\documentclass[conference]{IEEEtran}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("article" "\\documentclass[11pt]{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

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

  (setq browse-url-browser-function 'browse-url-generic
	      browse-url-generic-program "firefox")
  ;; (setq browse-url-generic-args '("-P" "priv"))
  (require 'updnix)
  (require 'upmu)
  :custom
  (custom-file (expand-file-name (format "%s%s" user-emacs-directory "emacs-custom.el")))
  ;; (custom-file (make-temp-file "emacs-custom-"))
  (auth-sources (expand-file-name (format "%s%s" user-emacs-directory ".authinfo.gpg")))
  (backup-directory-alist
	 `((".*" . ,(expand-file-name (format "%s%s" user-emacs-directory "saves/")))t))
  (auto-save-file-name-transforms
	 `((".*" ,(expand-file-name (format "%s%s" user-emacs-directory "saves/"))t)))

  (shr-use-fonts  nil); No special fonts
  (shr-use-colors nil); No colours
  (eww-search-prefix "https://html.duckduckgo.com/?q="); Use another engine for searching
  ;; (setq debug-on-error t)
  ;; (setq ffap-machine-p-known 'reject)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (ring-bell-function 'ignore)
  (visible-bell nil)
  (defvar ispell-dictionary "british")
  (auto-save-default t)
  (sort-fold-case t)
  (comment-multi-line t)
  (sentence-end-double-space nil)
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  (fill-column 80)
  (confirm-kill-emacs 'y-or-n-p)
  (dired-listing-switches "-AlF --si -Gg --group-directories-first")
  (minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (completion-in-region-function #'consult-completion-in-region)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (scroll-conservatively 100)
  (dired-movement-style t)
  (auto-save-timeout 5)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (isearch-lazy-count t)
  ;; (setq line-number-display-limit-width 2000000) ;;stop the question marks from showing in a large file
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))
  ;; (setq save-interprogram-paste-before-kill nil)
  (delete-by-moving-to-trash t)
  (tab-width 2)
;;;(setopt ediff-window-setup-function #'ediff-setup-windows-plain)
  (dired-dwim-target t)
  (indent-tabs-mode nil)
  (electric-indent-mode)
  (tab-always-indent 'complete)
  (completion-cycle-threshold 1)
  ;;(text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (password-cache-expiry 3600)
  (history-length 2000)
  (dired-kill-when-opening-new-dired-buffer t)
  (native-comp-async-report-warnings-errors nil)
  ;; (warning-minimum-level :error)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (display-time-default-load-average nil)
  (display-time-format "%H:%M")
  (savehist-mode 1)
  (recentf-mode)
  (recentf-max-saved-items 5)
  (org-startup-indented t)
  (org-pretty-entities t)
  ;;(org-hide-emphasis-markers t)
  ;;(org-agenda-start-on-weekday t)
  (org-ellipsis "~")
  (org-log-done 'time)
  (org-html-validation-link nil)
  (org-enforce-todo-dependencies t)
  (calendar-week-start-day 1)
  (org-agenda-files '("~/Documents/Notes/Org/todo.org"))
  (org-todo-keywords '((type "TODO" "IN PROGRESS(I!)" "CANCELED(C@/!)" "|" "DONE")))
  (org-todo-keyword-faces
   '(("TODO" .  "#bdae93" )("IN PROGRESS" . "#BD8700") ("CANCELED" . "red")))
  (org-display-custom-times t)
  (org-icalendar-use-scheduled '(event-if-todo-not-done))
  (org-icalendar-use-deadline '(event-if-todo-not-done))
  :hook
  ((after-init . display-time-mode)
   (after-init . display-battery-mode)
   (after-init . global-subword-mode)
   (after-init . pending-delete-mode)
   (compilation-filter . ansi-color-compilation-filter)
   (prog-mode-hook . (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
   (prog-mode . (lambda ()(setq show-trailing-whitespace t)))
   (prog-mode . electric-pair-mode))
  :bind (("C-x C-c" . close-or-kill-emacs)
         ("<f5>" . recompile)
         ("C-x v" . vterm-n-map)
         ("C-x v v" . multi-vterm)
         ("C-x v n" . multi-vterm-next)
         ("C-x v p" . multi-vterm-prev)
         ("C-x v d" . multi-vterm-dedicated-toggle)
         ("C-c u" . upmu-add-entry)
	       ;;   (global-set-key (kbd "<C-S-D>") 'backward-delete-char) ;;https://www.emacswiki.org/emacs/ShiftedKeys and https://www.emacswiki.org/emacs/TheMysteriousCaseOfShiftedFunctionKeys to fix
         ("C-;" . comment-or-uncomment-region)
         ([remap dabbrev-expand] . hippie-expand)
         ("C-M-x" . embark-export)
         ("M-j" . avy-n-map)
         ("M-j l" . avy-copy-line)
         ("M-j k" . avy-kill-whole-line)
         ("M-j m" . avy-move-line)
         ("M-j c" . avy-goto-char)
         ("M-j g" . avy-goto-line)
         ("C-c c" . flyspell-buffer);; maybe use this? https://github.com/minad/jinx
         ("C-c s" . ispell-buffer)
         ("M-Z" . zap-up-to-char)
         ("C-c e" . (lambda ()"opens init.el"(interactive)(find-file "~/Dev/dots/.config/emacs/init.el")))
         ("C-x K" . (lambda ()"kills curent buffer without confirmation"(interactive)(kill-buffer (current-buffer))))
         ("C-c r" . (lambda ()"reloads emacs config"(interactive)"~/Dev/dots/.config/emacs/init.el"))
         ("C-c t" . (lambda ()"opens todo"(interactive)(find-file "~/Documents/Notes/Org/todo.org")))
         ("C-c C-l" . (lambda ()
	  		                "Copies the whole line without moving the cursor"
	  		                (interactive)
	  		                (save-excursion
	  		                  (kill-new
	  		                   (buffer-substring
	  		                    (line-beginning-position)
	  		                    (line-end-position))))
	  		                (message "Line copied.")))))


(use-package diminish
  :init)
(diminish 'subword-mode)
(diminish 'visual-line-mode)
(diminish 'eldoc-mode)

(use-package vertico
  :hook ((after-init . vertico-mode)
         (after-init . vertico-flat-mode)
         (after-init . vertico-indexed-mode))
  :custom
  (vertico-count 40))

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
  (completion-styles '(orderless substring basic initials partial-completion))
  ;; (completion-styles '(substring orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (initials styles partial-completion)))))
;; (completion-category-overrides '((file (initials)))))


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

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (advice-add #'register-preview :override #'consult-register-window)
  (register-preview-delay 0.5)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer
							                                            consult--source-buffer
							                                            consult--source-file-register
							                                            consult--source-bookmark
							                                            consult--source-project-buffer-hidden
							                                            consult--source-project-root-hidden))
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))


(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package apheleia
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
  (setf (alist-get 'prettier apheleia-formatters)
	      '("yamlfmt" "--"))
  (add-to-list 'apheleia-mode-alist '(css-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(yaml-ts-mode . prettier))
  (setf (alist-get 'ormolu apheleia-formatters)
	      '("ormolu" "--stdin-input-file" "--"))
  (add-to-list 'apheleia-mode-alist '(haskell-mode . ormolu)))


(if (file-directory-p (expand-file-name (format "%s%s" user-emacs-directory "tree-sitter/")))
    (message "")
  (message "Downloading treesiter grammers")
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (diminish 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package elfeed
  :custom
  (setq elfeed-search-title-max-width '130)
  (elfeed-search-filter "@3-days-ago +unread")
  (elfeed-db-directory (expand-file-name (format "%s%s" user-emacs-directory "elfeed/"))))

(use-package elfeed-org
  :hook (after-init . elfeed-org)
  :custom
  (rmh-elfeed-org-files (list (expand-file-name (format "%s%s" user-emacs-directory "elfeed.org")))))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook ((nix-ts-mode . (lambda ()(electric-pair-mode -1)))));;surely theres a better way?


(use-package yaml-ts-mode
  :mode "\\.yml\\'")


(use-package pipenv
  :hook (python-ts-mode . pipenv-mode))



;; https://github.com/promethial/.emacs.d/blob/c71732112300f1dc294769821533a8627440b282/init.el#L326
(use-package haskell-mode)

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-history-mode)
         (after-init . corfu-popupinfo-mode))

  :custom
  ;;(corfu-auto t)
  ;;(corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package cape
  :custom
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (completion-at-point-functions
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
	       (LaTeX-mode . eglot-ensure)
	       (nix-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
    	         `(nix-ts-mode  . ("nixd")))
  (add-to-list 'eglot-server-programs
               `(LaTeX-mode  . ("texlab")))
  )



(use-package flymake
  :hook ((emacs-lisp-mode . flymake-mode)
	       (LaTeX-mode . flymake-mode))
  :custom
  (flymake-indicator-type 'fringes))


(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
	       (org-mode . yas-minor-mode)
	       (LaTeX-mode . yas-minor-mode))
  :config
  (diminish 'yas-minor-mode))

(use-package async
  :custom
  (dired-async-mode t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("*" "+")))

(use-package org-make-toc)

(use-package markdown-mode
  :magic ("%md" . markdown-mode)
  :custom (setq markdown-command "pandoc"))

(use-package auctex
  :hook (tex-mode . LaTeX-mode))

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
  :custom
  (aw-scope 'frame)
  (aw-minibuffer-flag t))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	       ("C->" . mc/mark-next-like-this)
	       ("C-<" . mc/mark-previous-like-this)
	       ("C-C C-@" . mc/mark-all-like-this)
	       ("C-C M-v" . mc/edit-beggineing-of-lines))
  :custom
  (mc/always-run-for-all t))

(use-package undo-fu)

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-compression 'zst)
  (undo-fu-session-incompatible-files
   '("COMMIT_EDITMSG"
     "NOTES_EDITMSG"
     "MERGE_MSG"
     "TAG_EDITMSG"
     "\\.gpg\\'"
     "/tmp"
     file-remote-p)))

(use-package expreg
  :bind ("C-@" . expreg-expand)
  ("C-'" . expreg-contract))

(use-package vlf
  :custom
  (require 'vlf-setup))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)
        ([tab] . dired-subtree-toggle)
        ("S-TAB" . dired-subtree-cycle)
        ([backtab] . dired-subtree-cycle))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package sudo-edit
  :bind
  ("M-s e" . sudo-edit)
  ("M-s f" . sudo-edit-find-file))

(use-package magit)

(use-package indent-guide
  :hook (python-ts-mode . indent-guide-mode))


(use-package zoxide
  :hook (find-file . zoxide-add)
  :bind
  ("M-s z f" . zoxide-find-file))

;;(use-package golden-ratio
;;  :hook (after-init . golden-ratio-mode)
;;  :custom
;;  (golden-ratio-auto-scale t)
;;  (add-to-list 'golden-ratio-extra-commands 'ace-window))
;;
;;(use-package edwina
;;  :hook (after-init . edwina-mode)
;;  :custom
;;  (display-buffer-base-action '(display-buffer-below-selected))
;;  (edwina-mode-line-format ""))

;; (use-package evil
;;   :hook (prog-mode . evil-local-mode))

;; (use-package key-chord
;;   :hook (evil-local-mode . key-chord-mode)
;;   :config
;;   (setq key-chord-two-keys-delay 0.3)
;;   (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

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
