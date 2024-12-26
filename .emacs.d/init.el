;; -*- lexical-binding: t; -*-

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

;; (setq custom-file (make-temp-file "emacs-custom-"))
(expand-file-name "")
(setq custom-file (expand-file-name "~/.emacs.d/emacs-custom.el"))
(setq ffap-machine-p-known 'reject)
(defvar ispell-dictionary "british")
(setq auto-save-default t)
(setq sort-fold-case t)
(setq comment-multi-line t)
(setq sentence-end-double-space nil)
(setq fill-column 80)
(setq confirm-kill-emacs 'y-or-n-p)
(setq dired-listing-switches "-AlF --si -Gg --group-directories-first")
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program '"firefox"
      browse-url-generic-args '("-P" "priv"))
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(setq scroll-conservatively 100)
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
(setq backup-directory-alist
	  `((".*" . ,"~/.emacs.d/saves/")))
(setq auto-save-file-name-transforms
	  `((".*" ,"~/.emacs.d/saves/" t)))
(setq isearch-lazy-count t)
;; (setq line-number-display-limit-width 2000000) ;;stop the question marks from showing in a large file
(setq
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 eww-search-prefix "https://duckduckgo.com/?q=")    ; Use another engine for searching

;; (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))

(defalias 'yes-or-no-p 'y-or-n-p)
;; (setq save-interprogram-paste-before-kill nil)
(setq delete-by-moving-to-trash t)
(setq-default tab-width 4)
;;;(setopt ediff-window-setup-function #'ediff-setup-windows-plain)
(setq dired-dwim-target t)
(setq-default indent-tabs-mode nil)
(electric-indent-mode)
(setq tab-always-indent 'complete)
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
(setq org-time-stamp-custom-formats '("%a/%b/%e/%Y" . "%a/%b/%e/%Y %H:%M"))


(use-package diminish)
(diminish 'subword-mode)
(diminish 'visual-line-mode)
(diminish 'eldoc-mode)

(fido-mode)
(setq completion-category-overrides '((file (initials))))
;; (setq completion-styles '(initials partial-completion flex ))
(setq completion-styles '(basic substring initials partial-completion flex ))
(global-set-key [remap minibuffer-complete] 'icomplete-fido-ret)
(setq icomplete-compute-delay 0)

(use-package apheleia
  :init
  :hook ((prog-mode . apheleia-mode)
		 (LaTeX-mode . apheleia-mode))
  :config
  (setq apheleia-remote-algorithm 'local)
  (setf (alist-get 'astyle apheleia-formatters)
		'("astyle" "--mode=c" "--style=google"))
  (add-to-list 'apheleia-mode-alist '(c-ts-mode . astyle))
  (setf (alist-get 'csharpier apheleia-formatters)
		'("~/.dotnet/tools/dotnet-csharpier" "--write-stdout")) ;;dotnet tool install --global csharpier to install
  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))
  (setf (alist-get 'shfmt apheleia-formatters)
		'("shfmt"))
  (add-to-list 'apheleia-mode-alist '(bash-ts-mode . shfmt))
  (setf (alist-get 'ruff apheleia-formatters)
		;; '("yapf"))
		'("ruff format"))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (setf (alist-get 'tidy apheleia-formatters)
		'("tidy" "-i" "-q" "-f" "err"))
  (add-to-list 'apheleia-mode-alist '(html-mode . tidy))
  (setf (alist-get 'nixfmt apheleia-formatters)
		'("nixfmt"))
  (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))
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

(if (file-directory-p "~/.emacs.d/tree-sitter")
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

(use-package haskell-mode
  :magic ("%hs" . haskell-mode))

(use-package slime)

(use-package elfeed
  :config
  (setq elfeed-search-title-max-width '130)
  (setq elfeed-search-filter "@1-months-ago +unread"))

(use-package elfeed-org
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :bind ("C-c r" . updnix))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
		 (prog-mode . corfu-popupinfo-mode)))

(use-package cape
  :init
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
  (setq elisp-flymake-byte-compile-load-path load-path))

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

(use-package markdown-mode
  :magic ("%md" . markdown-mode)
  :init (setq markdown-command "pandoc"))

(use-package auctex
  :hook (tex-mode . LaTeX-mode))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  (diminish 'goggles-mode))

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
  :bind ("C-x o" . ace-window))

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

;;(use-package editorconfig)

(use-package sdcv)

(use-package golden-ratio
  :config
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (golden-ratio-mode 1))

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (setq edwina-mode-line-format "")
  (edwina-mode 1))

(use-package vterm)
(use-package multi-vterm)

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
(global-set-key (kbd "C-c b") 'bookmark-jump)
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
(global-set-key (kbd "C-c e")(lambda ()"opens init.el"(interactive)(find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x K")(lambda ()"kills curent buffer without confirmation"(interactive)(kill-buffer (current-buffer))))
(global-set-key (kbd "C-c r")(lambda ()"reloads emacs config"(interactive)(load-file "~/.emacs.d/init.el")))
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

(server-start)

(provide 'init)
;;; init.el ends here
