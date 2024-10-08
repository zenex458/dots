;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
		use-package-expand-minimally t))

;;(setq custom-file (make-temp-file "emacs-custom-"))
(defvar ispell-dictionary "british")
(setq confirm-kill-emacs 'y-or-n-p)
(setq dired-listing-switches "-AlF --si -Gg --group-directories-first")
(setq scroll-conservatively 100)
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
;;(setq tab-always-indent 'complete)
(setq backup-directory-alist
	  `((".*" . ,"~/.emacs.d/saves/")))
(setq auto-save-file-name-transforms
	  `((".*" ,"~/.emacs.d/saves/" t)))
(setq isearch-lazy-count t)
(setq line-number-display-limit-width 2000000) ;;stop the question marks from showing in a large file
(setq
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 eww-search-prefix "https://duckduckgo.com/?q=")    ; Use another engine for searching

;; (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))



(defalias 'yes-or-no-p 'y-or-n-p)
(setq save-interprogram-paste-before-kill t)
(setq delete-by-moving-to-trash t)
(setq-default tab-width 4)
;;;(setopt ediff-window-setup-function #'ediff-setup-windows-plain)
;;(setq dired-dwim-target t)
(setq-default indent-tabs-mode t)
(setq backup-by-copying t)
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
(if '((system-name "eukaryotic"))
	(defvar my-term-shell "/run/current-system/sw/bin/bash")
  (defvar my-term-shell "/usr/bin/env bash"))

;; (defvar my-term-shell "/usr/bin/env bash") ;; add conditional
;; (defadvice ansi-term (before force-bash)
;;   "https://github.com/daedreth/UncleDavesEmacs#default-shell-should-be-bash"
;;   (interactive (list my-term-shell)))
;; (ad-activate 'ansi-term)

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

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("%a %b %e %Y" . "%a %b %e %Y %H:%M"))

(use-package diminish
  :ensure t)
(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-resize nil))

(use-package orderless
  :init
  (setq completion-category-overrides '((file (partial-completion))))
  (setq completion-styles '(substring orderless))) ;;add flex for more completion candiates

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(setq completion-in-region-function #'consult-completion-in-region)

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
		 ([remap Info-search] . consult-info)
		 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
		 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
		 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
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
		 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
		 ("M-s D" . consult-locate)
		 ("M-s g" . consult-grep)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

(use-package embark-consult)

(use-package apheleia
  :hook ((prog-mode . apheleia-mode))
  :config
  (diminish apheleia-mode)
  ;;  (setq apheleia-remote-algorithm 'local)
  (setf (alist-get 'astyle apheleia-formatters)
		'("astyle" "--mode=c" "--style=google"))
  (add-to-list 'apheleia-mode-alist '(c-ts-mode . astyle))
  (setf (alist-get 'csharpier apheleia-formatters)
		'("~/.dotnet/tools/dotnet-csharpier" "--write-stdout")) ;;dotnet tool install --global csharpier to install
  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))
  (setf (alist-get 'shfmt apheleia-formatters)
		'("shfmt"))
  (add-to-list 'apheleia-mode-alist '(bash-ts-mode . shfmt))
  (setf (alist-get 'black apheleia-formatters)
		'("yapf"))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . black))
  (setf (alist-get 'tidy apheleia-formatters)
		'("tidy" "-i" "-q" "-f" "err"))
  (add-to-list 'apheleia-mode-alist '(html-mode . tidy))
  (setf (alist-get 'nixfmt apheleia-formatters)
		'("nixfmt"))
  (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))
  (setf (alist-get 'ormolu apheleia-formatters)
		'("ormolu" "--stdin-input-file" "--"))
  (add-to-list 'apheleia-mode-alist '(haskell-mode . ormolu)))

;; (use-package aggressive-indent
;;   :hook (prog-mode . aggressive-indent-mode))

(setq treesit-language-source-alist
	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
		(c "https://github.com/tree-sitter/tree-sitter-c")
		(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		(c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
		(css "https://github.com/tree-sitter/tree-sitter-css")
		(elisp "https://github.com/Wilfred/tree-sitter-elisp")
		(go "https://github.com/tree-sitter/tree-sitter-go")
		(haskell "https://github.com/tree-sitter/tree-sitter-haskell")
		(html "https://github.com/tree-sitter/tree-sitter-html")
		(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		(json "https://github.com/tree-sitter/tree-sitter-json")
		(make "https://github.com/alemuller/tree-sitter-make")
		(markdown "https://github.com/ikatyang/tree-sitter-markdown")
		(nix "https://github.com/nix-community/tree-sitter-nix")
		(python "https://github.com/tree-sitter/tree-sitter-python")
		(toml "https://github.com/tree-sitter/tree-sitter-toml")
		(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(if (file-directory-p "~/.emacs.d/tree-sitter")
	(message "Tree-sitter grammers already installed")
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

(use-package pulsar
  :hook ((next-error . pulsar-pulse-line)
 		 (minibuffer-setup . pulsar-pulse-line))
  :config
  (pulsar-global-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (diminish 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package haskell-mode
  :magic ("%hs" . haskell-mode))

(use-package clojure-mode
  :magic ("%clj" . clojure-mode))

(use-package cider
  :custom
  (cider-repl-history-file t))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'insert)
  :hook ((prog-mode . corfu-mode)
		 (prog-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
			  ("M-SPC"      . corfu-insert-separator)
			  ("TAB"        . corfu-next)
			  ([tab]        . corfu-next)
			  ("S-TAB"      . corfu-previous)
			  ([backtab]    . corfu-previous)
			  ("M-SPC" . corfu-insert-separator)
			  ("RET" . corfu-insert)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
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
			   ;;			   `(csharp-ts-mode . ("/nix/store/jdp56g0j6mf7yjvqy9npw28y4pxcvgsw-omnisharp-roslyn-1.39.10/bin/OmniSharp" "-lsp"))
			   `(nix-ts-mode  . ("nil"))
			   ))

(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode))

;;(use-package dart-mode
;;  :custom
;;  (eglot-ignored-server-capabilities '(:signatureHelpProvider)))
;;
;;(use-package flutter
;;  :after dart-mode)
;;

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
		 (org-mode . yas-minor-mode))
  :config
  (add-hook 'prog-mode-hook  #'(lambda ()(yas-reload-all)))
  ;;yas-hippie-try-expand
  (diminish 'yas-minor-mode))

(use-package async
  :config
  (dired-async-mode t))

;; (use-package which-key
;;   :config
;;   (which-key-mode)
;;   (diminish 'which-key-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("*" "+")))

(use-package markdown-mode
  :magic ("%md" . markdown-mode)
  :init (setq markdown-command "pandoc"))

;; (use-package tex
;;   :ensure auctex
;;   :config
;;   (load "preview-latex.el" nil t t))

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)

(use-package auctex
  :ensure t
  :defer t)

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  (diminish 'volatile-highlights-mode))

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
		gcmh-high-cons-threshold (* 20 1024 1024)) ;;20mb
  (gcmh-mode 1)
  (diminish 'gcmh-mode))

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

;;(use-package vundo)

(use-package expreg
  :bind ("C-@" . expreg-expand)
  ("C-'" . expreg-contract))

(use-package expand-region)

(use-package vlf
  :init
  (require 'vlf-setup))

;;;(use-package pdf-tools
;;;  :magic ("%PDF" . pdf-view-mode)
;;;  :hook (pdf-view-mode . pdf-view-themed-minor-mode))

(use-package vterm)

(use-package dired-subtree
  :init
  (setq dired-subtree-use-backgrounds nil)
  (define-key dired-mode-map (kbd "TAB") #'dired-subtree-toggle)
  (define-key dired-mode-map [backtab] #'dired-subtree-cycle))

(use-package sudo-edit
  :bind
  ("M-s e" . sudo-edit)
  ("M-s f" . sudo-edit-find-file))

(use-package zoxide
  :hook (find-file . zoxide-add)
  :bind
  ("M-s z f" . zoxide-find-file))

;; (use-package god-mode
;;   :bind ("C-`" . god-local-mode))

(use-package magit)

;; (use-package writeroom-mode)

(use-package editorconfig)

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;; (use-package dired-ranger)
;; https://github.com/jcfk/dired-nnn
;; (use-package hydra
;;   :config
;;   ;;hydra for controlling brightness
;;   ;;hydra for controlling volume
;;   )

;; (use-package hammy
;;   :config
;;   ;;hammy config for looking away every 20mins
;;   ;;hammy config for resting hands every 4min
;;   )

(require 'upmu)
(global-set-key (kbd "C-c u") 'upmu-add-entry)
(define-prefix-command 'avy-n-map);; add this? http://yummymelon.com/devnull/announcing-casual-avy.html
(global-set-key (kbd "M-j") 'avy-n-map)
(global-set-key (kbd "<C-S-D>") 'backward-delete-char) ;;https://www.emacswiki.org/emacs/ShiftedKeys and https://www.emacswiki.org/emacs/TheMysteriousCaseOfShiftedFunctionKeys to fix
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-M-x") 'embark-export)
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
(global-set-key (kbd "C-c l s c")
				(lambda ()
				  "copies the whole line without moving the cursor"
				  (interactive)
				  (save-excursion
					(kill-new
					 (buffer-substring
					  (line-beginning-position)
					  (line-end-position))))))

(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

(setq mode-line-position (list " (%l:%C %P %I) "))
;;https://www.emacs.dyerdwelling.family/emacs/20230902114449-emacs--my-evolving-modeline/
(setq-default mode-line-format '(
								 (:eval
								  (if (and (buffer-file-name) (buffer-modified-p))
									  (propertize "**" 'face
												  '(:background "#e20023" :foreground "#000000" :inherit bold)) " "))
								 (:eval
								  (if (and buffer-read-only (mode-line-window-selected-p))
									  (propertize "%%%%" 'face
												  '(:background "#c6c6c6" :foreground "#000000" :inherit bold)) " "))
								 " "
								 (:eval
								  (if (mode-line-window-selected-p)
									  (propertize (buffer-name) 'face '(:foreground "#c6c6c6" :inherit bold))
									(propertize (buffer-name) 'face '(:foreground "#222222" :inherit bold))))
								 mode-line-position
								 (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

(setq minor-mode-alist nil)

(server-start)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ein lsp-pyright editorconfig auctex aggressive-indent org-babel cider clojure-mode elfeed-org elfeed helpful magit zoxide sudo-edit dired-subtree vlf expand-region expreg vundo multiple-cursors ace-window hungry-delete gcmh volatile-highlights markdown-mode org-bullets which-key async yasnippet cape corfu nix-ts-mode haskell-mode rainbow-delimiters rainbow-mode pulsar apheleia embark-consult embark consult marginalia orderless vertico diminish pdf-tools)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
