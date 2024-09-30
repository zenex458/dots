;; -*- lexical-binding: t; -*-
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name
		"straight/repos/straight.el/bootstrap.el"
		(or (bound-and-true-p straight-base-dir)
			user-emacs-directory)))
	  (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq custom-file (make-temp-file "emacs-custom-"))
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
(setq tab-always-indent 'complete)
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
(defalias 'yes-or-no-p 'y-or-n-p)
(setq ffap-machine-p-known 'reject)
(setq delete-by-moving-to-trash t)
(setq-default tab-width 4)
;;(setq dired-dwim-target t)
(setq-default indent-tabs-mode t)
(setq backup-by-copying t)
(setq history-length 20)
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
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  "https://github.com/daedreth/UncleDavesEmacs#default-shell-should-be-bash"
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

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
(setq org-agenda-files '("~/Documents/2.Notes/Org/todo.org"))
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("%a %b %e %Y" . "%a %b %e %Y %H:%M"))

(use-package diminish)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (("C-(" . sp-splice-sexp)
		 ("C-)" . sp-wrap-round))
  :config
  (require 'smartparens-config)
  (diminish 'smartparens-mode))

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
  (setf (alist-get 'astyle apheleia-formatters)
		'("astyle" "--mode=c" "--style=google"))
  (add-to-list 'apheleia-mode-alist '(c-ts-mode . astyle))
  (setf (alist-get 'csharpier apheleia-formatters)
		'("~/.dotnet/tools/dotnet-csharpier" "--write-stdout")) ;;dotnet tool install --global csharpier to install
  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))
  (setf (alist-get 'shfmt apheleia-formatters)
		'("shfmt"))
  (add-to-list 'apheleia-mode-alist '(bash-ts-mode . shfmt))
  (setf (alist-get 'tidy apheleia-formatters)
		'("tidy" "-i" "-q" "-f" "err"))
  (add-to-list 'apheleia-mode-alist '(html-mode . tidy))
  (setf (alist-get 'ormolu apheleia-formatters)
		'("ormolu" "--stdin-input-file" "--"))
  (add-to-list 'apheleia-mode-alist '(haskell-mode . ormolu)))


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
		(python "https://github.com/tree-sitter/tree-sitter-python")
		(toml "https://github.com/tree-sitter/tree-sitter-toml")
		(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		(yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

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

(use-package elpy
  :mode ("*\\.py\\'" . elpy-mode))

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
		 (bash-ts-mode . eglot-ensure)
		 (sh-mode . eglot-ensure)
		 (c-ts-mode . eglot-ensure)))

(use-package dart-mode
  :custom
  (eglot-ignored-server-capabilities '(:signatureHelpProvider)))

(use-package flutter
  :after dart-mode)

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
		 (org-mode . yas-minor-mode))
  :config
  (add-hook 'prog-mode-hook  #'(lambda ()(yas-reload-all)))
  (diminish 'yas-minor-mode))

;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode))

(use-package async
  :config
  (dired-async-mode 1))

(use-package which-key
  :config
  (which-key-mode)
  (diminish 'which-key-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("*" "+")))

(use-package markdown-mode
  :magic ("%md" . markdown-mode)
  :init (setq markdown-command "pandoc"))

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

(use-package vundo)

(use-package expreg
  :bind ("C-@" . expreg-expand)
  ("C-'" . expreg-contract))

(use-package vlf
  :init
  (require 'vlf-setup))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :config
  (pdf-loader-install))

(use-package elfeed)

(use-package elfeed-org
  :defer t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package dired-subtree
  :init
  (setq dired-subtree-use-backgrounds nil)
  (define-key dired-mode-map (kbd "TAB") #'dired-subtree-toggle))

(use-package sudo-edit
  :bind
  ("M-s e" . sudo-edit)
  ("M-s f" . sudo-edit-find-file))

(use-package zoxide
  :hook (find-file . zoxide-add)
  :bind
  ("M-s z f" . zoxide-find-file))

(use-package god-mode
  :bind ("C-`" . god-local-mode))

(use-package magit)

(use-package writeroom-mode)

(use-package emms
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  :bind
  ("C-c m e" . emms)
  ("C-c m b" . emms-smart-browse)
  ("C-c m u" . emms-player-mpd-update-all-reset-cache)
  ("C-c m p" . emms-previous)
  ("C-c m n" . emms-next)
  ("C-c m P" . emms-pause)
  ("C-c m s" . emms-stop)
  ("C-c m r" . (lambda ()
				 "Goto a random tack then add it to the paylist"
				 (interactive)
				 (progn
				   (emms-browser-expand-all)
				   (emms-browser-goto-random)
				   (emms-browser-add-tracks)))))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

(use-package dired-ranger)

;;
(define-prefix-command 'avy-n-map)
(global-set-key (kbd "M-j") 'avy-n-map)
(global-set-key (kbd "C-c t") 'edit-todo)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-M-x") 'embark-export)
(global-set-key (kbd "M-j l") 'avy-copy-line)
(global-set-key (kbd "M-j k") 'avy-kill-whole-line)
(global-set-key (kbd "M-j m") 'avy-move-line)
(global-set-key (kbd "M-j c") 'avy-goto-char)
(global-set-key (kbd "M-j g") 'avy-goto-line)
(global-set-key (kbd "C-c c") 'flyspell-buffer)
(global-set-key (kbd "C-c s") 'ispell-buffer)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-c e")(lambda ()"opens init.el"(interactive)(find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x K")(lambda ()"kills curent buffer without confirmation"(interactive)(kill-buffer (current-buffer))))
(global-set-key (kbd "C-c r")(lambda ()"reloads emacs config"(interactive)(load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c t")(lambda ()"opens todo"(interactive)(find-file "~/Documents/2.Notes/Org/todo.org")))
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
								 "  (%l/"
								 (:eval
								  (let ((win (selected-window)))
									(with-current-buffer (window-buffer win)
									  (if (or (eq ml-selected-window win) (null ml-total-lines))
										  (save-excursion
											(goto-char (point-max))
											(setq ml-total-lines (format-mode-line "%l")))
										ml-total-lines))))
								 "(%P):%C %I)  "
								 (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

(setq minor-mode-alist nil)

(server-start)

(provide 'init)
;;; init.el ends here
