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

;;(setq custom-file (make-temp-file "emacs-custom-"))
(defvar ispell-dictionary "british")
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default dired-listing-switches "-Alh --group-directories-first")
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
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(add-hook 'prog-mode-hook #'(lambda ()
							  (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'prog-mode-hook  #'(lambda ()(setq show-trailing-whitespace t)))
(setq display-time-default-load-average nil)
(setq display-time-format "%I:%M")
(display-time-mode 1)
(display-battery-mode 1)
(global-subword-mode 1)
(pending-delete-mode t)
(savehist-mode 1)

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

(setq electric-pair-pairs '(
							(?\{ . ?\})
							(?\( . ?\))
							(?\[ . ?\])
							(?\" . ?\")
							))

(add-hook 'prog-mode-hook 'electric-pair-mode)


(use-package diminish)

(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-resize nil))

(use-package orderless
  ;;  :custom
  ;;  (completion-category-overrides '((file (styles basic partial-completion))))
  ;;  (completion-styles '(basic substring initials flex orderless)))
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

(use-package format-all
  :hook ((prog-mode . format-all-mode)
		 (format-all-mode . format-all-ensure-formatter))
  :config
  (diminish 'format-all-mode))
(add-hook 'cshap-mode-hook (setq format-all-formatters '(("C#" (astyle "--mode=cs" "--style=whitesmith")))))


(use-package pulsar
  :hook ((next-error . pulsar-pulse-line)
         (minibuffer-setup . pulsar-pulse-line)))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (diminish 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package haskell-mode
  :magic ("%hs" . haskell-mode)
  :hook ((haskell-mode . haskell-doc-mode)
		 (haskell-mode . turn-on-haskell-indent)))

(use-package elpy
  :mode ("*\\.py\\'" . elpy-mode))

;;(use-package corfu
;;  :custom
;;  (setq completion-cycle-threshold 3)
;;  (corfu-auto t)
;;  (corfu-cycle t)
;;  (setq corfu-quit-no-match 'separator)
;;  (corfu-preselect 'prompt)
;;  (setq corfu-popupinfo-delay 1)
;;  :hook ((prog-mode . corfu-mode)
;;		 (prog-mode . corfu-popupinfo-mode)))
;;
;;(use-package cape
;;  :bind (("C-c p w" . cape-dict)
;;		 ("C-c p l" . cape-line))
;;  :init
;;  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;  (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;  (add-to-list 'completion-at-point-functions #'cape-file)
;;  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;  (add-to-list 'completion-at-point-functions #'cape-history)
;;  (add-to-list 'completion-at-point-functions #'cape-keyword)
;;  (add-to-list 'completion-at-point-functions #'cape-line)
;;  (setq-local completion-at-point-functions
;;			  (mapcar #'cape-company-to-capf
;;					  (list #'company-files #'company-keywords #'company-dabbrev))))
;;

;;(use-package company
;;  :hook ((prog-mode . company-mode)))


(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  ;;  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :hook ((prog-mode . corfu-mode)
  		 (prog-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil)))


(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (setq-local completion-at-point-functions
			  (mapcar #'cape-company-to-capf
					  (list #'company-files #'company-keywords #'company-dabbrev))))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;;  (setq lsp-csharp-omnisharp-roslyn-server-path "/nix/store/2w8wkhkhd0b85fz48pnsds5msy31iyhm-omnisharp-roslyn-1.39.10/bin/OmniSharp")
  :hook ((csharp-mode . lsp)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)


(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
		 (org-mode . yas-minor-mode))
  :config
  (add-hook 'prog-mode-hook  #'(lambda ()(yas-reload-all)))
  (diminish 'yas-minor-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

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

(use-package expand-region
  :bind ("C-@" . er/expand-region)) ;;add other binds

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
  :config
  (elfeed-org))

(use-package dired-subtree
  :init
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "TAB") #'dired-subtree-cycle)
	(define-key map (kbd "M-^") #'dired-subtree-remove)))

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

(require 'dired)
(let ((map dired-mode-map))
  (define-key map (kbd "TAB") #'dired-subtree-toggle))
;;(global-set-key [C-backspace] 'hungry-delete-backward)
;;(global-set-key [C-delete] 'hungry-delete-forward)
(global-set-key (kbd "C-c t") 'edit-todo)
(global-set-key (kbd "C-c l a c") 'avy-copy-line)
(global-set-key (kbd "C-c a k") 'avy-kill-whole-line)
(global-set-key (kbd "C-c l a m") 'avy-move-line)
(global-set-key (kbd "C-c l a g") 'avy-goto-char)
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
								 ":%C %I)  "
								 (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

(setq minor-mode-alist nil)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
