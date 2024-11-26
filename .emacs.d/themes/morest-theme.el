;;; morest-theme.el --- Theme

;; Copyright (C) 2023 , zenex

;; Author: zenex
;; Version: 0.3
;; Package-Requires: ((emacs "24"))
;; Created with ThemeCreator, https://github.com/mswift42/themecreator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; This was inspired by https://github.com/guidoschmidt/emacs-nyx-theme.

;;; Code:

;;TODO: figure out how to change the orderless matching colours, look at `modus-vivendi' for how to do it.

(deftheme morest)
(let ((class '((class color) (min-colors 89)))
      (fg1 "#bdae93") ;;c6c6c6
      (fg2 "#b3a283") ;;b6b6b6
      (fg3 "#aa9673") ;;a6a6a6
      (fg4 "#a08a64") ;;969696
      (bg1 "#212121")
      (bg2 "#2e2e2e")
      (bg3 "#414141")
      (bg4 "#555555")
      (builtin "#aaaaff")
      (keyword "#B33929")
      (const   "#c0c000")
      (comment "#5F5F5F")
      (func    "#BD8700")
      (str     "#75B329")
      (type    "#2874B2")
      (var     "#802caa") ;;6729B3
      (warning "#ff0000")
      (warning2 "#e67300"))
  (custom-theme-set-faces
   'morest
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func ))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(term-color-black ((,class (:foreground ,fg2 :background nil))))
   `(region ((,class (:background ,"#888888" :foreground ,"#000000"))))
   `(highlight ((,class (:foreground ,fg2 :background ,bg4))))
   `(hl-line ((,class (:background  ,bg3))))
   `(fringe ((,class (:background ,bg1 :foreground ,builtin))))
   `(cursor ((,class (:background ,keyword))))
   `(isearch ((,class (:bold :foreground ,bg1 :background ,fg1))))
   `(mode-line ((,class (:foreground ,fg4 :background ,bg1))))
   `(mode-line-inactive ((,class (:foreground ,"#000000" :background ,"#000000" :weight normal)))) ;;foreground, comment
   `(mode-line-buffer-id ((,class (:bold :foreground ,fg1 :background nil))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))
   `(vertical-border ((,class (:foreground ,bg4))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,comment))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,const :underline t))))
   `(org-code ((,class (:foreground ,fg2))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:bold t :foreground ,"#6cb2eb"))))
   `(org-level-2 ((,class (:bold t :foreground ,builtin))))
   `(org-level-3 ((,class (:bold t :foreground ,"#6cb2eb"))))
   `(org-level-4 ((,class (:bold t :foreground ,builtin))))
   `(org-level-5 ((,class (:bold t :foreground ,"#6cb2eb"))))
   `(org-level-6 ((,class (:bold t :foreground ,builtin))))
   `(org-level-7 ((,class (:bold t :foreground ,"#6cb2eb"))))
   `(org-level-8 ((,class (:bold t :foreground ,builtin))))
   `(org-document-info-keyword ((,class (:bold t :foreground ,keyword))))
   `(org-document-title ((,class (:bold t :foreground ,fg1))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-link ((,class (:underline t :foreground ,comment ))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-block ((,class (:foreground ,fg3))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))))
   `(org-done ((,class (:box (:line-width 1 :color ,bg3) :bold t :foreground ,bg4))))
   `(org-warning ((,class (:underline t :foreground ,warning))))
   `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
   `(org-agenda-done ((,class (:foreground ,bg4))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,var :italic t))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(ido-only-match ((,class (:foreground ,warning))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(ido-first-match ((,class (:foreground ,keyword :bold t))))
   `(ivy-current-match ((,class (:foreground ,fg3 :inherit highlight :underline t))))
   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))
   `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
   `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
   `(mu4e-header-marks-face ((,class (:foreground ,type))))
   `(ffap ((,class (:foreground ,fg4))))
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-member ((,class (:foreground ,fg3))))
   `(js3-warning-face ((,class (:underline ,keyword))))
   `(js3-error-face ((,class (:underline ,warning))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,fg2))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-instance-member-face ((,class (:foreground ,const))))
   `(warning ((,class (:foreground ,warning))))
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(trailing-whitespace ((,class :foreground unspecified :background ,warning)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,func)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,str)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,var)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,builtin)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,"#0073e6")))
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-section-highlight      ((,class (:background ,bg2))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
   `(term-color-blue ((,class (:foreground ,func :background ,func))))
   `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
   `(term-color-green ((,class (:foreground ,type :background ,bg3))))
   `(term-color-yellow ((,class (:foreground ,var :background ,var))))
   `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
   `(term-color-cyan ((,class (:foreground ,str :background ,str))))
   `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,var))))
   `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,keyword))))
   `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
   `(company-tooltop-annotation ((,class (:foreground ,const))))
   `(company-tooltip-common ((,class ( :foreground ,fg3))))
   `(company-tooltip-common-selection ((,class (:foreground ,str))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
   `(company-template-field ((,class (:inherit region))))
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
   `(jde-java-font-lock-package-face ((t (:foreground ,var))))
   `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,fg2))))
   `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-number-face ((t (:foreground ,var))))
   `(yas-field-highlight-face ((t (:background ,bg4))))
   `(markdown-header-face-1 ((t (:bold t :foreground ,keyword))))
   `(markdown-header-face-2 ((t (:bold t :foreground ,func))))
   `(markdown-header-face-3 ((t (:bold t :foreground ,const))))
   `(markdown-header-face-4 ((t (:bold t :foreground ,str))))
   `(markdown-header-face-5 ((t (:bold t :foreground ,var))))
   `(markdown-header-face-6 ((t (:bold t :foreground ,builtin))))
   `(sh-quoted-exec ((t (:foreground , builtin))))
   `(dired-directory ((t (:foreground , "#6cb2eb"))))
   `(diredp-dir-name ((t (:foreground , "#6cb2eb"))))
   `(diredp-symlink ((,class (:foreground ,fg4))))
   `(diredp-file-name ((,class (:foreground ,fg1))))
   `(diredp-date-time ((,class (:foreground ,fg1))))
   `(diredp-number ((,class (:foreground ,fg1))))
   `(diredp-file-suffix ((,class (:foreground ,fg1))))
   `(diredp-executable-tag ((,class (:foreground ,str))))
   `(diredp-exec-priv ((,class (:foreground ,str))))
   `(diredp-read-priv ((,class (:foreground ,keyword))))
   `(diredp-write-priv ((,class (:foreground ,const))))
   `(diredp-no-priv ((,class (:foreground ,fg1))))
   `(diredp-dir-priv ((t (:foreground , "#6cb2eb"))))
   `(diredp-link-priv ((,class (:foreground ,fg4))))
   `(diredp-dir-heading ((,class (:foreground ,fg1))))
   `(diredp-compressed-file-name ((,class (:foreground ,"#a52a2a"))))
   `(diredp-compressed-file-suffix ((,class (:foreground ,"#a52a2a"))))
   `(marginalia-file-priv-exec ((,class (:foreground ,str))))
   `(marginalia-file-priv-read ((,class (:foreground ,keyword))))
   `(marginalia-file-priv-write ((,class (:foreground ,const))))
   `(marginalia-file-priv-dir ((t (:foreground , "#6cb2eb"))))

   ;;  `(markdown-header-face-6 ((t (:bold t :foreground "#0073e6"))))
   ;; `(sh-quoted-exec ((t (:foreground "#0073e6"))))
   ;; `(dired-directory ((t (:foreground "#0073e6"))))
   ;; `(flymake-error-echo ((t (:bold t :foreground ,warning))))
   ;; `(flymake-warning-echo ((t (:bold t :foreground ,warning2))))
   '(flymake-warning ((((class color)) :underline (:color "#e67300" :style wave))))
   `(compilation-error ((t (:bold t :foreground ,warning))))
   `(compilation-warning ((t (:bold t :foreground ,warning2))))

   )
  ;; Legacy
  (if (< emacs-major-version 22)
      (custom-theme-set-faces
	   'morest
	   `(show-paren-match-face ((,class (:background ,warning)))) ;; obsoleted in 22.1, removed 2016
	   )
	(custom-theme-set-faces
	 'morest
	 `(show-paren-match ((,class (:foreground ,bg1 :background ,str))))
	 `(show-paren-mismatch ((,class (:foreground ,bg1 :background ,warning))))
	 )
	)
  ;; emacs >= 26.1
  (when (>= emacs-major-version 26)
	(custom-theme-set-faces
	 'morest
	 `(line-number ((t (:inherit fringe))))
	 `(line-number-current-line ((t (:inherit fringe :foreground "white" :weight bold))))))
  ;; emacs >= 27.1
  (when (>= emacs-major-version 27)
	(custom-theme-set-faces
	 'morest
	 `(tab-line              ((,class (:background ,bg2 :foreground ,fg4))))
	 `(tab-line-tab          ((,class (:inherit tab-line))))
	 `(tab-line-tab-inactive ((,class (:background ,bg2 :foreground ,fg4))))
	 `(tab-line-tab-current  ((,class (:background ,bg1 :foreground ,fg1))))
	 `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg2))))))
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
			   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'morest)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; morest-theme.el ends here
