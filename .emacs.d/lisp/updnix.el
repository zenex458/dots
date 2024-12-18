;;; package --- Summary
;; This updates nixos so I don't have to open a terminal.

;;; Commentary:

;; This is not part of GNU EMACS.

;;; Code:

;; -*- lexical-binding: t; -*-

;; (defun sudo-shell-command (command)
;;   (interactive "MShell command (root): ")
;;   (with-temp-buffer
;;     (cd "/sudo::/")
;; 	(async-shell-command command)))
;; ;; (start-process "my-process" "*nix-output*" "ls" "-l" "/home/zenex")
;; 	;;; use `start-process' and bring the buffer `nix-output' focus forwards and search it, if it doesn't contain the word error, then do git stuff
;; ;; ))

;; (progn

;;   (setq proc (sudo-shell-command "nixos-rebuild --use-remote-sudo switch --flake /home/zenex/dots/.config/Nixos/#eukaryotic"))

;;   (message "oog::%s" (process-status proc))

;;   (if (eq proc "run")
;; 	  (message "true then")
;; 	(message "not true"))
;;   )


;; (start-process "my-process" "*nix-output*" "ls" "-l" "/home/zenex")


(defun zenex/sudo-shell-command (command)
  "Run COMMAND as root asynchronously."
  (interactive "MShell command (root): ")
  (with-temp-buffer
    ;; (cd "/sudo::/") ;;use this if you want to run the whole command as root
	(async-shell-command command)))
;; (start-process "my-process" "*nix-output*" "ls" "-l" "/home/zenex")
	;;; use `start-process' and bring the buffer `nix-output' focus forwards and search it, if it doesn't contain the word error, then do git stuff
;; ))

(defun updnix ()
  "Update nixos."
  (interactive)
  (zenex/sudo-shell-command "nh os switch"))


(provide 'updnix)
;;; updnix.el ends here
