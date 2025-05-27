;;; package --- Summary
;; This updates Nixos so I don't have to open a terminal.

;;; Commentary:

;; This is not part of GNU EMACS.

;;; Code:

;; -*- lexical-binding: t; -*-

(defun zenex/sudo-shell-command (command)
  "Run COMMAND as root asynchronously."
  (interactive "MShell command (root): ")
  (with-temp-buffer
    (cd "/sudo::/")
	  (async-shell-command command "*nixupd*"
                         )))

(defun updnix ()
  "Update nixos."
  (interactive)
  (zenex/sudo-shell-command "nixos-rebuild --use-remote-sudo switch --flake /home/zenex/Dev/dots/.config/nixos/#eukaryotic"))

(defun updnix-kill-buffers ()
  "Kill updnix related buffers."
  (interactive)
  (kill-buffer "*nixupd*")
  (message "Killed buffers."))

(provide 'updnix)
;;; updnix.el ends here
