;;;;https://github.com/LukeSmithxyz/voidrice/blob/master/.local/bin/mounter


;;iterate over the result and if `mount | $item' is null then show it to the user
;; (setq mounts '())
;; (with-temp-buffer
;;   (insert-file-contents '(shell-command-to-string "lsblk -P -o NAME,MOUNTPOINT | grep -o \"sd[b-z][0-9]\\|mmcblk0*[a-z][0-9]\""))
;;   (goto-char(point-min))
;;   (set-mark-command)
;;   (end-of-visual-line)
;;   (kill-ring-save)
;;   (push (yank-pop) mounts)
;;   )


;; (shell-command-to-string "lsblk -P -o NAME,MOUNTPOINT | grep MOUNTPOINT=\"\" | grep -o \"sd[b-z][0-9]\\|mmcblk0*[a-z][0-9]\"")

;; (shell-command-to-string "lsblk -P -o NAME,MOUNTPOINT | grep MOUNTPOINT=\"\" | grep -o \"sd[b-z][0-9]\"")



(setq mounts '())
(with-temp-buffer
  (insert-file-contents (shell-command-to-string "lsblk -P -o NAME,MOUNTPOINT")) ;;not insert-file-contents https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  (goto-char(point-min))
  (set-mark-command)
  (end-of-visual-line)
  (kill-ring-save)
  (push (yank-pop) mounts)
  )




(require 'transient)


(transient-define-prefix Transient ()
  "My Transient."
  ["Commands" ("l" "List" List :transient t)])

(defun List ()
  "List."
  (interactive)
  (progn
    (with-temp-buffer(shell-command "lsblk -P -o NAME,MOUNTPOINT | grep MOUNTPOINT=\"\" | grep -o \"sd[b-z][0-9]\"" "*test*"))))

(Transient)
