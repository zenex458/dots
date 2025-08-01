;;; package --- Summary
;; This stores records your favourite songs, in a human readable plain text file, in a specific format (ARTIST -- SONG_NAME (feeling) -- dd/mm/yy hh:mm:ss).

;;; Commentary:

;; This is not part of GNU EMACS.

;;; Code:

;; -*- lexical-binding: t; -*-
;; TODO: make sure to add a command for at point in an emms playlist buffer
;; TODO: remove progn and make it more functinal, and make it customisible

(defun upmu-add-entry()
  "Add a music entry to a file."
  (interactive)
  (progn
	  (setq paths '("/home/zenex/Documents/Notes/hmb" "/home/zenex/Documents/Notes/THEBEST"))
	  (setq input-file (completing-read (message "Enter path: ") paths))

	  (setq song-meta-data '("Artist" "Song" "Description"))
	  (setq user-input '())

	  (with-temp-buffer
	    (insert-file-contents input-file)
	    (goto-char(point-min))
  	  (replace-regexp ".--.*" "")
	    (setq completition-artists (-uniq (split-string (buffer-string) "\n" t))))

    (with-temp-buffer
	    (insert-file-contents input-file)
	    (goto-char(point-min))
  	  (replace-regexp " -- [0-9].*" "")
	    (goto-char(point-min))
	    (replace-regexp "[a-z]+ -- " "")
	    (goto-char(point-min))
	    (replace-regexp " (.*)" "")
	    (setq completition-songs (-uniq (split-string (buffer-string) "\n" t))))

	  (with-temp-buffer
	    (insert-file-contents input-file)
	    (goto-char(point-min))
  	  (replace-regexp "[a-z]+ --.* (" "")
	    (goto-char(point-min))
	    (replace-regexp ").*" "")
	    (goto-char(point-min))
	    (replace-regexp "[a-z]+ --.*" "")
	    (setq completition-desc (-uniq (split-string (buffer-string) "\n" t))))

	  (defun takeinput (input)
	    (if (string= input "Artist")
		      (setq comp completition-artists))

	    (if (string= input "Song")
		      (setq comp completition-songs))

	    (if (string= input "Description")
		      (setq comp completition-desc))

	    (push (completing-read (format "Enter %s: " input) comp) user-input))

	  (mapc 'takeinput song-meta-data)

	  (with-temp-buffer (progn
						            (insert-file-contents input-file)
						            (goto-char(point-min))
						            (if (search-forward (format "%s -- %s" (nth 2 user-input) (nth 1 user-input)) nil t)
							              (message "already entered")
						              (append-to-file (message "%s -- %s (%s) -- %s\n" (nth 2 user-input) (nth 1 user-input) (nth 0 user-input) (format-time-string '"%d/%m/%y %T")) t input-file))))))

(provide 'upmu)
;;; upmu.el ends here
