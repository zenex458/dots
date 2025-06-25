(defun emms-random-tr (&optional arg)
  "Goto random track and add it to the queue, if a ARG is added then it will add a track ARG times."
  (interactive "p")
  (emms-browser-expand-all)
  (if (eq arg nil)
	  (progn
		(emms-browser-goto-random)
		(emms-browser-add-tracks))
	;;inc is the counter, and return inc, otherwise it will return nil
	(let ((inc 0))
	  (while (< inc arg)
		(emms-browser-goto-random)
		(emms-browser-add-tracks)
		(setq inc (1+ inc)))
	  inc
	  )
	))

(emms-random-tr 10)
