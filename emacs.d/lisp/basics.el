(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the
   buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill
   the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(provide 'basics)
