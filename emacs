(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; clear the buffer in eshell
;; src: http://daily-emacs.blogspot.com/2011/11/clear-in-eshell.html
(defun eshell/clear ()
  "clear the eshell buffer."
  ;; figure out if we are actually in the eshell buffer
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
    ;; figure out how to insert a new line
)

(display-time)
(eshell)

