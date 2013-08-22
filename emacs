(add-to-list 'load-path "~/.emacs.d/lib/")

(load-library "p4")
(load-library "p4v")
(load-library "js2-mode")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;; clear the buffer in eshell
;; src: http://daily-emacs.blogspot.com/2011/11/clear-in-eshell.html
(defun eshell/clear ()
  "clear the eshell buffer."
  ;; figure out if we are actually in the eshell buffer
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
)

;; spaces 4-ever
(setq-default indent-tabs-mode nil)

;; show date and time
(setq display-time-day-and-date t)
(display-time)

;; hide menu bar
(menu-bar-mode -1)

(eshell)

