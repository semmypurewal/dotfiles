(add-to-list 'load-path "~/.emacs.d/lib/")

;; add subdirectories of the lib directory
(let ((default-directory "~/.emacs.d/lib/"))
  (normal-top-level-add-subdirs-to-load-path))

;;(require 'git)
;;(require 'git-blame)

;;(load-library "git")
;;(load-library "git-blame")

(require 'p4)
(require 'p4v)
(require 'js2-mode)
(require 'git)

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

;; ido mode on
(ido-mode t)

(eshell)

