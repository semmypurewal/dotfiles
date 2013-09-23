(add-to-list 'load-path "~/.emacs.d/lib/")

;; add subdirectories of the lib directory
(let ((default-directory "~/.emacs.d/lib/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'p4)
(require 'p4v)
(require 'js2-mode)
(require 'git)
(require 'transpose-frame)
(require 'json)

;; some simple keybindings
(global-set-key (kbd "M-g")   'goto-line)
(global-set-key (kbd "C-x )") 'rotate-frame-clockwise)
(global-set-key (kbd "C-x (") 'rotate-frame-anticlockwise)

;; turn off some annoying 'features'
(setq inhibit-startup-message t)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; sometimes i have to look at compiled JS files (gross)
(setq line-number-display-limit-width 1000)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; clear the buffer in eshell
;; src: http://daily-emacs.blogspot.com/2011/11/clear-in-eshell.html
(defun eshell/clear ()
  "clear the eshell buffer."
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

(server-start)

(eshell)
