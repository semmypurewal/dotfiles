(package-initialize)

(require 'subr-x)

(load-theme 'wheatgrass)
(display-time)

(column-number-mode t)
(global-auto-revert-mode t)
(ido-mode 1)
(ido-vertical-mode 1)
(menu-bar-mode -1)
(projectile-mode t)

(when window-system
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(add-to-list 'auto-mode-alist '("\\.js" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjs" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json" . js-mode))
(add-to-list 'auto-mode-alist '("\\.yaml" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

(setq js-indent-level 2)

(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(setq linum-format "%4d\u2502 ")

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq inhibit-startup-message t)

(setq web-mode-code-indent-offset 2)

(setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Git Mtn)))

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-x C-j") 'projectile-find-file)

(setq-default indent-tabs-mode nil)

(set-frame-parameter nil 'fullscreen 'fullboth)

(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'c-mode-common-hook
          (function (lambda ()
                    (add-hook 'before-save-hook
                              'clang-format-buffer nil t))))
(add-hook 'python-mode-hook
          (function (lambda ()
                    (add-hook 'before-save-hook
                              'blacken-buffer nil t))))

(add-hook 'minibuffer-setup-hook
          (lambda () (setq show-trailing-whitespace nil)))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode blacken ido-vertical-mode clang-format json-reformat-region web-mode prettier-js)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
