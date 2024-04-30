;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(defun package--save-selected-packages (&rest opt) nil)

;; custom file
(setq custom-file "custom.el")
(load custom-file)

;; System-specific config
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; General UI config
(display-time)
(column-number-mode t)
(global-auto-revert-mode t)
(menu-bar-mode -1)

(setq backup-directory-alist '(("." . "~/.emacs.d/.emacs-backups")))
(setq linum-format "%4d\u2502")
(setq inhibit-startup-message t)
(setq show-trailing-whitespace t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook 'minibuffer-setup-hook
          (lambda () (setq show-trailing-whitespace nil)))

(when window-system
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;; IDO
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)

  :ensure t)

;; Projectile
(use-package projectile
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-x C-j") 'projectile-find-file)

  :ensure t)

;; IDE Stuff
(use-package eglot
  :config
  ;; npm install -g vscode-langservers-extracted
  ;; (add-to-list 'eglot-server-programs '(web-html-mode . ("vscode-html-language-server" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(web-css-mode . ("vscode-css-language-server" "--stdio")))

  ;; npm install -g typescript-language-server typescript
  (add-to-list 'eglot-server-programs '(web-js-mode . ("typescript-language-server" "--stdio")))

  :hook
  (web-js-mode . eglot-ensure)
  (web-html-mode . eglot-ensure)
  (web-css-mode . eglot-ensure)

  :ensure t)

(use-package company
  :after eglot
  :hook
  (eglot-managed-mode . company-mode)
  :ensure t)

;; Front End Dev Config
 (use-package prettier-js
  :ensure t)

(use-package web-mode
  :init
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (define-derived-mode web-js-mode web-mode "web-js-mode")
  (define-derived-mode web-html-mode web-mode "web-html-mode")
  (define-derived-mode web-css-mode web-mode "web-css-mode")

  :mode
  (("\\.js" . web-js-mode)
   ("\\.mjs" . web-js-mode)
   ("\\.jsx" . web-js-mode)
   ("\\.ts" . web-js-mode)
   ("\\.tsx" . web-js-mode)
   ("\\.json" . web-js-mode)
   ("\\.html" . web-html-mode)
   ("\\.css" . web-css-mode))

  :hook
  (web-js-mode . prettier-js-mode)

  :ensure t)

;; Python
(use-package blacken
  :config
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (add-hook 'before-save-hook
                                  'blacken-buffer nil t))))
  :ensure t)

(use-package clang-format
  :config
  (add-hook 'c-mode-common-hook
            (function (lambda ()
                        (add-hook 'before-save-hook
                                  'clang-format-buffer nil t))))
  :ensure t)

;; C/C++
(c-add-style "my-c-style" '((c-tab-always-indent . t)
                            (c-basic-offset . 4)
                            (c-offsets-alist (access-label . -2)
                                             (label . +))))
(setq c-default-style "my-c-style")


;; Misc
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

  :ensure t)

(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vert" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag" . glsl-mode))

  :ensure t)


