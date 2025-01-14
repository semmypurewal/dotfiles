;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

;; Custom file
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; This seems dangerous, but it's annoying to see byte compile
;; warnings pop up when installing packages.
(setq byte-compile-warnings nil)

;; Add my local lisp functions
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

;; System-specific config
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; General UI config
(display-time)
(column-number-mode t)
(global-auto-revert-mode t)
(menu-bar-mode -1)

(global-set-key (kbd "C-c l") 'display-line-numbers-mode)

(setq backup-directory-alist '(("." . "~/.emacs.d/.emacs-backups")))
(setq inhibit-startup-message t)

(setq-default show-trailing-whitespace t)
(setq-default require-final-newline t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook 'minibuffer-setup-hook
          (lambda () (setq show-trailing-whitespace nil)))

(when window-system
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(fido-vertical-mode 1)

(use-package emacs
  :hook
  (emacs-lisp-mode .
                   (lambda ()
                     (add-hook 'before-save-hook
                               (lambda ()
                                 (indent-region (point-min) (point-max)))
                               nil t))))

(use-package basics
  :bind
  (("C-c q" . 'close-and-kill-this-pane))
  (("C-c x" . 'close-and-kill-next-pane)))

(use-package project
  :config
  (setq project-switch-commands 'project-find-file)
  (add-to-list 'project-vc-extra-root-markers ".sl"))

;; IDE Stuff
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(web-js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))

  :hook
  (web-js-mode . eglot-ensure)
  (web-html-mode . eglot-ensure)
  (web-css-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (rust-mode . eglot-ensure)

  :ensure t)

(use-package company
  :after eglot
  :hook
  (eglot-managed-mode . company-mode)
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

  :ensure t)

(use-package rust-mode
  :mode "\\.rs"
  :ensure t)

;; Autoformatters
(use-package autoformat
  :hook
  (python-mode .
               (lambda ()
                 (add-hook 'before-save-hook
                           #'python-format-buffer nil t)))
  (c-mode-common .
                 (lambda ()
                   (add-hook 'before-save-hook
                             #'cpp-format-buffer nil t)))

  (rust-mode .
             (lambda ()
               (add-hook 'before-save-hook
                         #'rust-format-buffer nil t)))

  (web-js-mode .
               (lambda ()
                 (add-hook 'before-save-hook
                           #'js-format-buffer nil t)))

  (yaml-mode .
             (lambda ()
               (add-hook 'before-save-hook
                         #'yaml-format-buffer nil t)))
  )

(use-package pbcopy
  :config
  (global-set-key (kbd "M-w") 'kill-ring-save-pbcopy)
  (global-set-key (kbd "C-w") 'kill-region-pbcopy))

(use-package cc-mode
  :init
  (setq c-default-style "my-c-style")

  :config
  (c-add-style "my-c-style" '((c-tab-always-indent . t)
                              (c-basic-offset . 4)
                              (c-offsets-alist . ((access-label . -2)
                                                  (label . +)))))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "C-x C-k") 'ff-get-other-file))))


;; Misc
(use-package yaml-mode
  :mode
  ("\\.yaml" . yaml-mode)
  ("\\.yml" . yaml-mode)
  :ensure t)

(use-package glsl-mode
  :mode
  ("\\.vert" . glsl-mode)
  ("\\.frag" . glsl-mode)
  :ensure t)

