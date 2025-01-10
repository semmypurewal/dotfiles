(require 'util)

(defun python-format-buffer ()
  "Run black and isort on the current buffer"
  (interactive)
  (if (derived-mode-p 'python-mode)
      (if (run-shell-commands-on-buffer-and-restore-point '("black -q" "isort"))
          (message "python-autoformat: success")
        (error "python-autoformat: failed"))))

(defun cpp-format-buffer ()
  "Run clang-format on the current buffer"
  (interactive)
  (if (run-shell-commands-on-buffer-and-restore-point '("clang-format -i"))
      (message "cpp-autoformat: success")
    (error "cpp-autoformat: failed")))

(defun js-format-buffer ()
  "Run prettier on the current buffer"
  (interactive)
  (if (run-shell-commands-on-buffer-and-restore-point '("prettier -w --parser typescript"))
      (message "js-autoformat: success")
    (error "js-autoformat: failed")))

(defun yaml-format-buffer ()
  "Run prettier on the current buffer"
  (interactive)
  (if (run-shell-commands-on-buffer-and-restore-point '("prettier -w --parser yaml"))
      (message "yaml-autoformat: success")
    (error "yaml-autoformat: failed")))

(provide 'autoformat)
