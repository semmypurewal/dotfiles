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
    (message "cpp-autoformat: failed")))

(defun js-format-buffer ()
  "Run prettier on the current buffer"
  (interactive)
  (if (run-shell-commands-on-buffer-and-restore-point '("prettier -w --parser typescript"))
      (message "js-autoformat: success")
    (message "js-autoformat: failed")))

(provide 'autoformat)
