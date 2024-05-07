(defun create-temp-file-in-default-directory ()
  "Create a temporary file in the default directory"
  (let ((temp-file (make-temp-file "temp" nil ".tmp")))
    (let ((new-path (expand-file-name (file-name-nondirectory temp-file) default-directory)))
      (rename-file temp-file new-path)
      new-path)))

(defun run-shell-command-on-file (cmd file)
  "Run shell command on file and return t on success, nil on failure"
  (eq (shell-command (concat cmd " " file)) 0))

(defun run-shell-commands-on-file (cmds file)
  "Run a sequence of shell commands on a file and return t on success,
   nil on failure."
  (if (eq (null cmds) nil)
      (let ((result (run-shell-command-on-file (car cmds) file)))
        (if result
            (run-shell-commands-on-file (cdr cmds) file)
          (progn
            (message "%s failed on buffer" (car cmds))
            result)))
      t))

(defun run-shell-commands-on-buffer (cmds)
  "Run a sequence of shell commands on a buffer and return t on
   success, nil on failure."
  (let ((temp-file (create-temp-file-in-default-directory)))
    (write-region (point-min) (point-max) temp-file)
    (if (run-shell-commands-on-file cmds temp-file)
        (progn
          (erase-buffer)
          (insert-file-contents temp-file)
          (delete-file temp-file)
          t)
      (progn
        (delete-file temp-file)
        nil))))

(defun python-format-buffer ()
  "Run black and isort on the current buffer"
  (interactive)
  (if (derived-mode-p 'python-mode)
      (if (run-shell-commands-on-buffer '("black -q" "isort"))
          (message "python-autoformat: success")
        (error "python-autoformat: failed"))))

(defun cpp-format-buffer ()
  "Run clang-format on the current buffer"
  (interactive)
  (if (run-shell-commands-on-buffer '("clang-format -i"))
      (message "cpp-autoformat: success")
    (message "cpp-autoformat: failed")))

(defun js-format-buffer ()
  "Run prettier on the current buffer"
  (interactive)
  (if (run-shell-commands-on-buffer '("prettier -w --parser typescript"))
      (message "js-autoformat: success")
    (message "js-autoformat: failed")))

(provide 'autoformat)
