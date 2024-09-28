(defun line-length ()
  "Return the length of the current line."
  (save-excursion (end-of-line) (current-column)))

(defun move-to-line-and-col (line col)
  "Move to the specified line and column if it exists.

If the line exists, but it's not long-enough, move to the specified
line and the last column in that line. If the line does not exist,
move to point-max."
  (goto-line line)
  (if (> col (line-length)) (end-of-line) (move-to-column col)))

(defun create-temp-file-in-default-directory ()
  "Create a temporary file in the default directory"
  (let ((temp-file (make-temp-file "autoformat-" nil ".tmp")))
    (let ((new-path (expand-file-name (file-name-nondirectory temp-file) default-directory)))
      (rename-file temp-file new-path)
      (message "Created temp file '%s'" new-path)
      new-path)))

(defun run-shell-command-on-file (cmd file)
  "Run shell command on file and return t on success, nil on failure"
  (let ((full-command (concat cmd " " file)))
    (message "Attempting to run '%s'" full-command)
    (let ((result (shell-command full-command)))
      (message "result: %s" result)
      (eq result 0))))

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
  (let ((temp-file (create-temp-file-in-default-directory))
        (original-point (point)))
    (write-region (point-min) (point-max) temp-file nil 'silent)
    (unwind-protect
        (if (run-shell-commands-on-file cmds temp-file)
            (progn
              (erase-buffer)
              (insert-file-contents temp-file)
              (goto-char (min original-point (point-max)))
              t)
          nil)
      (delete-file temp-file))))

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
