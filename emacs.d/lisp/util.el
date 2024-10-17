(defun get-text-of-line (line)
  "Get the text of the specified line."
  (save-excursion
    (message "going to line %s" line)
    (goto-line line)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun get-word-at-point ()
  "Get the word at the current point."
  (thing-at-point 'word t))

(defun search-nearest (text)
  "Search for the nearest occurrence of TEXT and return its position, either forward or backward."
  (let* ((case-fold-search nil)
         (initial-point (point))
         (forward-result (save-excursion (word-search-forward text nil t)))
         (back-result (save-excursion (word-search-backward text nil t)))
         (forward-delta (if forward-result (- forward-result initial-point) nil))
         (backward-delta (if back-result (- initial-point back-result) nil)))

    (cond
     ((and forward-delta backward-delta)
      (if (< forward-delta backward-delta)
          forward-result
        back-result))
     (forward-delta forward-result)
     (backward-delta back-result))))

(defun line-length ()
  "Return the length of the current line."
  (save-excursion (end-of-line) (current-column)))

(defun get-current-center-line ()
  (let* ((window-midpoint (/ (window-body-height) 2))
         (center-line (line-number-at-pos (window-start) t)))
    (+ center-line window-midpoint)))

(defun restore-center-line (center-line)
  (goto-line center-line)
  (recenter))

(defun move-to-line-and-col (line col)
  "Move to the specified line and column if it exists.

If the line exists, but it's not long-enough, move to the specified
line and the last column in that line. If the line does not exist,
move to point-max."
  (goto-line line)
  (if (> col (line-length)) (end-of-line) (move-to-column col)))

(defun text-to-point ()
  "Get the text up to the point in the current buffer."
  (buffer-substring-no-properties (point-min) (point)))

(defun text-after-point ()
  "Get the text after the point in the current buffer."
  (buffer-substring-no-properties (+ (point) 1) (point-max)))

(defun create-temp-file-in-default-directory ()
  "Create a temporary file in the default directory"
  (let ((temp-file (make-temp-file "autoformat-" nil ".tmp")))
    (let ((new-path (expand-file-name (file-name-nondirectory temp-file) default-directory)))
      (rename-file temp-file new-path)
      (message "Created temp file '%s'" new-path)
      new-path)))

(defun run-shell-command-on-file (cmd file)
  "Run shell command on file and return t on success, nil on failure.
   If CMD contains the '$file' placeholder, it will be replaced with the FILE path.
   Otherwise, the FILE path will be appended to CMD."
  (let ((full-command (if (string-match-p "\\$file" cmd)
                          (replace-regexp-in-string "\\$file" file cmd)
                        (concat cmd " " file))))
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

(defun run-shell-commands-on-region (cmds &optional reformat entire-buffer)
  "Run a sequence of shell commands on the selected region, or the whole
   buffer if ENTIRE-BUFFER is set to t.
   Return t on success, nil on failure.
   If REFORMAT is true, the region is erased and replaced with the command's output.
   The region is restored if it was active before the operation."
  (let* ((temp-file (create-temp-file-in-default-directory))
         ;; If entire-buffer is t, use the entire buffer
         (region-active (and (use-region-p) (not entire-buffer)))
         (region-min (if region-active (region-beginning) (point-min)))
         (region-max (if region-active (region-end) (point-max)))
         (reformat (if (null reformat) reformat t))
         result)  ;; Declare a result variable to capture the success/failure

    ;; Write the selected region (or whole buffer) to a temp file
    (write-region region-min region-max temp-file nil 'silent)
    (unwind-protect
        (setq result
              (if (run-shell-commands-on-file cmds temp-file)
                  (progn
                    ;; Only erase and reformat the region if reformat is true
                    (when reformat
                      (delete-region region-min region-max)
                      (goto-char region-min)
                      (insert-file-contents temp-file))
                    t)  ;; return success
                nil))  ;; return failure
      ;; Always delete the temp file, even if an error occurs
      (delete-file temp-file))

    ;; Only restore the region if it was active before the operation
    (when (and region-active (not (equal region-min region-max)))
      (goto-char region-max))

    ;; Explicitly return the result
    result))

(defun restore-point-after (func &rest args)
  "Execute the input FUNC which may modify the buffer, and then attempt to restore the point using some straightforward heuristics."
  (let* ((line (+ (current-line) 1))
         (col (current-column))
         (pre-to-point (text-to-point))
         (line-at-point (get-text-of-line line))
         (word-at-point (get-word-at-point)))
    (progn
      (apply func args)
      (move-to-line-and-col line col)
      (let* ((post-to-point (text-to-point)))
        (if (string= pre-to-point post-to-point)
            (message "No change up to point, leave it where it is.")
          (progn
            (message "Change before point, searching for line '%s'." line-at-point)
            (let* ((line-pos (search-nearest line-at-point)))
              (if line-pos
                  (progn
                    (message "found '%s' at char %s, restoring point." line-at-point line-pos)
                    (message "going to line %s" line-pos)
                    (goto-char line-pos)
                    (message "goint to beginning of line plus col")
                    (goto-char (+ (line-beginning-position) col)))
                ;; (goto-char (+ line-pos col)))
                (progn
                  (message "'%s' not in buffer, trying word '%s' instead" line-at-point word-at-point)
                  (let* ((word-pos (search-nearest word-at-point)))
                    (message "word-pos: %s" word-pos)
                    (if word-pos
                        (progn
                          (message "found '%s' at char %s, restoring point." word-at-point word-pos)
                          (goto-char word-pos))
                      (progn
                        (message "Line not found, word not found. Falling back to original line and column")
                        (move-to-line-and-col line col)))))))))))))

(defun run-shell-commands-on-buffer-and-restore-point (cmds)
  "Run shell commands on the buffer, restore the point and return the result."
  (let ((center-line (get-current-center-line))
        result)  ;; Declare a variable to store the result
    (setq result (restore-point-after #'run-shell-commands-on-region cmds t t))
    (save-excursion
      (restore-center-line center-line))
    result))


(provide 'util)
