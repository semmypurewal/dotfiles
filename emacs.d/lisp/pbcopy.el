(require 'util)

(defun kill-ring-save-pbcopy (beg end)
  "Copy the region to the kill ring and the system clipboard using pbcopy."
  (interactive "r")
  ;; First, run pbcopy
  (run-shell-commands-on-region '("cat $file | pbcopy") nil)
  ;; Then, copy to the kill ring
  (kill-ring-save beg end))

(defun kill-region-pbcopy (beg end)
  "Cut the region to the kill ring and the system clipboard using pbcopy."
  (interactive "r")
  ;; First, run pbcopy
  (run-shell-commands-on-region '("cat $file | pbcopy") nil)
  ;; Then, cut the region
  (kill-region beg end))

(provide 'pbcopy)
