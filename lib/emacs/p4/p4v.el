;;; p4v.el --- Simple Perforce-Emacs Integration

;;; Commentary:

;;    Programs for  Emacs <-> Perforce Integration.
;;    Copyright (C) 2011	Eric Warmenhoven
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;    If you have any problems to report, or suggestions, please send them
;;    to p4el-bugs@lists.sourceforge.net

;;; Code:

(require 'p4)

(defvar p4v-sort-dirs-at-top nil)

(defvar p4v-include-deleted-files nil)

(defvar p4v-mode-hook nil)

(defface p4v-header-face
  '((t (:inherit dired-header)))
  "Face for header."
  )

(defface p4v-dir-face
  '((t (:inherit dired-directory)))
  "Face for directories."
  )

(defvar p4v-places nil)

(defvar p4v-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-m" 'p4v-return)
    (define-key map "^" 'p4v-up)
    (define-key map "D" 'p4v-toggle-include-deleted)
    (define-key map "f" 'p4v-filelog)
    (define-key map "g" 'p4v-refresh)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'bury-buffer)
    (define-key map "s" 'p4v-toggle-sort)
    (define-key map "Q" 'p4v-quit)
    (define-key map "V" 'p4v-blame)
    (define-key map (kbd "RET") 'p4v-return)
    (define-key map [return] 'p4v-return)
    map))

(defun p4v-mode ()
  "browse p4

\\{p4v-mode-map}"
  (interactive)
  (if (get-buffer "*P4V*")
      (progn
        (switch-to-buffer "*P4V*")
        (p4v-refresh))
    (progn
      (switch-to-buffer "*P4V*")
      (kill-all-local-variables)
      (setq major-mode 'p4v-mode)
      (setq mode-name "P4V")
      (setq buffer-read-only t)
      (use-local-map p4v-mode-map)
      (run-hooks 'p4v-mode-hook)
      (make-local-variable 'p4v-path)
      (setq p4v-path "")
      (make-local-variable 'p4v-server)
      (setq p4v-server (p4-current-server-port))
      (p4v-redraw))))

(defun p4v-places-store ()
  (setq p4v-places (remove (assoc p4v-path p4v-places) p4v-places))
  (add-to-list 'p4v-places (cons p4v-path (buffer-substring (point-at-bol) (point-at-eol))) t))

(defun p4v-refresh ()
  (interactive)
  (p4v-change-path p4v-path))

(defun p4v-redraw ()
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))

  (let ((path p4v-path))
    (insert
     (with-temp-buffer
       ;; pull in all directory-like objects
       (if (string= path "")
           (progn
             (p4-exec-p4 (current-buffer) '("depots"))
             (goto-char (point-min))
             (while (re-search-forward "^Depot \\([^ ]+\\) .*" nil t)
               (replace-match "\\1" nil nil)))
         (progn
           (p4-exec-p4 (current-buffer)
                       (append '("dirs")
                               (if p4v-include-deleted-files
                                   '("-D"))
                               (list (concat "/" path "/*"))))
           (while (re-search-forward (concat "^/" path "/\\* - no such file(s)." (format "\n")) nil t)
             (replace-match  "" nil nil))))

       ;; set up dir face for all lines
       (goto-char (point-min))
       (while (not (eq (point) (point-max)))
         (add-text-properties (point-at-bol) (point-at-eol) '(face p4v-dir-face))
         (forward-line))

       ;; pull in all files
       (unless (string= path "")
         (progn
           (p4-exec-p4 (current-buffer) (list "files" (concat "/" path "/*")))
           (let ((p4v-re-pattern (concat "^/" path "/"
                                         (if p4v-include-deleted-files
                                             "\\* - no such file(s)."
                                           "\\(\\* - no such file(s).\\|.* - delete change .*\\)")
                                         (format "\n"))))
             (save-excursion
               (while (re-search-forward p4v-re-pattern nil t)
                 (replace-match "" nil nil))))
           (goto-char (point-at-bol))
           (while (re-search-forward "^\\([^#]+\\)#.*" nil t)
             (replace-match "\\1" nil nil))))

       (goto-char (point-min))
       (while (re-search-forward (concat "^/" path "/") nil t)
         (replace-match "" nil nil))
       (unless p4v-sort-dirs-at-top
         (sort-lines nil (point-min) (point-max)))
       (goto-char (point-min))
       (insert (format "/%s/...\n" path))
       (add-text-properties (point-min) (- (point) 1) '(face p4v-header-face))
       (buffer-string))))
  (goto-char (point-min))
  (forward-line)
  (if (assoc p4v-path p4v-places)
      (search-forward-regexp (concat "^" (cdr (assoc p4v-path p4v-places))) nil t))
  (goto-char (point-at-bol))
  (setq buffer-read-only t))

(defun p4v-return ()
  (interactive)
  (let ((cur-point (point))
        (thing (buffer-substring (point-at-bol) (point-at-eol))))
    (set-text-properties 0 (length thing) nil thing)
    (goto-char (point-at-bol))
    (if (equal (face-at-point) 'p4v-dir-face)
        (p4v-cd thing)
      (progn
        (if (not (equal (face-at-point) 'p4v-header-face))
            (progn
              (goto-char cur-point)
              (p4v-file thing))
          (goto-char cur-point))))))

(defun p4v-toggle-include-deleted ()
  (interactive)
  (setq p4v-include-deleted-files (not p4v-include-deleted-files))
  (p4v-refresh))

(defun p4v-toggle-sort ()
  (interactive)
  (setq p4v-sort-dirs-at-top (not p4v-sort-dirs-at-top))
  (p4v-refresh))

(defun p4v-change-path (path)
  (p4v-places-store)
  (let ((server (p4-current-server-port)))
    (if (string= p4v-server server)
        (setq p4v-path path)
      (progn
        (message "P4 Server has changed from %s to %s, returning to root" p4v-server server)
        (setq p4v-server server)
        (setq p4v-path ""))))
  (p4v-redraw))

(defun p4v-cd (path)
  (p4v-change-path (concat p4v-path "/" path)))

(defun p4v-up ()
  (interactive)
  (p4v-change-path (mapconcat 'identity (butlast (split-string p4v-path "/")) "/")))

(defun p4v-file (file)
  (p4-depot-find-file (concat "/" p4v-path "/" file)))

(defun p4v-filelog ()
  (interactive)
  (let ((cur-point (point)))
    (beginning-of-line)
    (if (and (not (equal (face-at-point) 'p4v-dir-face))
             (not (equal (face-at-point) 'p4v-header-face)))
        (progn
          (goto-char cur-point)
          (p4-file-change-log "filelog" (list (concat "/" p4v-path "/" (buffer-substring (point-at-bol) (point-at-eol))))))
      (goto-char cur-point))))

(defun p4v-blame ()
  (interactive)
  (let ((cur-point (point)))
    (beginning-of-line)
    (if (and (not (equal (face-at-point) 'p4v-dir-face))
             (not (equal (face-at-point) 'p4v-header-face)))
        (progn
          (goto-char cur-point)
          (p4-blame-int (concat "/" p4v-path "/" (buffer-substring (point-at-bol) (point-at-eol)))))
      (goto-char cur-point))))

(defun p4v-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'p4v)
