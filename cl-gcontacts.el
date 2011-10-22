(defun cl-gcontacts-insinuate ()
  (fset 'bbdb-complete-name 'cl-gcontacts-complete))

(defvar cl-gcontacts-mtime '(0 0))
(defvar cl-gcontacts-list nil)
(defvar cl-gcontacts-file "~/.google-contacts-completions.el")

(defun cl-gcontacts-load ()
  (message "Loading contacts...")
  (save-excursion
    (let ((buffer (find-file-noselect cl-gcontacts-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (setq cl-gcontacts-list (read (current-buffer)))
        t))))

(defun cl-gcontacts-maybe-load ()
  (let ((mtime (nth 5 (file-attributes cl-gcontacts-file))))
    (when (not (equal mtime cl-gcontacts-mtime))
      (cl-gcontacts-load)
      (setq cl-gcontacts-mtime mtime))))

(defun cl-gcontacts-complete (&optional start-pos)
  (interactive)
  (cl-gcontacts-maybe-load)
  (let* ((end (point))
         (beg (or start-pos
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
         (orig (buffer-substring beg end))
         (typed (downcase orig))
         (s (ido-completing-read "Recipient: " cl-gcontacts-list
                                 nil nil typed)))
    (delete-region beg end)
    (insert (get-text-property 0 'address s))))
