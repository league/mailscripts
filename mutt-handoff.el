;;; mutt-handoff.el -- initialize a message buffer from a Mutt temp file.

;; Copyright ©2011 Christopher League

;; This is free software; you may redistribute and/or modify it under
;; the terms of the GNU General Public License, as published by the
;; Free Software Foundation, either version 2 of the license or (at
;; your option) any later version.

;;; Commentary:

;; The idea here is to use Mutt for reading and managing mail, and
;; Emacs message-mode for sending (not just composing, as usual). Mutt
;; creates its temporary file, and invoke emacsclient --no-wait on it.
;; We recognize the filename pattern in `find-file-hook' and set up a
;; standard message buffer.

;; If we are forwarding a message from Mutt with mime_forward=yes,
;; then we need to return to Mutt to include the attached message, and
;; let it handle sending. Otherwise, we let message-mode handle
;; attachments and sending.

;; Mutt settings:
;;   set editor = "emacsclient --no-wait"
;;   set edit_headers = yes
;;   set mime_forward = no
;;   set forward_decode = no

;; Emacs settings:
;;  (add-hook 'find-file-hook 'mutt-handoff-find-file)
;;  (autoload 'mutt-handoff "mutt-handoff")
;;  (defun mutt-handoff-find-file ()
;;    (when (string-match "\\bmutt-\\w+\\(-[[:xdigit:]]+\\)+" buffer-file-name)
;;      (mutt-handoff)))

;;; Code:

(require 'message)

(defvar mutt-handoff-begin-forward-regex
  "[ \n\t]*----- Forwarded message.* -----\n\n"
  "Regex matching Mutt's 'begin forwarded message annotation.")

(defvar mutt-handoff-end-forward-regex
  "\n\n----- End forwarded message -----\n"
  "Regex matching Mutt's 'end forwarded message' annotation.")

(defvar mutt-handoff-forward-using-mutt-from t
  "If true, use from address provided by Mutt when forwarding.")

(defvar mutt-handoff-forward-using-mutt-subject nil
  "If true, use subject provided by Mutt when forwarding.
Otherwise, set subject from `message-make-forward-subject-function'.")

(defun mutt-handoff ()
  "Top-level dispatcher to turn Mutt temporary file into new message buffer."
  (if (mutt-handoff-forwarded-p)
      (mutt-handoff-forward)
    (mutt-handoff-new)))

(defun mutt-handoff-forwarded-p ()
  "Determine whether Mutt file contains a forwarded message.
For this to work best, set mime_forward=no and forward_decode=no.
Then the raw text of the message is in the buffer and we can
MIME-include it from message-mode."
  (save-excursion
    (message-goto-body)
    (looking-at mutt-handoff-begin-forward-regex)))

(defun mutt-handoff-forward ()
  "Construct new message buffer for forwarding a message from Mutt."
  (let (from subject headers)
    (save-restriction
      (message-narrow-to-headers-or-head)
      (setq from (mutt-handoff-extract-header "From"))
      (setq subject (mutt-handoff-extract-header "Subject"))
      (setq headers (mutt-handoff-headers-as-alist)))
    (goto-char (point-min))
    (re-search-forward mutt-handoff-begin-forward-regex)
    (replace-match "")
    (goto-char (point-max))
    (re-search-backward mutt-handoff-end-forward-regex nil t)
    (replace-match "\n")
    (set-buffer-modified-p nil)         ; pretend nothing happened
    (message-forward)
    (save-excursion
      (when (and from mutt-handoff-forward-using-mutt-from)
        (message-replace-header "From" from))
      (when (and subject mutt-handoff-forward-using-mutt-subject)
        (message-replace-header "Subject" subject))
      (message-carefully-insert-headers headers))))

(defun mutt-handoff-new ()
  "Construct new message buffer for sending or replying to a Mutt message."
  (let (to subject headers body)
    (save-restriction
      (message-narrow-to-headers-or-head)
      (setq to (mutt-handoff-extract-header "To"))
      (setq subject (mutt-handoff-extract-header "Subject"))
      (setq headers (mutt-handoff-headers-as-alist)))
    (setq body (buffer-substring-no-properties (point-min) (point-max)))
    (set-buffer-modified-p nil)         ; pretend nothing happened
    (message-mail to subject headers)
    (save-excursion                     ; insert quoted body
      (message-goto-body)
      (newline)
      (insert body))))

(defun mutt-handoff-extract-header (field)
  "Remove header FIELD from buffer and return its value.
Handels continuation lines automatically. If FIELD is missing or
empty, return nil. Assumes buffer already narrowed to headers."
  (let ((value (message-fetch-field field)))
    (message-remove-header field)
    value))

(defun mutt-handoff-headers-as-alist ()
  "Remove remaining header fields and return (field . value) alist.
Omits empty headers. Assumes buffer is already narrowed to
headers."
  (goto-char (point-min))
  (let (alist)
    (while (looking-at "\\([-[:alpha:]]+\\): ")
      (let* ((field (match-string-no-properties 1))
             (value (mutt-handoff-extract-header field)))
        (when value
          (setq alist (cons (cons (intern field) value) alist))))
      (goto-char (point-min)))
    alist))

;;; mutt-handoff.el ends here
