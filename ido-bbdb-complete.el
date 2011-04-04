;;; ido-bbdb-complete.el -- a smarter bbdb-complete-name based on ido

;; Show completions as:
;;   (1) full name <email>  if it's a primary email
;;   (2) email (full name)  if it's not primary
;;   (3) aka (full name)
;;   (4) mail-alias [N recipients]

(require 'bbdb)
(require 'ido)

(defun ido-bbdb-alt-address (record email/aka)
  "Produce string 'email (full name)' for non-primary address."
  (concat email/aka " (" (bbdb-record-name record) ")"))

(defun ido-bbdb-address-props (record &optional string email)
  "Annotate STRING with address based on EMAIL.
If STRING is missing, use `bbdb-dwim-net-address' on RECORD. If
EMAIL is missing, use primary net address from RECORD."
  (let ((address (bbdb-dwim-net-address record email)))
    (unless string (setq string address))
    (set-text-properties 0 (length string) (list 'address address) string)
    string))

(defun ido-bbdb-alt-props (record email)
  "Produce annotated string for non-primary address."
  (ido-bbdb-address-props record (ido-bbdb-alt-address record email) email))

(defun ido-bbdb-aka-props (record aka)
  "Produce annotated string for AKA and primary address."
  (ido-bbdb-address-props record (ido-bbdb-alt-address record aka)))

(defun ido-bbdb-completions (records)
  "Return a list of all kinds of completions of RECORDS."
  (let (alias-map choices)
    ;; While iterating through records, collect choices and aliases.
    (dolist (record records)
      (when (bbdb-record-net record)
        ;; Primary email (1)
        (setq choices (cons (ido-bbdb-address-props record) choices))
        ;; Secondary emails (2)
        (dolist (email (cdr (bbdb-record-net record)))
          (setq choices (cons (ido-bbdb-alt-props record email) choices)))
        ;; AKAs (3)
        (dolist (aka (bbdb-record-aka record))
          (setq choices (cons (ido-bbdb-aka-props record aka) choices)))
        ;; Collect aliases
        (let ((aliases (cdr (assq 'mail-alias
                                  (bbdb-record-raw-notes record)))))
          (dolist (alias (and aliases (split-string aliases ", ")))
            (let ((binding (assoc alias alias-map)))
              (if binding
                  (setcdr binding (cons record (cdr binding)))
                (setq alias-map (cons (cons alias (list record)) alias-map))
                ))))))
    ;; Now process the aliases
    (dolist (binding alias-map)
      (let* ((n (length (cdr binding)))
             (s (if (= n 1)
                    (concat (car binding) " ("
                            (bbdb-record-name (cadr binding)) ")")
                  (format "%s (%d recipients)" (car binding) n)))
             (es (mapconcat 'bbdb-dwim-net-address (cdr binding) ", ")))
        (set-text-properties 0 (length s) (list 'address es) s)
        (setq choices (cons s choices))))
    ;; Return everything
    (nreverse choices)))

(defun ido-bbdb-complete (&optional start-pos)
  (interactive)
  (let* ((end (point))
         (beg (or start-pos
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
         (orig (buffer-substring beg end))
         (typed (downcase orig))
         (pattern (bbdb-string-trim typed))
         (s (ido-completing-read "Recipient: "
                                 (ido-bbdb-completions (bbdb-records))
                                 nil nil pattern)))
    (delete-region beg end)
    (insert (get-text-property 0 'address s))))

;; Override bbdb-complete-name
(defun ido-bbdb-insinuate ()
  (fset 'bbdb-complete-name 'ido-bbdb-complete))

(defun ido-bbdb-test-suite ()
  (interactive)
  (let* ((r1ea "bob@bob.bob")
         (r1eb "bob@bob.ca")
         (r1ec "robert.barker@bob.ca")
         (r1first "Robert")
         (r1last "Barker")
         (r1nick "Bob")
         (r1 (vector r1first r1last (list r1nick) nil nil nil
                     (list r1ea r1eb r1ec) '((mail-alias . "bo, grp")) [nil]))
         (r1full (concat r1first " " r1last))
         (r1fa (concat r1full " <" r1ea ">"))
         (r1fb (concat r1full " <" r1eb ">"))
         (r1fc (concat r1full " <" r1ec ">"))
         (r1ga (concat r1ea " (" r1full ")"))
         (r1gb (concat r1eb " (" r1full ")"))
         (r1gc (concat r1ec " (" r1full ")"))
         (r2 (vector "Alice" "Waters" (list "Ali" "AW") nil nil nil
                     (list "alice@alice.org" "aw@aw.aw")
                     '((mail-alias . "grp")) [nil])))

    (assert (equal (bbdb-record-name r1) r1full))

    (assert (equal (ido-bbdb-alt-address r1 r1ea) r1ga))
    (assert (equal (ido-bbdb-alt-address r1 r1eb) r1gb))
    (assert (equal (ido-bbdb-alt-address r1 r1ec) r1gc))

    (assert (equal (bbdb-dwim-net-address r1 r1ea) r1fa))
    (assert (equal (bbdb-dwim-net-address r1 r1eb) r1fb))
    (assert (equal (bbdb-dwim-net-address r1 r1ec) r1ec)) ;

    (let* ((s1a "Bloo")
           (s1b "Floo")
           (p1a (ido-bbdb-address-props r1))
           (p1b (ido-bbdb-address-props r1 s1a))
           (p1c (ido-bbdb-address-props r1 s1b r1eb))
           (p1d (ido-bbdb-alt-props r1 r1eb))
           (p1e (ido-bbdb-aka-props r1 "Bobo")))
      (assert (equal p1a r1fa))
      (assert (equal p1b s1a))
      (assert (equal p1c s1b))
      (assert (equal p1d r1gb))
      (assert (equal p1e (concat "Bobo (" r1full ")")))
      (flet ((propeq (p q)
                     (assert (equal (get-text-property 0 'address p) q))))
        (propeq p1a r1fa)
        (propeq p1b r1fa)
        (propeq p1c r1fb)
        (propeq p1d r1fb)))

    (let ((cs (ido-bbdb-completions (list r1 r2))))
      (assert (member "AW (Alice Waters)" cs))
      (assert (member "Ali (Alice Waters)" cs))
      (assert (member "aw@aw.aw (Alice Waters)" cs))
      (assert (member "Alice Waters <alice@alice.org>" cs))
      (assert (member "Bob (Robert Barker)" cs))
      (assert (member "robert.barker@bob.ca (Robert Barker)" cs))
      (assert (member "bob@bob.ca (Robert Barker)" cs))
      (assert (member "Robert Barker <bob@bob.bob>" cs))
      (assert (member "grp (2 recipients)" cs))
      (assert (member "bo (Robert Barker)" cs))
      cs
      )))

(provide 'ido-bbdb-complete)
