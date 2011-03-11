(defun iwb ()
  "Indent whole buffer, delete trailing whitespace, and untabify."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun perltidy-region () "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)
    ))

(defun perltidy-defun () "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
               (perltidy-region)))

(defun perltidy-buffer () "Run perltidy on current buffer."
  (interactive)
  (let ((where_i_was (point)))
    (save-excursion (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t))
    (goto-char where_i_was)
    )
  )

(defun my-rpm-changelog-increment-version ()
  (interactive)
  (goto-char (point-min))
  (let* ((max (search-forward-regexp rpm-section-regexp))
       (version (rpm-spec-field-value "Version" max)))
   (rpm-add-change-log-entry (concat "Update version to " version))
   )
  )

;; Use % to match various kinds of brackets...
;; See: http://www.lifl.fr/~hodique/uploads/Perso/patches.el
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

