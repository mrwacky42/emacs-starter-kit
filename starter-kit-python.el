;;; starter-kit-python.el --- Some helpful Python code
;;
;; Part of the Emacs Starter Kit

(add-hook 'python-mode-hook 'pretty-lambdas)

(provide 'starter-kit-python)

;(setq pycodechecker "/home/sharif/bin/pyscrub.py")
;(when (load "flymake" t)
;  (load-library "flymake-cursor")
;  (defun dss/flymake-pycodecheck-init ()
;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;           (local-file (file-relative-name
;                        temp-file
;                        (file-name-directory buffer-file-name))))
;      (list pycodechecker (list local-file))))
;  (add-to-list 'flymake-allowed-file-name-masks
;               '("\\.py\\'" dss/flymake-pycodecheck-init)))

;; starter-kit-python.el ends here
