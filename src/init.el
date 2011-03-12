;; ;; ------------------------------------------------
;; (setq my-lisp-home "/home/sharif/.emacs-lisp")
;; (add-to-list 'load-path  my-lisp-home)


;; this is the only true way to use spaces instead of tabs, and indent
;; to 4 spaces
(defun coding-standards-enforcement ()
  (setq indent-tabs-mode nil)
  (setq c-indent-level 4))

(custom-set-variables
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t))

(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t ; rename after killing uniquified
 uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; ----------------------------------------------------
;; -- Always display line and column numbers in the mini-buffer window
;; ----------------------------------------------------
(column-number-mode t)
(line-number-mode t)

;; org-mode tweaks
(append auto-mode-alist '("\\.org\\" . org-mode))
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-directory "~/info/orgfiles")
(setq org-default-notes-file "~/info/orgfiles/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)
(setq org-log-done t)


;; ;; ------------------------------------------------
;; ;; -- Set backups to not hose sym/hard links
;; ;; ------------------------------------------------h
(setq backup-by-copying t)

;; ;; ----------------------------------------------------
;; ;; -- Make sure the backspace key works from dumb-terms
;; ;; -- I don't know about this one. It fixes the problem,
;; ;; -- but doesn't seem to be a clean solution.
;; ;; ----------------------------------------------------

;; (global-set-key "\C-h" 'delete-backward-char)
;; (global-set-key "\M-g" 'goto-line)

;; ------------------------------------------------
;; -- Use a backup dir instead of having ~ files
;; -- everywhere. Obtained from emacs wiki
;; ------------------------------------------------
(setq
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; use versioned backups


(cond
 (window-system
  (mwheel-install)
  (setq default-frame-alist
        '(
          ;;(font . "")
          ;;(font . "-dec-terminal-bold-r-normal--14-140-75-75-c-80-iso8859-1")
          ;;
          ;;(font . "-dec-terminal-medium-r-normal--14-140-75-75-c-80-iso8859-1")
          ;;(font . "-dec-terminal-medium-r-normal-*-*-140-*-*-c-*-iso8859-1")
          ;;(font . "-b&h-lucidatypewriter-medium-r-normal-sans-11-80-100-100-m-70-iso8859-1")
          ;;          (font . "-b&h-lucidatypewriter-medium-r-normal-sans-14-140-75-75-m-90-iso8859-1")
          ;;(font . "-b&h-lucidatypewriter-medium-r-normal-sans-18-180-75-75-m-110-iso8859-1")
          ;;(font . "-bitstream-bitstream vera sans mono-medium-r-normal--0-0-0-0-m-0-ascii-0")
          (font . "-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
          (mouse-color . "red")
          (cursor-color . "green")
          (foreground-color . "gray")
          (background-color . "black")))))

;; (set-face-foreground  'modeline  "navy")
;; (set-face-background  'modeline  "SteelBlue")
;; (set-face-foreground  'region    "White")
;; (set-face-background  'region    "SteelBlue")
;; (set-face-foreground  'default   "gray")
;; (set-face-background  'default   "black")
;; (set-face-background  'highlight "purple")


;; ----------------------------------------------------
;; -- Turn on line wrapping so text does not exceed screen width
;; -- (this doesn't seem to work in emacs 21.1...)
;; ----------------------------------------------------
(setq truncate-partial-width-windows t)

;; ;; ----------------------------------------------------
;; ;; -- Set up server for emacsclients
;; ;; ----------------------------------------------------
(server-start)
(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

;; ;; ----------------------------------------------------
;; ;; -- Who cares if there is a newline at the end of a file
;; ;; ----------------------------------------------------
;; (setq require-final-newline nil)

;; ;; ----------------------------------------------------
;; ;; -- Make the cursor automatically avoid the mouse cursor
;; ;; ----------------------------------------------------
;; (setq mouse-avoidance-mode 'animate)

;; ----------------------------------------------------
;; -- Make the column break be less stupid
;; ----------------------------------------------------
(setq fill-column 95)

;; ----------------------------------------------------
;; -- Make sure passwords can't be seen by observers in
;; -- shell-flavor modes
;; ----------------------------------------------------
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)


;; ----------------------------------------------------
;; -- Make emacs remember buffers and rings
;; ----------------------------------------------------
;;(add-to-list 'load-path (concat my-lisp-home "/session"))
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

;; ;; ----------------------------------------------------
;; ;; apache-mode
;; ;; ----------------------------------------------------
;; (autoload 'apache-mode "apache-mode" "autoloaded" t)
;; (add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
;; (add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
;; (add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
;; (add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))
;; (add-to-list 'auto-mode-alist '("apache[12]\?\\.conf$" . apache-mode))
;; (add-to-list 'auto-mode-alist '("commonapache[12]\?\\.conf$" . apache-mode))

;; ;; ---------------------------------------------------------------------------
;; ;; -- Perforce integration
;; ;; ---------------------------------------------------------------------------
;; ;(load-library "p4")

;; ;; ---------------------------------------------------------------------------
;; ;; -- HTML Tidy
;; ;; ---------------------------------------------------------------------------
;; (autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
;; (autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
;; (autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
;; (autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

;; (defun my-html-mode-hook () "Customize my html-mode."
;;   (tidy-build-menu html-mode-map)
;;   (local-set-key [(control c) (control c)] 'tidy-buffer)
;;   (setq sgml-validate-command "tidy"))

;; (add-hook 'html-mode-hook 'my-html-mode-hook)


;; ;; ----------------------------------------------------
;; ;; Directory colors for shell mode
;; ;; ----------------------------------------------------
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)



;; ;; ----------------------------------------------------
;; ;; CSS
;; ;; ----------------------------------------------------
;; (autoload 'css-mode "css-mode")
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
;; (setq cssm-indent-function #'cssm-c-style-indenter)
;; (setq cssm-indent-level '2)

;; ;; ----------------------------------------------------
;; ;; Javascript
;; ;; ----------------------------------------------------
;; (add-to-list 'auto-mode-alist '("\\.js$"   . java-mode))


;; ;; ----------------------------------------------------
;; ;; Template Toolkit mode
;; ;; ----------------------------------------------------
;; (autoload 'tt-mode "tt-mode")
;; (setq auto-mode-alist
;;  (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))

;; ;; ----------------------------------------------------
;; ;; PHP
;; ;; ----------------------------------------------------
;; (require 'php-mode)
;; (add-hook 'php-mode-user-hook 'turn-on-font-lock)
;; (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; ;; ---------------------------------------------------------------------------
;; ;; -- Auc Tex mode TODO: fix this so that it loads from my emacs dir, instead of global
;; ;; ---------------------------------------------------------------------------
;; ;;(require 'tex-site)


;; (setq font-lock-maximum-decoration t)

;; ;; Give me access to X clipboard.
;; ;;
;; ;;(setq x-select-enable-clipboard t)
;; ;; Below appears to be default:
;; ;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; (global-set-key [(shift delete)]   'clipboard-kill-region)
;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
;; (global-set-key [(shift insert)]   'clipboard-yank)

;; ;;;; Load an applicationtemplate in a new unattached buffer...
;; ;;(defun application-template-pm ()
;; ;;  "Inserts the standard Perl application template"  ; For help and info.
;; ;;  (interactive "*")                                 ; Make this user accessible.
;; ;;  (switch-to-buffer "application-template-pm")
;; ;;  (insert-file "~/.code_templates/perl_application.pl"))
;; ;;;; Set to a specific key combination...
;; ;;(global-set-key "\C-ca" 'application-template-pm)
;; ;;
;; ;;;; Load a module template in a new unattached buffer...
;; ;;(defun module-template-pm ()
;; ;;  "Inserts the standard Perl module template"       ; For help and info.
;; ;;  (interactive "*")                                 ; Make this user accessible.
;; ;;  (switch-to-buffer "module-template-pm")
;; ;;  (insert-file "~/.code_templates/perl_module.pl"))
;; ;;;; Set to a specific key combination...
;; ;;(global-set-key "\C-cm" 'module-template-pm)

;; ;;;; Expand the following abbreviations while typing in text files...
;; ;;(abbrev-mode 1)
;; ;;
;; ;;(define-abbrev-table 'global-abbrev-table '(
;; ;;    ("pdbg"   "use Data::Dumper qw( Dumper );\nwarn Dumper[];"   nil 1)
;; ;;    ("phbp"   "#! /usr/bin/perl"    r                             nil 1)
;; ;;    ("pbmk"   "use Benchmark qw( cmpthese );\ncmpthese -10, {};" nil 1)
;; ;;    ("pusc"   "use Smart::Comments;\n\n### "                     nil 1)
;; ;;    ("putm"   "use Test::More 'no_plan';"                        nil 1)
;; ;;    ))
;; ;;
;; ;;(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))



;; ;;; multi-term

;; (autoload 'multi-term "multi-term" nil t)
;; (autoload 'multi-term-next "multi-term" nil t)
;; ;; only needed if you use autopair
;; (add-hook 'term-mode-hook
;;   #'(lambda () (setq autopair-dont-activate t)))


;; (global-set-key (kbd "C-c t") 'multi-term-next)
;; (global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; (add-to-list 'load-path (concat my-lisp-home "/zenicb"))
;; (add-to-list 'load-path (concat my-lisp-home "/apache-mode"))
;; (add-to-list 'load-path (concat my-lisp-home "/cperl-mode"))
;; (add-to-list 'load-path (concat my-lisp-home "/php-mode"))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
