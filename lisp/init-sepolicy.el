;; Defines major mode for editing SELinux TE-RBAC ("sample")
;; policy definitions.  Mostly used to get nice color.

;; Author: Eamon Walsh <ewalsh@epoch.ncsc.mil>

;; Instructions for use:
;; place in your emacs library path (e.g. "site-lisp" directory,
;; look for .el and .elc files).  Then, add the following to your
;; ~/.emacs: (load-library "selinux-policy")

;; Use M-x sepolicy-mode in Emacs to enter the mode (editing
;; a .te file enters the mode automatically).

;; Note: make sure font-lock mode is enabled in Emacs or you won't
;; get the color highlighting.

(require 'compile)

(defvar sepolicy-mode-syntax-table nil
  "Syntax table for use in Sepolicy-mode buffers.")

(if sepolicy-mode-syntax-table
    ()
  (setq sepolicy-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?# "<"   sepolicy-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  sepolicy-mode-syntax-table)
  (modify-syntax-entry ?{ "(}"  sepolicy-mode-syntax-table)
  (modify-syntax-entry ?} "){"  sepolicy-mode-syntax-table)
  (modify-syntax-entry ?\( "()"  sepolicy-mode-syntax-table)
  (modify-syntax-entry ?\) ")("  sepolicy-mode-syntax-table)
  (modify-syntax-entry ?\; "."  sepolicy-mode-syntax-table)
  (modify-syntax-entry ?, "."   sepolicy-mode-syntax-table)
  (modify-syntax-entry ?= "."   sepolicy-mode-syntax-table)
  (modify-syntax-entry ?~ "."   sepolicy-mode-syntax-table)
  (modify-syntax-entry ?* "."   sepolicy-mode-syntax-table)
  (modify-syntax-entry ?_ "w"   sepolicy-mode-syntax-table)
  (modify-syntax-entry ?. "_"   sepolicy-mode-syntax-table)
  )

(define-derived-mode sepolicy-mode m4-mode "Sepolicy"
  "Major mode for editing SELinux TE-RBAC policies."
  (set-syntax-table sepolicy-mode-syntax-table)
  ;; Font-lock stuff
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults sepolicy-font-lock-defaults)
  )

(defvar sepolicy-font-lock-defaults
  `(sepolicy-font-lock-keywords
    nil		; do fontify strings and comments
    t		; case is INsignificant
    nil
    ;;    ,(mapcar (function (lambda (c) (cons c "w")))
    ;;	     "[]$_.:#") ; make these word syntax for font-lock
    nil
    ))

(defconst sepolicy-font-lock-keywords
  '("\\<type\\>"
    "\\<allow\\>"
    "\\<neverallow\\>"
    "\\<types\\>"
    "\\<self\\>"
    "\\<type_transition\\>"
    "\\<role_transition\\>"
    "\\<type_change\\>"
    "\\<alias\\>"
    "\\<role\\>"
    "\\<roles\\>"
    "\\<common\\>"
    "\\<inherits\\>"
    "\\<class\\>"
    "\\<user\\>"
    "\\<attribute\\>"
    "\\<auditallow\\>"
    "\\<dontaudit\\>"
    "\\<dominance\\>"
    "\\<constrain\\>"
    "\\<dom\\>"
    "\\<domby\\>"
    "\\<incomp\\>"
    "\\<not\\>"
    "\\<or\\>"
    "\\<and\\>"
    "\\<u1\\>"
    "\\<u2\\>"
    "\\<t1\\>"
    "\\<t2\\>"
    "\\<r1\\>"
    "\\<r2\\>"
    "\\<sid\\>"
    "\\<fs_use_xattr\\>"
    "\\<fs_use_psid\\>"
    "\\<fs_use_task\\>"
    "\\<fs_use_trans\\>"
    "\\<genfscon\\>"
    "\\<portcon\\>"
    "\\<netifcon\\>"
    "\\<nodecon\\>"
    ("\\<\\w+?_t\\>" 0 font-lock-type-face keep t)
    ("\\<\\w+?_u\\>" 0 font-lock-constant-face keep t)
    ("\\<\\w+?_r\\>" 0 font-lock-builtin-face keep t)
    ("\\<\\(\\w+?\\)\\>\\s-*(" 1 font-lock-warning-face keep t)
    ("\\s." 0 font-lock-string-face keep t))
  "Fontification for SELinux TE-RBAC policy code.")

(add-to-list 'auto-mode-alist '("\\.te$" . sepolicy-mode))
(provide 'init-sepolicy)

