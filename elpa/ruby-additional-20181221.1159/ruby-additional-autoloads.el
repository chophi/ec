;;; ruby-additional-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rdoc-mode" "rdoc-mode.el" (0 0 0 0))
;;; Generated autoloads from rdoc-mode.el

(autoload 'rdoc-mode "rdoc-mode" "\
Major mode for RD editing.
\\{rdoc-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rdoc-mode" '("rdoc-")))

;;;***

;;;### (autoloads nil "rubydb2x" "rubydb2x.el" (0 0 0 0))
;;; Generated autoloads from rubydb2x.el

(autoload 'rubydb "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rubydb2x" '("rubydb-command-name" "gud-rubydb-")))

;;;***

;;;### (autoloads nil "rubydb3x" "rubydb3x.el" (0 0 0 0))
;;; Generated autoloads from rubydb3x.el

(autoload 'rubydb "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rubydb3x" '("rubydb-command-name" "gud-rubydb-")))

;;;***

;;;### (autoloads nil nil ("ruby-additional-pkg.el" "ruby-additional.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ruby-additional-autoloads.el ends here
