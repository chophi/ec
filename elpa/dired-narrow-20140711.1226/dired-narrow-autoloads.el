;;; dired-narrow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dired-narrow-fuzzy dired-narrow dired-narrow-regexp)
;;;;;;  "dired-narrow" "dired-narrow.el" (21895 6547 502357 282000))
;;; Generated autoloads from dired-narrow.el

(autoload 'dired-narrow-regexp "dired-narrow" "\
Narrow a dired buffer to the files matching a regular expression.

\(fn)" t nil)

(autoload 'dired-narrow "dired-narrow" "\
Narrow a dired buffer to the files matching a string.

If the string contains spaces, then each word is matched against
the file name separately.  To succeed, all of them have to match
but the order does not matter.

For example \"foo bar\" matches filename \"bar-and-foo.el\".

\(fn)" t nil)

(autoload 'dired-narrow-fuzzy "dired-narrow" "\
Narrow a dired buffer to the files matching a fuzzy string.

A fuzzy string is constructed from the filter string by inserting
\".*\" between each letter.  This is then matched as regular
expression against the file name.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("dired-narrow-pkg.el") (21895 6547 574749
;;;;;;  463000))

;;;***

(provide 'dired-narrow-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-narrow-autoloads.el ends here
