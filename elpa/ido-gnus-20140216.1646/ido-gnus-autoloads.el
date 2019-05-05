;;; ido-gnus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-gnus" "ido-gnus.el" (0 0 0 0))
;;; Generated autoloads from ido-gnus.el

(autoload 'ido-gnus-select-group "ido-gnus" "\
Select a gnus group to visit using ido.
If a prefix arg is used then the sense of `ido-gnus-num-articles' will be reversed:
  if it is a number then the number of articles to display will be prompted for,
otherwise `gnus-large-newsgroup' articles will be displayed.

gnus will be started if it is not already running.

\(fn PREFIX)" t nil)

(autoload 'ido-gnus-select-server "ido-gnus" "\
Select a gnus server to visit using ido.

gnus will be started if it is not already running.

\(fn)" t nil)

(autoload 'ido-gnus-select "ido-gnus" "\
Select a gnus group/server or existing gnus buffer using ido.

\(fn PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-gnus" '("ido-gnus-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-gnus-autoloads.el ends here
