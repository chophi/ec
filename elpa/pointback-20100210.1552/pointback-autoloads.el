;;; pointback-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pointback" "../../../../.emacs.d/elpa/pointback-20100210.1552/pointback.el"
;;;;;;  "de51bb819693ca65bc51e31b0facf01a")
;;; Generated autoloads from ../../../../.emacs.d/elpa/pointback-20100210.1552/pointback.el

(autoload 'pointback-mode "pointback" "\
Restore previous window point when switching back to a buffer.

\(fn &optional ARG)" t nil)

(defvar global-pointback-mode nil "\
Non-nil if Global Pointback mode is enabled.
See the `global-pointback-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pointback-mode'.")

(custom-autoload 'global-pointback-mode "pointback" nil)

(autoload 'global-pointback-mode "pointback" "\
Toggle Pointback mode in all buffers.
With prefix ARG, enable Global Pointback mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pointback mode is enabled in all buffers where
`pointback-on' would do it.
See `pointback-mode' for more information on Pointback mode.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "pointback" "../../../../.emacs.d/elpa/pointback-20100210.1552/pointback.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/pointback-20100210.1552/pointback.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pointback" '("pointback-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/pointback-20100210.1552/pointback-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/pointback-20100210.1552/pointback.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pointback-autoloads.el ends here
