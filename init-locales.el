(when *is-windows-system-p*
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  (set-terminal-coding-system 'utf-8)
  (setq file-name-coding-system 'gbk)

  (set-selection-coding-system 'gbk)
  (set-clipboard-coding-system 'gbk)

  (prefer-coding-system 'utf-8)

  ;; (set-coding-system-priority 'utf-8 'utf-8-dos)

  ;; ;; week day 乱码
  (fset 'old-format-time-string (symbol-function 'format-time-string))
  ;; (format-time-string "%a")
  (defun format-time-string-fix-local-week-day(format-string &optional time universal)
    (let ((time-string (old-format-time-string format-string time universal))
	  (friday '(21485 28969 156000 0))
	  (origin-day-string nil)
	  (day-string '("THU" "WED" "TUE" "MON" "SUN" "SAT" "FRI")))
      (dotimes (count 7)
	(setq origin-day-string (cons (old-format-time-string "%a"
							      (time-add friday (days-to-time count)))
				      origin-day-string)))
      (dotimes (count 7)
	(let ((orig-str (elt origin-day-string count))
	      (replace-to-str (elt day-string count)))
	  ;; (message "%s->%s" orig-str replace-to-str)
	  (setq time-string
		(replace-regexp-in-string orig-str
					  replace-to-str
					  time-string))))
      time-string))

  (fset 'format-time-string (symbol-function 'format-time-string-fix-local-week-day))
  (fmakunbound 'format-time-string-fix-local-week-day))

(provide 'init-locales)
