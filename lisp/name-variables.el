;;; ts-name-variables.el -- get some name related variables
;; ts-name-variable-list
;; ts-get-name-variables
;; ts-set-name-variables
(defconst ts-name-variable-list 
  '(ts-file-name ts-file-name-ext ts-file-name-dir 
		 ts-file-name-ndir ts-file-basename
		 ts-file-name-no-ext) 
  "names of ts name variable")
;;; let them be local variables
(dolist (var ts-name-variable-list)
  (make-variable-buffer-local var)
  (set-default 'ts-file-name nil))
;;; get all variables in the format ((NAME  VALUE) ...)
(defun ts-get-name-variables()
  (let ((var-list nil))
    (dolist (var ts-name-variable-list)
      (setq var-list (append var-list (list (list var (symbol-value var))))))
    var-list)
  )
;; add-hook stuff
(defun ts-set-name-variables()
  (catch 'return
    (when (equal ts-file-name (buffer-file-name))
      (throw 'return nil))
    (setq ts-file-name (buffer-file-name)
	  ts-file-name-ext (file-name-extension ts-file-name)
	  ts-file-name-dir (file-name-directory ts-file-name)
	  ts-file-name-no-ext (substring ts-file-name 
					 0 (- (length ts-file-name) 
					      (+ (length ts-file-name-ext) 1)))
	  ts-file-name-ndir (substring ts-file-name
				       (length ts-file-name-dir)
				       (length ts-file-name))
	  ts-file-basename (substring ts-file-name
				      (length ts-file-name-dir) 
				      (length ts-file-name-no-ext)))))
(add-hook 'buffer-list-update-hook 'ts-set-name-variables)
(add-hook 'find-file-hook 'ts-set-name-variables)
(provide 'name-variables)
