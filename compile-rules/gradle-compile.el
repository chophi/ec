(progn
  (defun _generate-gradle-mode-map-commands ()
    (let ((commands (mapcar 'cdr (seq-filter 'listp  (-flatten gradle-mode-map)))))
      (mapcar (lambda (c)
                `(unit ,(symbol-name c) (elisp (call-interactively (quote ,c)))))
              commands)))
  (defun _check-dir (dir)
    (let ((compile-exprs nil)
          (possible-gradle (cu-find-nearest-ancestor-match dir ".gradle")))
      (when possible-gradle
        (message "found a gradle project: %s\n" possible-gradle)
        (let ((project-root (file-name-directory possible-gradle)))
          (add-to-list 'compile-exprs `(,project-root ,(_generate-gradle-mode-map-commands)))))
      compile-exprs))
  (_check-dir default-directory))

