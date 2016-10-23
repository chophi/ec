(defun cp-under-dir-has-sub (regex &optional path)
  (when (not path) (setq path default-directory))
  (let ((parts (split-string path "/"))
        (ret nil))
    (dolist (part parts ret)
      (when (string-match regex part)
        (message "found matching %s" part)
        (setq ret t)))))


(defun cp-detect-context (&optional path)
  (when (and (cp-under-dir-has-sub "Chapter_[0-9]+$")) 'cp-opengles3-book))


(provide 'init-custom-compile)

