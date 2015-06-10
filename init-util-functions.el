(defun my-trash-files(file-list &optional delete-p)
  "if delete-p set, then delete files, or trash files"
  (dolist (file-name file-list)
    (if (file-exists-p file-name)
        (if delete-p
            (progn
              ;; (warn "delete file: %s" file-name)
              (shell-command (format "rm %s" file-name)))
          (progn
            ;; (warn "trash file: %s" file-name)
          (move-file-to-trash file-name)))
      (warn "file doesn't exist: %s" file-name))))

(defun my-trash-file(file &optional delete-p)
  "if delete-p set, then delete file, or trash file"
  (my-trash-files `(,file) delete-p))

(defun uf-clip-cur-path ()
  (interactive)
  (when (buffer-file-name)
    (kill-new (buffer-file-name))))

(defun my-shell-command-to-string (command)
  (let ((str (shell-command-to-string command)))
    (when (and (stringp str) (> (length str) 0))
      (substring str 0 (1- (length str)))
      )))

(defun uf-insert-date ()
  (interactive)
  (insert (my-shell-command-to-string "date \"+%Y.%m.%d-%H:%M:%S\"")))

(provide 'init-util-functions)
