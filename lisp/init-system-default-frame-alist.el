(setq frame-default-name "Emacs::Console")
(defun my-set-frame-name ()
  (interactive)
  (let ((name (read-string (concat "Frame name[" frame-default-name "]: "))))
    (when (or (not name) (equal name ""))
      (setq name frame-default-name))
    (set-frame-parameter (selected-frame) 'title name)))

(provide 'init-system-default-frame-alist)

