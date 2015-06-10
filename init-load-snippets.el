(defvar temp-snippets-dir "~/.emacs.d/lisp-snippets")

(dolist (filename (directory-files temp-snippets-dir))
  (if (equal "." (substring filename 0 1))
      nil
    (load-file (concat temp-snippets-dir "/" filename))))


(provide 'init-load-snippets)
