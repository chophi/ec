(when (file-exists-p "~/.emacs.d/lisp-snippets/az-common-env.el")
  (load-file "~/.emacs.d/lisp-snippets/az-common-env.el")
  
  (dolist (buf (buffer-list))
    (let ((fname (buffer-file-name buf))
          newname)
      (when (and fname (string-match "mtk-protect" fname))
        (setq newname (replace-regexp-in-string "mtk-protect" "fmtk-500" fname))
        (when (file-exists-p newname)
          (message "from %s to %s" fname newname)
          (kill-buffer buf)
          (find-file newname)
          )))))




