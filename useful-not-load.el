;;; modify encoding of all normal files under .emacs.d to utf-8-unix
(dolist (file  (gnus-recursive-directory-files "~/.emacs.d"))
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (save-buffer)
      (kill-buffer buffer))))


