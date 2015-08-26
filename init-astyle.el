(defun program-exist(program)
  (not (equal (shell-command-to-string (concat "which "  program)) "")))

(when (program-exist "astyle")
  (defun astyle-this-buffer(pmin pmax)
    (interactive "r")
    (shell-command-on-region pmin pmax
                             "astyle --style=stroustrup"
                             (current-buffer)
                             t
                             (get-buffer-create "*Astyle Errors*")
                             t)))

(require 'clang-format)
(provide 'init-astyle)
