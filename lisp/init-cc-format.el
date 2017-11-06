(when (cu-program-exists-p "astyle")
  (defun astyle-this-buffer(pmin pmax)
    (interactive "r")
    (shell-command-on-region pmin pmax
                             "astyle --style=stroustrup"
                             (current-buffer)
                             t
                             (get-buffer-create "*Astyle Errors*")
                             t)))

(require 'clang-format)

(setq-default clang-format-style
      "{BasedOnStyle: Google, IndentWidth: 4}")

(provide 'init-cc-format)
