(require-package 'apples-mode)
(require 'apples-mode)
(add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))

(defun my-run (params)
  (interactive "P")
  (if params
      (setq params (read-string (format "osascript %s \\\n" (buffer-file-name))))
    (setq params " "))
  (shell-command (format "osascript %s %s" (buffer-file-name) params)))

(define-key apples-mode-map "\C-c\C-e" 'my-run)
(provide 'init-applescript-mode)
