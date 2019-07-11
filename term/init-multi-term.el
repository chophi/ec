(require-package 'multi-term)

(cond ((and shell-file-name (file-exists-p shell-file-name))
       (setq multi-term-program shell-file-name)))

(provide 'init-multi-term)
