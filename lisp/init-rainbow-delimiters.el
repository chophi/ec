(require-package 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode)))
(add-hook 'c-mode-common-hook (lambda () (rainbow-delimiters-mode)))
(provide 'init-rainbow-delimiters)
