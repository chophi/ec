(defun turn-on-rainbow-delimiters-mode ()
  (rainbow-delimiters-mode 1))

(dolist (hook
         '(emacs-lisp-mode
           racket-mode
           c-mode-common-hook))
  (add-hook hook 'turn-on-rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
