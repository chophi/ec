(add-to-list 'load-path "~/.emacs.d/site-lisp/smali-mode")
(autoload 'smali-mode "smali-mode" "Major mode for editing and viewing smali issues" t)
(add-to-list 'auto-mode-alist '(".smali$" . smali-mode))

(provide 'init-smali)
