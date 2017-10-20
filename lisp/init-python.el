(setq-default python-indent-offset 4)

(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)

(require-package 'helm-pydoc)
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))

(require-package 'yapfify)
;;; Don't enable it by default, or everytime you save the python source code,
;;; it will re-format the file.
;;; (add-hook 'python-mode-hook 'yapf-mode)

(provide 'init-python)
