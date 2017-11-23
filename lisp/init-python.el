(setq-default python-indent-offset 4)

(require-package 'jedi)
(setq jedi:complete-on-dot t)

(require-package 'helm-pydoc)
(require 'smart-shift)
(with-eval-after-load "python"
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (add-hook 'python-mode-hook 'smart-shift-mode-on)
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))

(require-package 'yapfify)
;;; Don't enable it by default, or everytime you save the python source code,
;;; it will re-format the file.
;;; (add-hook 'python-mode-hook 'yapf-mode)

(provide 'init-python)
