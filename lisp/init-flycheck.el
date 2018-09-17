(require-package 'flycheck)
(setq flycheck-global-modes '(python-mode c-mode c++-mode java-mode))
(global-flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++1z")))
(provide 'init-flycheck)
