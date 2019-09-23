(use-package projectile :ensure t)
(use-package pfuture :ensure t)
(use-package treemacs :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)

;; prefer lsp-ui
(setq-default lsp-prefer-flymake nil)
(use-package lsp-ui :ensure t
  :custom
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-enable nil))

;;; Replace completing-read with ido-completing-read
(defadvice lsp
    (around use-ido-when-possible activate)
  (cl-letf (((symbol-function 'completing-read) 'ido-completing-read))
    ad-do-it))

;;; Debug adapter protocol integration
(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'init-lsp)
