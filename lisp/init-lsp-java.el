(require 'cc-mode)

(defvar possible-java-lsp-server (cu-join-path (getenv "HOME") ".jdt-server/"))

(setq-default lsp-java-server-install-dir possible-java-lsp-server
      lsp-java-boot-enabled nil
      lsp-java-workspace-dir (cu-join-path (getenv "HOME") ".jdt-workspace"))

(when (file-exists-p possible-java-lsp-server)
  (condition-case nil
      (require 'use-package)
    (file-error
     (require 'package)
     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
     (package-initialize)
     (package-refresh-contents)
     (package-install 'use-package)
     (require 'use-package)))

  (use-package projectile :ensure t)
  (use-package treemacs :ensure t)
  (use-package yasnippet :ensure t)
  (use-package lsp-mode :ensure t)
  (use-package hydra :ensure t)
  (use-package company-lsp :ensure t)
  (use-package lsp-ui :ensure t)
  (use-package lsp-java :ensure t :after lsp
    ;;:config (add-hook 'java-mode-hook 'lsp)
    )

  (use-package dap-mode
    :ensure t :after lsp-mode
    :config
    (dap-mode t)
    (dap-ui-mode t))

  (use-package dap-java :after (lsp-java))
  (use-package lsp-java-treemacs :after (treemacs)))

(provide 'init-lsp-java)
