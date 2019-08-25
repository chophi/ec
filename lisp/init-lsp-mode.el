(require 'cc-mode)

(defvar possible-java-lsp-server (cu-join-path (getenv "HOME") ".jdt-server/"))

(setq-default lsp-java-server-install-dir possible-java-lsp-server
              lsp-java-boot-enabled nil
              lsp-java-workspace-dir (cu-join-path (getenv "HOME") ".jdt-workspace"))

;;; prefer lsp-ui
(setq-default lsp-prefer-flymake nil)

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
(use-package pfuture :ensure t)
(use-package treemacs :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui :ensure t
  :custom
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil))

(use-package lsp-java :ensure t :after lsp)

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))
(use-package lsp-java-treemacs :after (treemacs))

;; Replace completing-read with ido-completing-read
(defadvice lsp
    (around use-ido-when-possible activate)
  (cl-letf (((symbol-function 'completing-read) 'ido-completing-read))
    ad-do-it))

;; (when-let* ((dir (format "%s/tools/lsp-intellij" (getenv "REPO_PUBLIC_ROOT")))
;;           (file (cu-join-path dir "lsp-intellij.el"))
;;           (exists-p (file-exists-p file)))
;;   (add-to-list 'load-path dir)
;;   (require 'lsp-intellij))

(require 'lsp-java-boot)

(defun my-check-enable-lsp-for-java-hook ()
  (interactive)
  (when (and (featurep 'projectile) (projectile-project-p)
             (file-exists-p (cu-join-path (projectile-project-root ".classpath"))))
    (lsp)
    (lsp-java-boot-lens-mode)))

(defun my-make-jdt-project-configuration ()
  (interactive)
  (let ((script-path "~/repo/fba/NinjaUtilsClojure/src/NinjaUtilsClojure/build/bin/generate-brazil-jdt-project"))
    (if (not (and (file-exists-p script-path)
                  (featurep 'projectile) (projectile-project-p)))
        (error "no script found"))
    (let ((default-directory (projectile-project-root)))
      (start-file-process "generate-brazil-jdt-project"
                          (get-buffer-create "*generate-brazil-jdt-project*")
                          script-path))))

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'my-check-enable-lsp-for-java-hook)

(provide 'init-lsp-mode)
