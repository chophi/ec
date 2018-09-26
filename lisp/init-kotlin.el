(require-package 'kotlin-mode)

(require-package 'flycheck-kotlin)
(flycheck-kotlin-setup)

(require-package 'ob-kotlin)

(add-hook 'kotlin-mode-hook 'flycheck-mode)
(add-hook 'kotlin-mode-hook 'electric-pair-mode)

(add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode))

(add-to-path (format "%s/.sdkman/candidates/kotlin/current/bin" (getenv "HOME")))
(when (eq os 'linux)
  (add-to-path "/usr/lib/jvm/java-8-openjdk-amd64/bin" t)
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-8-openjdk-amd64"))

(provide 'init-kotlin)
