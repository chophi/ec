(require-package 'gradle-mode)
(require-package 'flycheck-gradle)
;;; For highlighting build.gradle
(require-package 'groovy-mode)
(with-eval-after-load 'flycheck
  (flycheck-gradle-setup))

(setq gradle-use-gradlew t
      gradle-gradlew-executable "./gradlew")

(provide 'init-gradle)
