(require-package 'gradle-mode)
(require-package 'flycheck-gradle)
;;; For highlighting build.gradle
(require-package 'groovy-mode)

(define-derived-mode gradle-build-mode groovy-mode "Gradle-Build")
(add-to-list 'auto-mode-alist '("build.gradle" . gradle-build-mode))

(defun _gradle--make-build-map ()
  (_make-commands-map-with-help-msg
   '((?b . gradle-build)
     (?t . gradle-test)
     (?s . gradle-single-test)
     (?B . gradle-build--daemon)
     (?T . gradle-test--daemon)
     (?S . gradle-single-test--daemon)
     (?E . gradle-execute--daemon)
     (?e . gradle-execute))))
(defun gradle--make-build-map ()
  (interactive)
  (when (file-exists-p "gradlew")
    (setq-local gradle-use-gradlew t)
    (setq-local gradle-gradlew-executable "./gradlew"))
  (call-interactively (_gradle--make-build-map)))

(add-hook 'gradle-build-mode
          (lambda ()
            (gradle-mode 1)) t)

(require 'gradle-mode)
(setq gradle-mode-map-old gradle-mode-map
      gradle-mode-map nil)
(define-key gradle-build-mode-map "\C-c\C-g" #'gradle--make-build-map)

(with-eval-after-load 'flycheck
  (flycheck-gradle-setup))

(provide 'init-gradle)
