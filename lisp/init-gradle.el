(require-package 'gradle-mode)
(require-package 'flycheck-gradle)
;;; For highlighting build.gradle
(require-package 'groovy-mode)

(define-derived-mode gradle-build-mode groovy-mode "Gradle-Build")
(add-to-list 'auto-mode-alist '("build.gradle" . gradle-build-mode))

(defun gradle-q-run ()
  (interactive)
  (gradle-run "-q run"))

(defun _gradle--make-build-map ()
  (_make-commands-map-with-help-msg
   '((?b . gradle-build)
     (?r . gradle-q-run)
     (?t . gradle-test)
     (?s . gradle-single-test)
     (?B . gradle-build--daemon)
     (?T . gradle-test--daemon)
     (?S . gradle-single-test--daemon)
     (?E . gradle-execute--daemon)
     (?e . gradle-execute))))

(defun gradle--make-build-lambda ()
  (interactive)
  (if (file-executable-p "gradlew")
    (let ((gradle-use-gradlew t)
          (gradle-gradlew-executable "./gradlew"))
      (_gradle--make-build-map))
    (_gradle--make-build-map)))

(add-hook 'gradle-build-mode
          (lambda ()
            (gradle-mode 1)) t)

(require 'gradle-mode)
(setq gradle-mode-map-old gradle-mode-map
      gradle-mode-map nil)
(define-key gradle-build-mode-map "\C-c\C-g"
  (lambda () (interactive) (call-interactively (gradle--make-build-lambda))))

(with-eval-after-load 'flycheck
  (flycheck-gradle-setup))

(provide 'init-gradle)
