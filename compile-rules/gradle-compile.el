(progn
  (defun _check-dir (dir)
    (let ((compile-exprs nil)
          (possible-gradle (cu-find-nearest-ancestor-match dir (regexp-opt '("build.gradle" "build.gradle.kts")) t)))
      (when possible-gradle
        (message "found a gradle project: %s\n" possible-gradle)
        (let ((project-root (file-name-directory possible-gradle)))
          (unless (equal project-root (concat (getenv "HOME") "/"))
            (add-to-list 'compile-exprs `(,project-root
                                          ((unit "gradle-build"
                                                 (elisp (call-interactively (gradle--make-build-lambda))))))))))
      compile-exprs))
  (_check-dir default-directory))

