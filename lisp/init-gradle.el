(require-package 'gradle-mode)
(require-package 'flycheck-gradle)
;;; For highlighting build.gradle
(require-package 'groovy-mode)

(define-derived-mode gradle-build-mode groovy-mode "Gradle-Build")
(add-to-list 'auto-mode-alist '("build.gradle" . gradle-build-mode))

(defun gradle-q-run ()
  (interactive)
  (gradle-kill-compilation-buffer)
  (let ((choices (list (gradle-make-command "-q run"))))
    (let ((exe-dir "build/exe"))
      (when (file-exists-p exe-dir)
        (setq choices (append
                       (mapcar (lambda (file) (cu-join-path "build/exe" file))
                                 (cu-list-files-recursively-general
                                  exe-dir
                                  #'(lambda (fulpath)
                                      (and (file-regular-p fulpath)
                                           (file-executable-p fulpath)))
                                  3 t)) 
                              choices)))
      )
    (if (equal (length choices) 1)
        (compile (car choices))
      (compile (ido-completing-read "Choose a command: " choices)))))

(defun _get-gradle-target-list ()
  (interactive)
  (let* ((str (shell-command-to-string "gradle task --all"))
         (regex-str "\\([a-zA-Z1-9_]+\\) - \\(.+\\)")
         (task-lines (seq-filter (lambda (line)
                                   (string-match-p regex-str line))
                                 (split-string str "\n")))
         (task-list (mapcar (lambda (line)
                              (progn (string-match regex-str line)
                                     (cons (match-string 1 line) (match-string 2 line))))
                            task-lines)))
    task-list))

(defun _move-the-chosen-to-first ()
  (let ((the-chosen (assoc local-gradle-target local-gradle-target-list)))
    (when the-chosen
      (setq-local local-gradle-target-list
                  (remove the-chosen local-gradle-target-list))
      (add-to-list 'local-gradle-target-list the-chosen))))

(defun _gradle-get-targets (&optional re-new)
  (interactive "P")
  (with-current-buffer current-custom-compile-log-buffer
    (when (or re-new (not (boundp 'local-gradle-target-list))
              (not local-gradle-target-list))
      (setq-local local-gradle-target-list (_get-gradle-target-list)))
    (when (not (boundp 'local-gradle-target))
      (setq-local local-gradle-target nil))
    (_move-the-chosen-to-first)
    (setq-local local-gradle-target
                (ido-completing-read "Choose a target: "
                                     (mapcar 'car local-gradle-target-list)))
    (_move-the-chosen-to-first)))

(defun gradle-make-target (re-new)
  (interactive "P")
  (unless (boundp 'in-custom-compile-environment)
    (error "Only used in the custom compile environment"))
  (with-current-buffer current-custom-compile-log-buffer
    (_gradle-get-targets re-new)
    (gradle-run (format "-q %s" local-gradle-target))))

(defun gradle-show-current-target ()
  (interactive)
  (unless (boundp 'in-custom-compile-environment)
    (error "Only used in the custom compile environment"))
  (with-current-buffer current-custom-compile-log-buffer
    (if (and (boundp 'local-gradle-target-list) (boundp 'local-gradle-target))
        (message "Current Target is:\n[%s] - [%s]" local-gradle-target
                 (cdr (assoc local-gradle-target local-gradle-target-list)))
      (message "local-gradle-target-list or local-gradle-target not bound"))))

(defun* gradle-visit-build-file ()
  (interactive)
  (unless (boundp 'in-custom-compile-environment)
    (error "Only used in the custom compile environment"))
  (let ((cur-file custom-compile-run-on-file))
    (with-current-buffer current-custom-compile-log-buffer
      (let* ((possible-build-file
              '("build.gradle" "build.gradle.kts"))
             (build-file nil)
             (file nil))
        (dolist (f possible-build-file)
          (setq file (cu-join-path custom-compile-project-root f))
          (when (and (file-exists-p file))
            (if (equal file cur-file)
                (message "You're currently visiting the build file")
              (find-file-other-window file))
            (return-from gradle-visit-build-file t)))))))

(defun gradle-list-target ()
  (interactive)
  (gradle-run "-q task --all"))

(defun _gradle--make-build-map ()
  (cu-make-commands-map-with-help-msg
   '((?m . gradle-make-target)
     (?i . gradle-show-current-target)
     (?l . gradle-list-target)
     (?r . gradle-q-run)
     (?b . gradle-build)
     (?t . gradle-test)
     (?s . gradle-single-test)
     (?B . gradle-build--daemon)
     (?T . gradle-test--daemon)
     (?S . gradle-single-test--daemon)
     (?E . gradle-execute--daemon)
     (?e . gradle-execute)
     (?v . gradle-visit-build-file))))

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

(with-eval-after-load 'flycheck
  (flycheck-gradle-setup))

(provide 'init-gradle)
