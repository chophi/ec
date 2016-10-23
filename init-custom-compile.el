;; Sample compile.el
;; ((unit "compile"
;;        (compile "ndk-build" "jni"))
;;  (unit "run"
;;        (run "adb root")
;;        (run "add remount")
;;        (run "adb push libs/armeabi/test-libstl /data/")
;;        (run "adb shell /data/test-libstl")))


(require 'init-handy)

(defun* _find-custom-compile-file-recursively (path)
  (when (equal path "/") (return-from _find-custom-compile-file-recursively nil))
  (let ((possible-name (concat path "compile.el")))
    (message "possible-name: %s " possible-name)
    (if (file-exists-p possible-name)
        (return-from _find-custom-compile-file-recursively possible-name)
      (_find-custom-compile-file-recursively (file-name-directory (directory-file-name path))))))

(defun find-custom-compile-file (path)
  (interactive "P")
  (when (not path) (setq path default-directory))
  (when (and (existed-directory? path) (not (equal (substring path (1- (length path))) "/")))
    (setq path (concat path "/")))
  (setq path (file-name-directory (expand-file-name path)))
  (_find-custom-compile-file-recursively path))

(defun* cp-custom-compile (path)
  (interactive "P")
  (when (not path) (setq path default-directory))
  (let ((compile-config (find-custom-compile-file path))
        project-root
        compile-expr
        choice
        chosed-unit
        (choice-list '())
        (compile-list '())
        (global-env ""))
    (when (not compile-config)
      (message "No custom compile file found, exit")
      (return-from cp-custom-compile nil))
    (setq compile-expr (eval-file-as-lisp-expression compile-config)
          project-root (file-name-directory compile-config))
    (dolist (ele compile-expr)
      (if (eq (car ele) 'global-env)
          (setq global-env (concat global-env " " (cadr ele)))
        (if (eq (car ele) 'unit)
            (progn (add-to-list 'compile-list (cdr ele))
                   (add-to-list 'choice-list (cadr ele)))
          (error "parse compile-expr err, should be either global-env or unit"))))
    (setq choice (ido-completing-read "Choose a unit: " choice-list)
          chosed-unit (cdr (assoc choice compile-list)))
    (dolist (command chosed-unit)
      (let ((command-type (car command))
            (command-string (cadr command))
            (relative-path (caddr command)))

        (with-current-buffer (get-buffer-create "*custom-compile-log*")
          (end-of-buffer)
          (insert (format "Global ENV: %s\n" (if (or  (equal global-env "")
                                                          (equal global-env " "))
                                                     "NONE"
                                                   global-env)))
          (insert (format "Command Type: %s\n"
                          (case command-type
                            ('compile "Compile")
                            ('run "Run")
                            (t "None"))))
          (insert (format "Command: %s\n" command-string))
          (insert (format "Relative Path: %s\n\n" relative-path)))

        (when (and global-env (not (equal global-env "")))
          (setq command-string (concat global-env " " command-string)))
        
        (when (and relative-path (not (equal relative-path "")))
          (setq command-string (concat "cd " relative-path " && " command-string)))

        (cd project-root)

        (case command-type
          ('compile (compile command-string))
          ('run (shell-command command-string))
          ('t (message "Unknown command type, exiting")
              (return-from cp-custom-compile nil)))))))

(global-set-key "\C-c\C-c" 'cp-custom-compile)
(provide 'init-custom-compile)
