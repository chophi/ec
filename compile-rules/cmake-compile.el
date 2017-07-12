;; suez make file
(progn
  (defun cmake--precondition-check (project-root mode)
    (when (not (file-exists-p (concat project-root "/" mode)))
      (make-directory (concat project-root "/" mode)))
    (when (not (file-exists-p (concat project-root "/" mode "/Makefile")))
      (compile (format "cd %s && cmake %s .."
                       (concat project-root "/" mode)
                       (cond ((equal "release" mode)
                              "-DCMAKE_BUILD_TYPE=Release")
                             (t
                              "-DCMAKE_BUILD_TYPE=Debug"))))))

  (defun _make-cmake-compile-exprs (project-root)
    (let* ((cmake-file (concat project-root "/" "CMakeLists.txt"))
           (mode (with-current-buffer (find-file-noselect cmake-file)
                   (if (boundp 'cmake--compile-mode)
                       cmake--compile-mode
                     (setq-local cmake--compile-mode 'debug))))
           (target-list (cu-extract-list
                         (find-file-noselect cmake-file)
                         "#"
                         "add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_-]*\\\)" 1)))
      `((unit "generate"
              (nop (cmake--precondition-check ,project-root ,(symbol-name mode))))
        (unit "all"
              (compile (cmake--compile-string "make all")))
        (unit "clean"
              (compile (cmake--compile-string "make clean")))
        (unit "dist-clean"
              (nop
               (let* ((mode-name ,(symbol-name mode))
                      (make-dir (format "%s%s" ,project-root mode-name))
                      (command (format "rm -rf %s" make-dir)))
                 (message "mode-name is %s" mode-name)
                 (message "make-dir is %s" make-dir)
                 (when (and
                        (or (equal mode-name "debug")
                            (equal mode-name "release"))
                        (cu-is-dir-or-dirlink? make-dir)
                        (y-or-n-p (format "Run: %s" command))
                        )
                   (shell-command command)
                   )
                 )))
        ,@(let ((target-compile-list '()))
           (dolist (target target-list target-compile-list)
             (add-to-list 'target-compile-list
                          `(unit ,target (compile
                                          (progn (cmake--precondition-check ,project-root ,(symbol-name mode))
                                                 (format "cd %s && make %s"
                                                         ,(symbol-name mode)
                                                         ,target))))))))
        ))

  (let (compile-exprs)
    (setq compile-exprs '())
    (let ((possible-cmake-file (_find-file-or-dir-recursively default-directory "CMakeLists.txt")))
      (when possible-cmake-file
        (message "found a CMakeLists.txt: %s\n" possible-cmake-file)
        (let ((project-root (file-name-directory possible-cmake-file)))
          (add-to-list 'compile-exprs `(,project-root  ,(_make-cmake-compile-exprs project-root)))))
      )
    compile-exprs))
