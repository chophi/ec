;; suez make file
(progn
  (defun cmake--precondition-check (project-root mode)
    (let ((build-dir (concat project-root "/" mode)))
      (when (not (file-exists-p build-dir))
        (make-directory build-dir))
      (when (not (file-exists-p (concat build-dir "/Makefile")))
        (compile (format "cd %s && cmake %s .."
                         build-dir
                         (cond ((equal "release" mode)
                                "-DCMAKE_BUILD_TYPE=Release")
                               (t
                                "-DCMAKE_BUILD_TYPE=Debug")))))
      ))

  (defun _make-cmake-compile-exprs (project-root)
    (let* ((cmake-file (concat project-root "CMakeLists.txt"))
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
        (unit "set-mode"
              (nop (with-current-buffer (find-file-noselect ,cmake-file)
                     (when (not (boundp 'cmake--compile-mode))
                       (error "cmake--compile-mode was not bound for %s" ,cmake-file))
                     (if (equal
                            (ido-completing-read
                             (format "Choose compile mode for %s: " ,cmake-file)
                             '("debug" "release"))
                            "release")
                         (setq-local cmake--compile-mode 'release)
                       (setq-local cmake--compile-mode 'debug)
                       ))))
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
        (unit "run"
              (term (let ((executable
                           (ido-completing-read
                            "Executable: "
                            (split-string (shell-command-to-string
                                           (format
                                            (if *mac?*
                                                "cd %s && find ./bin -type f -perm +111"
                                              "cd %s && find ./bin -type f -executable")
                                                   ,project-root))))))
                      (format "./%s"
                              executable))))
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
