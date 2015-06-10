;;(require-package 'cmake-project)
;;(require-package 'company-cmake)
(require-package 'cpputils-cmake)
(require-package 'cmake-mode)
(require 'cmake-mode)
(require 'ido)

(defun my-cmake-compile (arg)
  (interactive "P")
  (let (choose-list choice (dir (file-name-directory (buffer-file-name))) mode)
    (if arg (setq mode "release") (setq mode "debug"))
    (when (not (file-exists-p (concat dir mode)))
      (make-directory (concat dir mode)))
    (when (not (file-exists-p (concat dir mode "/Makefile")))
      (compile (format "cd %s && cmake .. -G \"MSYS Makefiles\"" (concat dir mode))))
    (setq choose-list
          (let ((pos 1) (result '()) (str (buffer-substring-no-properties (point-min) (point-max))))
            (while (and (< pos (point-max)) (string-match "add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_-]*\\\)" str pos))
              (add-to-list 'result (match-string 1 str))
              (setq pos (match-end 1)))
            result))
    (setq choose-list (append choose-list '("all" "clean" "generate")))
    (setq choice (ido-completing-read (format "COMPILE{%s}: " mode)  choose-list ))
    (cond ((equal choice "generate")
           (compile (format "cd %s && cmake %s .. -G \"MSYS Makefiles\""
                    (concat dir mode)
                    (if (equal mode "debug")
                        "-DCMAKE_BUILD_TYPE=Debug"
                      "-DCMAKE_BUILD_TYPE=Release"))))
          ((or (equal choice "all")
               (equal choice "clean"))
           (compile (format "cd %s && make %s"
                            (concat dir mode)
                            choice)))
          (t
           (compile (format "cd %s && make %s"
                            (concat dir mode)
                            choice))))))


(defun my-cmake-run (arg)
  (interactive "P")
  (let (choose-list exe-list run-command-list choice (dir (file-name-directory (buffer-file-name))) mode)
    (if arg (setq mode "release") (setq mode "debug"))
    (when (not (file-exists-p (concat dir mode)))
      (make-directory (concat dir mode)))
    (when (not (file-exists-p (concat dir mode "/Makefile")))
      (compile (format "cd %s && cmake .. -G \"MSYS Makefiles\"" (concat dir mode))))
    (setq exe-list
          (let ((pos 1) (result '()) (str (buffer-substring-no-properties (point-min) (point-max))))
            (while (and (< pos (point-max)) (string-match "add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_]*\\\)" str pos))
              (add-to-list 'result (match-string 1 str))
              (setq pos (match-end 1)))
            result)
          choose-list exe-list)
    (setq run-command-list
            (let ((pos 1) (result '()) (str (buffer-substring-no-properties (point-min) (point-max))))
              (while (and (< pos (point-max)) (string-match "add_custom_target\\\s*(\\\s*\\\([0-9a-zA-Z_]*\\\)" str pos))
                (add-to-list 'result (match-string 1 str))
                (setq pos (match-end 1)))
              result)
          choose-list (append choose-list run-command-list))
    (let ((choose-list-clean choose-list))
      (dolist (choice choose-list)
        (dolist (choice2 choose-list)
          (when (equal (concat "run_" choice) choice2)
            (setq choose-list-clean (delete choice choose-list-clean)))))
      (setq choose-list choose-list-clean))
    (setq choice (ido-completing-read (format "RUN{%s}: " mode)  choose-list ))
    (cond ((member choice exe-list)
           (compile (format "cd %s && make %s && ../bin/%s"
                                  (concat dir mode)
                                  choice
                                  choice)))
          ((member choice run-command-list)
           (compile (format "cd %s && make %s"
                                  (concat dir mode)
                                  choice))))))

(defun my-cmake-run2()
  (interactive)
  (let ((choice))
    (setq choice
          (ido-completing-read "RUN EXE: "
                               (cddr (directory-files "bin"))))
    (compile (concat "cd bin && " choice))
    ))
(defun my-cmake-help ()
  (interactive)
  (cmake-help-command)
)

(defun my-cmake-test (arg)
  (interactive "P")
  (let (mode)
    (if arg (setq mode "release") (setq mode "debug"))
    ;; ctest -V 将gtest的信息也打印出来
    (compile (format "cd %s && ctest -V" mode))))

(add-hook 'cmake-mode-hook
          (lambda () (interactive)
            (local-set-key "\C-c\C-c" 'my-cmake-compile)
            (local-set-key "\C-c\C-e" 'my-cmake-run2)
            (local-set-key "\C-c\C-t" 'my-cmake-test)
            (local-set-key "\C-ch" 'my-cmake-help)))

;; 添加cmake gtest错误信息
(require 'compile)
(add-to-list 'compilation-error-regexp-alist '("^[0-9]+: \\(.*?\\):\\([0-9]+\\): Failure$" 1 2))
(provide 'init-cmake)


