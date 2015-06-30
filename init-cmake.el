;;(require-package 'cmake-project)
;;(require-package 'company-cmake)
(require-package 'cpputils-cmake)
(require-package 'cmake-mode)
(require 'cmake-mode)
(require 'ido)


(defvar makefile-system-string
  (if (or (eq system-type 'windows-nt)
          (eq system-type 'ms-dos)
          (eq system-type 'cygwin))
      "-G \"MSYS Makefiles\""
    ""))


(defun no-comment-content (buf comment-prefix)
  (let (str-list
        (content ""))
    (when (not buf)
      (setq buf (current-buffer)))
    (setq str-list
          (with-current-buffer buf
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
    (dolist (str str-list content)
      (when (or (< (length str) (length comment-prefix))
                (not (equal comment-prefix
                            (substring str 0 (length comment-prefix))))) 
        (setq content (concat content "\n" str))))
    content))

(defun extract-list(buf comment-prefix regex part)
  (let ((pos 1)
        (result '())
        (str (no-comment-content buf comment-prefix)))
    (while (and (< pos (point-max))
                (string-match regex str pos))
      (add-to-list 'result (match-string part str))
      (setq pos (match-end part)))
    result))

(defun my-cmake-compile (arg)
  (interactive "P")

  (let ((dir (file-name-directory (buffer-file-name)))
        choose-list
        choice
        mode)

    (if arg (setq mode "release") (setq mode "debug"))

    ;; redefine the compile command to be executed in dir
    (defmacro my--compile (string &rest objects)
      `(compile (concat "cd " ,dir ,mode " && " (format ,string ,@objects))))

    (when (not (file-exists-p (concat dir mode))) (make-directory (concat dir mode)))
    (when (not (file-exists-p (concat dir mode "/Makefile")))
      (my--compile "cmake .. %s" makefile-system-string))

    (setq choose-list
          (extract-list nil "#" "add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_-]*\\\)" 1))
    (setq choose-list (append choose-list '("all" "clean" "generate")))
    
    (setq choice (ido-completing-read (format "COMPILE{%s}: " mode)  choose-list))
    
    (cond ((equal choice "generate")
           (my--compile "cmake %s .. %s"
                       (if (equal mode "debug")
                           "-DCMAKE_BUILD_TYPE=Debug"
                         "-DCMAKE_BUILD_TYPE=Release")
                       makefile-system-string))
          ((or (equal choice "all")
               (equal choice "clean"))
           (my--compile  "make %s"
                        choice))
          (t
           (my--compile "make %s"
                       choice)))))

(defun directory-executable-files (dir)
  (let ((ret '()))
    (dolist (f  (cddr (directory-files dir)) ret)
      ;; (message f)
      (when (file-executable-p (concat dir "/" f))
        (add-to-list 'ret f)))
    ret))

(defun my-cmake-run()
  (interactive)
  (let ((choice))
    (setq choice
          (ido-completing-read "RUN EXE: " (directory-executable-files "bin")))
    (compile (concat "cd bin && ./" choice))))

(defun my-cmake-help ()
  (interactive)
  (cmake-help-command)
  )

(defun my-cmake-test (arg)
  (interactive "P")
  (let (mode)
    (if arg (setq mode "release") (setq mode "debug"))
    ;; ctest -V : print the test info
    (compile (format "cd %s && ctest -V" mode))))

(add-hook 'cmake-mode-hook
          (lambda () (interactive)
            (local-set-key "\C-c\C-c" 'my-cmake-compile)
            (local-set-key "\C-c\C-e" 'my-cmake-run)
            (local-set-key "\C-c\C-t" 'my-cmake-test)
            (local-set-key "\C-ch" 'my-cmake-help)))

;; add cmake gtest error string
(require 'compile)
(add-to-list 'compilation-error-regexp-alist '("^[0-9]+: \\(.*?\\):\\([0-9]+\\): Failure$" 1 2))
(provide 'init-cmake)


