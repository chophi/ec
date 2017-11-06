(require-package 'cpputils-cmake)
(require-package 'cmake-mode)

(defvar cmake-generate-makefile-type
  (if (or (eq system-type 'windows-nt)
          (eq system-type 'ms-dos)
          (eq system-type 'cygwin))
      "-G \"MSYS Makefiles\""
    "")
  "The extra parameter specifying the makefile type to generate")

(defvar cmake-executable-regexp-pair
  '("add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_-]*\\\)" 1)
  "The regexp for searching exectuable in cmake makefiles")

(defvar cmake-comment-prefix
  "#"
  "The prefix for comment line")

(defun cmake-get-executable-list (buf)
  "Get a exectuable list from a CMakeLists.txt buffer"
  (let ((result '()))
    (dolist (line
             (split-string
              (cu-buffer-content-without-comment-lines buf cmake-comment-prefix)
              "\n")
             result)
      (when (string-match (car cmake-executable-regexp-pair) line)
        (add-to-list 'result
                     (match-string (cadr cmake-executable-regexp-pair) line))))))

(with-eval-after-load "ido"

  (defun generate-cmake-target ()
    "Generate cmake target, it's mode awared, and mode is either debug or release,
and it's binded to the CMakeLists.txt as a buffer local variable, the mode can be
set by choosing set-mode in the choise-list"
    (interactive)
    (let ((current-cmake
           (cu-find-nearest-ancestor-match
            default-directory
            "CMakeLists.txt")))
      (when (not current-cmake)
        (error "Can't find a CMakeLists.txt for %s" default-directory))
      (let* ((dir (file-name-directory current-cmake))
             (mode (with-current-buffer (get-file-buffer current-cmake)
                     (if (boundp 'cmake--local-build-mode)
                         cmake--local-build-mode
                       (setq-local cmake--local-build-mode "debug"))))
             (mode-dir (cu-join-path dir mode))
             (choice (ido-completing-read
                      (format "Compile {%s}: " mode)
                      (append (cmake-get-executable-list (current-buffer))
                              '("set-mode" "all" "clean" "generate" "dist-clean")))))
        ;; redefine the compile command to be executed in dir
        (defmacro __compile (format-spec &rest objects)
          `(compile (format (concat "cd %s && " ,format-spec)
                            ,mode-dir ,@objects)))
        ;; create the directories and generate the makefile if not already exists.
        (when (not (file-exists-p mode-dir)) (make-directory mode-dir))

        (cond
         ;; set buffer local variable cmake--local-build-mode to debug/release
         ((equal choice "set-mode")
          (with-current-buffer (get-file-buffer current-cmake)
            (setq-local cmake--local-build-mode
                        (ido-completing-read "Set Mode to: " '("debug" "release")))))
         ;; re-generate the makefile
         ((equal choice "generate")
          (__compile "cmake -DCMAKE_BUILD_TYPE=%s .. %s"
                     (capitalize mode) cmake-generate-makefile-type))
         ;; remove the whole directory for current mode
         ((equal choice "dist-clean")
          (when (y-or-n-p (concat  "rm -rf " mode-dir))
            (shell-command (concat  "rm -rf " mode-dir))))
         ;; pass to make <target> directly
         (t (__compile "make %s" choice)))))))


(defun* directory-executable-files (dir)  
  (let ((ret '()))
    (when (not (file-directory-p dir))
      (return-from directory-executable-files ret))
    (dolist (f (directory-files dir) ret)
      (when (and (not (equal f "."))
                 (not (equal f ".."))
                 (file-executable-p (concat dir "/" f)))
        (add-to-list 'ret f)))))

(defun run-cmake-target ()
  "Run the cmake built target."
  (interactive)
  (let ((current-cmake
         (cu-find-nearest-ancestor-match
          default-directory
          "CMakeLists.txt")))
    (when (not current-cmake)
      (error "Can't find a CMakeLists.txt for %s" default-directory))
    (let* ((dir (file-name-directory current-cmake))
           (mode (with-current-buffer (get-file-buffer current-cmake)
                   (if (boundp 'cmake--local-build-mode)
                       cmake--local-build-mode
                     (setq-local cmake--local-build-mode "debug"))))
           (suffix (if (equal mode "debug") "_d" "_r"))
           (cmake-target-list (cmake-get-executable-list
                               (get-file-buffer current-cmake)))
           (bin-list (directory-executable-files "bin"))
           (choice-list (seq-filter
                         (lambda (filename) (and (file-exists-p filename)
                                                 (file-executable-p filename)))
                         (mapcar `(lambda (target)
                                    (cu-join-path ,mode target))
                                 cmake-target-list))))
      (setq cmake-target-list
            (mapcar (lambda (x) (concat x suffix)) cmake-target-list))
      (setq bin-list
            (remove-if-not (lambda (x) (member x cmake-target-list)) bin-list))
      (dolist (bin-file bin-list)
        (push (concat "bin/" bin-file) choice-list))
      (setq choice (ido-completing-read "RUN EXE: " choice-list))
      (compile choice))))

(defun my-cmake-help ()
  (interactive)
  (cmake-help-command))

(add-hook 'cmake-mode-hook
          (lambda () (interactive)
            (local-set-key "\C-c\C-c" 'generate-cmake-target)
            (local-set-key "\C-c\C-e" 'run-cmake-target)
            (local-set-key "\C-ch" 'cmake-help-command)))

;; Add cmake gtest error string
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^[0-9]+: \\(.*?\\):\\([0-9]+\\): Failure$" 1 2))

(provide 'init-cmake)


