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

    (if arg (setq mode "release")
      (setq mode "debug"))

    (setq choose-list
          (extract-list nil "#" "add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_-]*\\\)" 1))
    (setq choose-list (append choose-list '("all" "clean" "generate" "dist-clean")))
    
    (setq choice (ido-completing-read (format "COMPILE{%s}: " mode)  choose-list))

    ;; redefine the compile command to be executed in dir
    (defmacro my--compile (string &rest objects)
      `(compile (concat "cd " ,dir ,mode " && " (format ,string ,@objects))))

    (when (not (file-exists-p (concat dir mode))) (make-directory (concat dir mode)))
    (when (not (file-exists-p (concat dir mode "/Makefile")))
      (my--compile "cmake .. %s" makefile-system-string))

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
          ((equal choice "dist-clean")
           (when (y-or-n-p (concat  "rm -rf " dir mode))
             (shell-command (concat  "rm -rf " dir mode))))
          (t
           (my--compile "make %s"
                       choice)))))

(defun* directory-executable-files (dir)  
  (let ((ret '()))
    (when (not (file-directory-p dir))
      (return-from directory-executable-files ret))
    (dolist (f  (cddr (directory-files dir)) ret)
      ;; (message f)
      (when (file-executable-p (concat dir "/" f))
        (add-to-list 'ret f)))
    ret))

(defun my-cmake-run(arg)
  (interactive "P")
  (let (choice cmake-target-list bin-list mode suffix choice-list)

    (if arg (setq mode "release" suffix "_r")
      (setq mode "debug" suffix "_d"))
    
    (setq cmake-target-list
          (extract-list nil "#" "add_executable\\\s*(\\\s*\\\([0-9a-zA-Z_-]*\\\)" 1)
          bin-list
          (directory-executable-files "bin"))
    (dolist (target cmake-target-list)
      (when (and (file-exists-p (concat mode "/" target))
                 (file-executable-p (concat mode "/" target)))
        (push (concat mode "/" target) choice-list)))
    (setq cmake-target-list (mapcar (lambda (x) (concat x suffix)) cmake-target-list))
    (setq bin-list (remove-if-not (lambda (x) (member x cmake-target-list)) bin-list))
    (dolist (bin-file bin-list)
      (push (concat "bin/" bin-file) choice-list))
    (setq choice (ido-completing-read "RUN EXE: " choice-list))
    (compile choice)))

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

(defvar cmake-project-lists '())

(let ((f "~/.emacs.d/.cmake-project-lists.el"))
  (when (file-exists-p f)
    (load-file f)))

;; redefine the c-mode-base-map
(require 'init-smart-compile)
(require 'init-smart-run)

(defun get-maybe-cmake ()
  (let ((bufname (buffer-file-name))
        (maybe-cmake nil)
        (cmakefile nil))
    (catch 'loop
      (dolist (proj cmake-project-lists)
        (setq maybe-cmake (concat proj "/CMakeLists.txt"))
        ;; (message "[%s,%s]" proj (substring bufname 0 (length proj)))
        (when (and (file-exists-p maybe-cmake)
                   (<= (length proj) (length bufname))
                   (equal proj (substring bufname 0 (length proj))))
          (setq cmakefile maybe-cmake)
          (throw 'loop cmakefile))))
    cmakefile))


(if *linux?*
    (setq *ndk-build-bin-path* "~/software/android-ndk-r10e")
  (setq *ndk-build-bin-path* "path-to-android-ndk-build"))

(defun my-smart-compile ()
  (interactive)
  (if (file-exists-p (format "%s/%s"
                             (file-name-directory (buffer-file-name))
                             "Android.mk"))
      (progn (compile (format "PATH=$PATH:%s ndk-build"
                              *ndk-build-bin-path*)))
    (let ((maybe-cmake nil))
      (setq maybe-cmake (get-maybe-cmake))
      (if maybe-cmake
          (progn (find-file-noselect maybe-cmake)
                 (with-current-buffer (get-file-buffer maybe-cmake)
                   (call-interactively 'my-cmake-compile)))
        (call-interactively 'smart-compile)))))

(require 'init-custom-compile)
(defun my-smart-run ()
  (interactive)
  (if (file-exists-p (format "%s/%s"
                             (file-name-directory (buffer-file-name))
                             "Android.mk"))
      (let ((binary (ido-read-file-name "Select a binary to run: "
                                        "../libs/")))
        (send-command-to-terminal
         (choose-buffer-local-terminal)
         (format "adb push %s /data/ && adb wait-for-device && adb shell /data/%s"
                 binary (file-name-nondirectory binary))))
    (let ((maybe-cmake (get-maybe-cmake)))
      (if maybe-cmake
          (progn (find-file-noselect maybe-cmake)
                 (with-current-buffer (get-file-buffer maybe-cmake)
                   (call-interactively 'my-cmake-run)))
        (call-interactively 'run-c-program)))))

(define-key c-mode-base-map "\C-c\C-c" 'my-smart-compile)
;; can't set using define-key, so change to set hook here!
;; (define-key c-mode-base-map "\C-c\C-e" 'my-smart-run)
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key "\C-c\C-e" 'my-smart-run)))

(defun my-jump-to-cmake ()
  (interactive)
  (let ((maybe-cmake (get-maybe-cmake)))
     (if maybe-cmake
         (find-file-other-window maybe-cmake)
       (error "no cmake file related"))))

(add-hook 'c-mode-common-hook
          (lambda () (local-set-key "\C-cm" 'my-jump-to-cmake)))

(provide 'init-cmake)


