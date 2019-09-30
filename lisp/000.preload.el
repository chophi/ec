;;; Identify the OS and computer emacs runs
;;; and provide the variable *os* for condition checking
(defconst os
  (cond ((or (eq system-type 'darwin) (eq system-type 'macos)) 'darwin)
        ((eq system-type 'gnu/linux) 'linux)
        ((eq system-type 'windows-nt) 'windows)
        (t nil))
  "Operating System, can be 'darwin, 'linux or 'windows")

(defun company-computer-p ()
  (file-exists-p "~/.COMPANY_COMPUTER"))

;;; For benchmark the require
(defun benchmark/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar benchmark/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
  "Note in `benchmark/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'benchmark/require-times
                     (cons feature
                           (benchmark/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))

(defun benchmark/show-metrics ()
  (interactive)
  (setq benchmark/require-times
        (sort benchmark/require-times (lambda (a b) (> (cdr a) (cdr b)))))
  (let ((buf (get-buffer-create "*benchmark.require-times*")))
    (with-current-buffer buf
        (dolist (var benchmark/require-times)
          (insert (format "%s: %s\n" (car var) (cdr var)))))
    (switch-to-buffer buf)))

;;; Setup the path
;; use interactive shell, and no startup file check.
(setq exec-path-from-shell-check-startup-files nil)

;; For mac, use /usr/local/bin/bash, which is brew installed bash.
(cond
 ((file-exists-p "/apollo/env/envImprovement/var/bin/zsh")
  (setq shell-file-name "/apollo/env/envImprovement/var/bin/zsh"))
 ((file-exists-p "/bin/zsh") (setq shell-file-name "/bin/zsh"))
 ((and (equal system-type 'darwin) (file-exists-p "/usr/local/bin/bash"))
  (setq shell-file-name "/usr/local/bin/bash")))

(setq exec-path-from-shell-variables
      '("PATH" "MANPATH" "INFOPATH"
        "LDFLAGS" "CPPFLAGS"            ; for clang
        "JAVA_HOME" "JRE_HOME" "CLASSPATH"  ; for java
        "PKG_CONFIG_PATH" "LD_LIBRARY_PATH"
        "ANDROID_NDK_HOME" "ANDROID_SDK_HOME"
        "VIRTUAL_ENV" "SHELL" "REPO_PUBLIC_ROOT" "REPO_PRIVATE_ROOT"))
(exec-path-from-shell-initialize)

(defun add-to-path(path &optional as-head)
  "Add the PATH to the environment PATH, append by default
if AS-HEAD not specified or nil,
but can also be added to head if AS-HEAD is not nil"
  (when (file-directory-p path)
    (let ((a (expand-file-name path))
          (p (getenv "PATH")))
      (if as-head
          (setenv "PATH" (concat a path-separator p))
        (setenv "PATH" (concat p path-separator a)))
      (add-to-list 'exec-path a (not as-head)))))

(add-to-path (concat (getenv "HOME") "/.emacs.d/scripts"))

(require 'use-package)

(provide '000.preload)
