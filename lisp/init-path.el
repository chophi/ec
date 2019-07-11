(require 'init-common-utils)
(require 'cl)

(require-package 'exec-path-from-shell)
;; use interactive shell, and no startup file check.
(setq exec-path-from-shell-check-startup-files nil)
;;; For mac, use /usr/local/bin/bash, which is brew installed bash.
(cond
 ((company-computer-p) (setq shell-file-name "/bin/zsh"))
 ((and (equal system-type 'darwin) (file-exists-p "/usr/local/bin/bash"))
  (setq shell-file-name "/usr/local/bin/bash")))

(setq exec-path-from-shell-variables
      '("PATH" "MANPATH" "INFOPATH"
        "LDFLAGS" "CPPFLAGS"            ; for clang
        "JAVA_HOME" "JRE_HOME" "CLASSPATH"  ; for java
        "PKG_CONFIG_PATH" "LD_LIBRARY_PATH"
        "ANDROID_NDK_HOME" "ANDROID_SDK_HOME"
        "VIRTUAL_ENV" "SHELL"))
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
(provide 'init-path)
