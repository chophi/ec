(require 'init-common-utils)
(require 'cl)

(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  ;; exec-path-from-shell-check-startup-files to disable
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(defun add-to-path(path &optional as-head)
  "Add the PATH to the environment PATH, append by default
if AS-HEAD not specified or nil,
but can also be added to head if AS-HEAD is not nil"
  (when (file-directory-p path)
    (setenv "PATH"
            (let ((a (expand-file-name path))
                  (p (getenv "PATH")))
              (if as-head
                  (concat a path-separator p)
                (concat p path-separator a))))))

(defconst linux-extra-path-list
  '(("/usr/local/texlive/2015/bin/x86_64-linux"))
  "The list of paths which will be add to path on linux if the path exists")

(defconst mac-extra-path-list
  '(("/usr/local/bin" t)
    ("/usr/local/texlive/2016/bin/universal-darwin")
    ("~/Library/Android/sdk/platform-tools/")
    ("~/Library/Android/android-ndk-r10e/")
    ("~/Library/Android/sdk/tools/"))
  "The list of paths which will be add to path on linux if the path exists")

(defconst bash-config-root-dir "~/git-repo/config")
(defconst common-extra-path-list
  (mapcar
   (lambda (path)
     (let ((replace-pairs
            `(("${CONFIG_ROOT_DIR}" ,bash-config-root-dir)
              ("${MY_HOST_SYSTEM}"
               ,(case system-type
                  (darwin "darwin")
                  (gnu/linux "linux")
                  (t "unknown"))))))
       (dolist (p replace-pairs)
         (setq path (replace-regexp-in-string (car p) (cadr p) path t))))
     (list path t))
   '("${CONFIG_ROOT_DIR}/bin/common"
     "${CONFIG_ROOT_DIR}/bin/common/decompile-apk"
     "${CONFIG_ROOT_DIR}/bin/${MY_HOST_SYSTEM}"
     "${CONFIG_ROOT_DIR}/private/bin/common"
     "${CONFIG_ROOT_DIR}/private/bin/${MY_HOST_SYSTEM}"
     "${CONFIG_ROOT_DIR}/script/common"
     "${CONFIG_ROOT_DIR}/script/${MY_HOST_SYSTEM}"
     "${CONFIG_ROOT_DIR}/private/script/common"
     "${CONFIG_ROOT_DIR}/private/script/${MY_HOST_SYSTEM}")))

(dolist (extra-path-list
         (append
          common-extra-path-list
          (case system-type
            (darwin mac-extra-path-list)
            (gnu/linux linux-extra-path-list)
            (t '()))))
  (let ((path (car extra-path-list))
        (as-head (cadr extra-path-list)))
    (add-to-path path as-head)))

;; source emacs global variable
(setenv "LD_LIBRARY_PATH"
        (concat (getenv "LD_LIBRARY_PATH")
                ":/usr/local/lib:/usr/local/lib32:/usr/local/lib64"))

(when (eq system-type 'darwin)
  (let ((man-list
         '("/usr/local/share/man"
           "/usr/share/man"))
        (cellar-man
         (shell-command-to-string "find /usr/local/Cellar -name \"man\""))
        (tmp ""))
    (dolist (cm (split-string cellar-man))
      (add-to-list 'man-list cm))
    (dolist (cm man-list)
      (when (file-exists-p cm)
        (setq tmp (concat tmp ":" cm))))
    (unless (equal tmp "")
      (setenv "MANPATH" (concat (getenv "MANPATH") ":" tmp)))))

(provide 'init-path)
