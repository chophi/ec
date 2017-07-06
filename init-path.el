(require 'init-common-utils)

(defun add-to-path(path &optional as-head)
  (when (cu-is-dir-or-dirlink? path)
    (if as-head
        (setenv "PATH"
                (concat (expand-file-name path) *path-separator* (getenv "PATH")))
        (setenv "PATH"
                (concat (getenv "PATH") *path-separator* (expand-file-name path))))))

(defconst *linux-extra-path-list*
  '(("~/.emacs.d/.emacs-bin" t)
    ("~/.linux_script" t)
    ("/usr/local/texlive/2015/bin/x86_64-linux"))
  "The list of paths which will be add to path on linux if the path exists")

(defconst *mac-extra-path-list*
  '(("~/bin" t)
    ("/usr/local/bin" t)
    ("~/bin/common-scripts" t)
    ("/usr/local/texlive/2016/bin/universal-darwin")
    ("~/Library/Android/sdk/platform-tools/")
    ("~/Library/Android/android-ndk-r10e/")
    ("~/Library/Android/sdk/tools/"))
  "The list of paths which will be add to path on linux if the path exists")


(dolist (extra-path-list
         (case system-type
           (darwin *mac-extra-path-list*)
           (gnu/linux *linux-extra-path-list*)
           (t '())))
  (let ((path (car extra-path-list))
        (as-head (cadr extra-path-list)))
    (add-to-path path as-head)))

(provide 'init-path)
