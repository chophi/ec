(require 'init-common-utils)
(require 'cl)

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
  '(("~/.emacs.d/.emacs-bin" t)
    ("~/.linux_script" t)
    ("/usr/local/texlive/2015/bin/x86_64-linux"))
  "The list of paths which will be add to path on linux if the path exists")

(defconst mac-extra-path-list
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
           (darwin mac-extra-path-list)
           (gnu/linux linux-extra-path-list)
           (t '())))
  (let ((path (car extra-path-list))
        (as-head (cadr extra-path-list)))
    (add-to-path path as-head)))

(provide 'init-path)
