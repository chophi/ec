(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defun path-seperator()
  (if *is-windows-system-p* ";" ":"))

(defun existed-directory? (path)
  (and (file-exists-p path)
       (let ((first-attr (car (file-attributes path))))
         (if (eq first-attr t)
             t
           (and (stringp first-attr) (existed-directory? first-attr))))))

(defun add-to-path(path &optional insert-to-head)
  (when (existed-directory? path)
    (if insert-to-head
        (setenv "PATH"
                (concat (expand-file-name path) (path-seperator) (getenv "PATH")))
        (setenv "PATH"
                (concat (getenv "PATH") (path-seperator) (expand-file-name path))))))

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
         (if *is-mac-machine*
             *mac-extra-path-list*
           *linux-extra-path-list*))
  (let ((path (car extra-path-list))
        (insert-to-head (cadr extra-path-list)))
    (add-to-path path insert-to-head)))

(provide 'init-path)
