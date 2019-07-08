(global-unset-key "\C-cp")
(require-package 'projectile)

(require 'projectile)
(projectile-mode)

(defun* projectile-ignored-project-function (project-root)
  (let ((ignored-repo-projects '("\\.repo/projects" "\\.repo/project-objects")))
    (dolist (ignore-pattern ignored-repo-projects)
      (when (string-match ignore-pattern project-root)
        (return-from projectile-ignored-project-function t))
      )
    nil))

(defun add-repos-to-projectile ()
  (interactive)
  (let* ((repo-folder "~/repo")
         (sub-types (seq-filter (lambda (name)
                                  (not (or (string= name ".")
                                           (string= name ".."))))
                                (directory-files repo-folder)))
         (sub-repo-path nil))
    (dolist (type sub-types)
      (dolist (sub-repo-path
               (seq-filter
                (lambda (path)
                  (and (> (length path) 0)
                       (file-exists-p (cu-join-path repo-folder type path))))
                (split-string
                 (shell-command-to-string
                  (format "cd %s && repo list --path-only"
                          (cu-join-path repo-folder type)))
                 "\n")))
        (projectile-add-known-project
         (cu-join-path repo-folder type sub-repo-path))))))
(provide 'init-projectile)
