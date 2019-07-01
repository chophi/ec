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

(provide 'init-projectile)
