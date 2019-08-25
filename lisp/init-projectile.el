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
         (subdirs (seq-filter (lambda (name)
                                (not (or (string= name ".")
                                         (string= name ".."))))
                              (directory-files repo-folder)))
         (sub-repo-path nil))
    (dolist (subdir subdirs)
      (cond
       ((file-exists-p (cu-join-path repo-folder subdir ".repo"))
        (dolist (sub-repo-path
                 (seq-filter
                  (lambda (path)
                    (and (> (length path) 0)
                         (file-exists-p (cu-join-path repo-folder subdir path))))
                  (split-string
                   (shell-command-to-string
                    (format "cd %s && repo list --path-only"
                            (cu-join-path repo-folder subdir)))
                   "\n")))
          (let ((path (cu-join-path repo-folder subdir sub-repo-path)))
            (message "Add to projectile: {%s}" path)
            (projectile-add-known-project path))))
       (t
        (if-let* ((subs (shell-command-to-string (format "find %s -name packageInfo -maxdepth 2 -mindepth 2" (cu-join-path repo-folder subdir))))
                  (not-empty-string (not (equal "" subs))))
            (dolist (repo-path
                     (seq-filter
                      (lambda (path)
                        (and (> (length path) 0)
                             (file-exists-p path)))
                      (split-string subs)))
              (message "Add to projectile: {%s}" repo-path)
              (projectile-add-known-project repo-path))))))))
(provide 'init-projectile)
