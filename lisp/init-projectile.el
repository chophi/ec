(require 'projectile)
(projectile-mode)

(defun prj-get-repo-project-roots ()
  "Get the repo projects"
  (let (result)
    (dolist (repo (mapcar (lambda (dot-repo) (file-name-directory dot-repo))
                          (cu-search-file-under "~/repo" ".repo" 2 3)) result)
      (setq result (append result
                           (mapcar (lambda (dir) (cu-join-path repo dir))
                                   (split-string
                                    (shell-command-to-string
                                     (format "cd %s && repo list --path-only" repo))
                                    "\n")))))))

(defun prj-get-brazil-project-roots ()
  "Get the brazil projects"
  (let (result)
    (dolist (workspace (mapcar (lambda (packageInfoPath) (file-name-directory packageInfoPath))
                               (cu-search-file-under "~/repo" "packageInfo" 3 3)) result)
      (setq result (append result (mapcar (lambda (file) (cu-normalize-filename (file-name-directory file)))
                                          (cu-search-file-under workspace "Config" 3 4)))))))

(defconst prj-get-project-functions
  '(prj-get-repo-project-roots prj-get-brazil-project-roots)
  "Function list of function to get the project list")

(defun prj-get-all-projects ()
  (let (result)
    (dolist (func prj-get-project-functions result)
      (setq result (append result (funcall func))))))


(defun prj-add-all-known-projects ()
  (interactive)
  (dolist (prj (prj-get-all-projects))
    (when (projectile-project-p prj)
      (projectile-add-known-project prj))))

(provide 'init-projectile)
