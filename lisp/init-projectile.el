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

(defconst prj-recent-visited-project-cache (expand-file-name "projectile-recent-visited.el" user-emacs-cache-directory)
  "cache file for saving the current visited project")

(defun prj-get-recent-visited-projects ()
  (interactive)
  (if (file-exists-p prj-recent-visited-project-cache)
      (cu-load-expr-from-file prj-recent-visited-project-cache)
    '()))

(defun* prj-add-project-to-recent-visited-projects ()
  (interactive)
  (unless (projectile-project-p)
      (error "It's not in a projectile project"))
  (let* ((prjs (prj-get-recent-visited-projects))
         (cur-proj-root (projectile-project-root))
         (name (read-string "Please choose a name: " (file-name-base (substring cur-proj-root 0 -1))))
         (need-update nil)
         (exist-project-name nil))
    (when (assoc name prjs) (setq exist-project-name name))
    (when (rassoc cur-proj-root prjs)
      (setq exist-project-name (car (rassoc cur-proj-root prjs))))
    (when exist-project-name
      (when (equal exist-project-name name)
        (message "Already exist!")
        (return-from prj-add-project-to-recent-visited-projects nil))
      (when (y-or-n-p (message "Project already exist, rename? %s->%s (y/n): "
                             (car (assoc exist-project-name prjs)) name))
        (setf (car (assoc exist-project-name prjs)) name)
        (setq need-update t))
      (when need-update
        (cu-save-expr-to-file prj-recent-visited-project-cache prjs))
      (return-from prj-add-project-to-recent-visited-projects nil))
    (add-to-list 'prjs (cons name cur-proj-root))
    (cu-save-expr-to-file prj-recent-visited-project-cache prjs)))

(defun prj-switch-to-recent-visited-projects ()
  (interactive)
  (let ((prjs (prj-get-recent-visited-projects)))
    (unless prjs
      (error "No recent visited projects set yet"))
    (projectile-switch-project-by-name
     (cdr (assoc
           (if (> (length prjs) 1)
               (ido-completing-read "Choose a project: " (mapcar 'car prjs))
             (car (car prjs)))
           prjs)))))

(provide 'init-projectile)
