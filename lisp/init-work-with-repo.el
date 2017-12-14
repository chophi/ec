(require 'init-custom-compile)

(defvar g-repo-directory "~/.platform_script" "Dir where repo executable locates")
(defvar g-repo-ws (or (getenv "DEFAULT_REPO_WS")
                      (if (boundp 'g-default-repo-ws) g-default-repo-ws nil))
  "The workspace for repo project")
(defvar g-repo-proj-list nil "The default project list")

(defun repo-ws ()
  (interactive)
  (let ((repo (cu-find-nearest-ancestor-match default-directory ".repo")))
    (when (not repo)
      (error "repo not found"))
    (file-name-directory repo)))

(defun current-repo-ws ()
  (or g-repo-ws (repo-ws)))

(defun gen-repo-list (&optional root)
  (interactive)
  (setq root (or root (current-repo-ws)))
  (let* ((raw (shell-command-to-string
               (format "cd %s && %s/repo list"
                       root g-repo-directory)))
         (proj-list (split-string raw "\n")))
    (setq proj-list
          (mapcar
           (lambda (unit) (split-string unit " : "))
           proj-list))
    (setq proj-list
          (remove-if-not
           (lambda (unit)
             (and (equal (length unit) 2)
                  (or (file-exists-p (format "%s/%s" root (car unit)))
                      (file-exists-p (format "%s/%s" root (cadr unit))))))
           proj-list))))

(defun change-repo-ws ()
  (interactive)
  (let ((dir (read-directory-name
              (message "Current workspace is: %s\nChange to: " g-repo-ws))))
    (when (not (file-exists-p (cu-join-path dir ".repo")))
      (error "There's no repo in %s" dir))
    (when (not (equal dir g-repo-ws))
      (setq g-repo-ws dir
            g-repo-proj-list (gen-repo-list dir)))))

(defun repo-goto-project ()
  (interactive)
  (when (not g-repo-ws)
    (error "Please set the repo workspace first!"))
  (when (not (file-exists-p (cu-join-path g-repo-ws ".repo")))
    (error "There's no repo in %s" g-repo-ws))
  (when (not g-repo-proj-list)
    (setq g-repo-proj-list (gen-repo-list g-repo-ws)))
  (find-file
   (cu-join-path
    g-repo-ws
    (completing-read (format "Goto [ws: %s]:\n  " g-repo-ws) g-repo-proj-list))))

(with-eval-after-load "org" (define-key org-mode-map "\C-c\C-r" nil))
(cu-set-key-bindings global-map "\C-c\C-r"
                     '((?c . change-repo-ws)
                       (?g . repo-goto-project)))

(provide 'init-work-with-repo)
