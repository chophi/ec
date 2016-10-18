(defvar *repo-root* "~/workspace/mg" "The root directory for repo")
(defvar *repo-project-list* nil "The list for projects")

(defun _repo-list-raw (&optional root)
  (interactive)
  (when (eq root nil) (setq root *repo-root*))
  (shell-command-to-string (format "cd %s && repo list" root)))

(defvar emacs_cache_dir "~/.emacs-cache" "The cache directory for emacs")
(when (not (file-exists-p emacs_cache_dir))
  (make-directory emacs_cache_dir))

(defun _repo_get_cache_prefix ()
  (format "%s/repo_%s_cache" emacs_cache_dir (file-name-nondirectory *repo-root*)))

(defun repo-switch-root (&optional root)
  (interactive)
  (when (or (eq root nil) (not (file-exists-p root))
            (not (file-exists-p (format "%s/.repo" root))))
    (error "the root you specify contains no repo"))
  (setq *repo-root* root))

(defun _parser-repo-list (string)
  (let (repo-list)
    (setq repo-list (split-string string "\n"))
    (setq repo-list (mapcar (lambda (unit) (split-string unit " : ")) repo-list))))

(defun _repo-generate-list (&optional root)
  (interactive)
  (when (eq nil root) (setq root *repo-root*))
  (setq *repo-project-list* (_parser-repo-list (_repo-list-raw root))))

(defun repo-list-projects (&optional root)
  (interactive)
  (let (buffer buffer-name)
    (when (not (eq root *repo-root*)) (_repo-generate-list root))
    (when (eq root nil) (setq root *repo-root*))
    (setq buffer-name (format "[*Repo List{%s}*]" root))
    (if (get-buffer "")
        (switch-to-buffer buffer-name)
      (progn
        (setq buffer (get-buffer-create buffer-name))
        (with-current-buffer buffer
          (read-only-mode -1)
          (erase-buffer)
          (goto-char 1)
          (mapcar (lambda (unit) (when (cadr unit) (insert (format "%-70s %s\n" (car unit) (cadr unit))))) *repo-project-list*)
          (switch-to-buffer buffer)
          (line-select-mode)
          (read-only-mode 1)
          (setq-local buffer-line-list (split-string (buffer-string) "\n"))
          (goto-char 1)
          (local-set-key "f" '_filter-line-base-on-input)
          (local-set-key "g" `(lambda () (interactive) (_jump-to-project-at-point ,root)))
          (local-set-key [return] `(lambda () (interactive) (_jump-to-project-at-point ,root)))
          (local-set-key (kbd "RET") `(lambda () (interactive) (_jump-to-project-at-point ,root)))
          (local-set-key "u" '_update-content)
          (local-set-key "?" (lambda () (interactive) (message "f : filter line\ng: jump to project\nreturn: jump to project\nu: update content"))))))))


(defun repo-list-select-projects ()
  (interactive)
  (let ((dir-list (cddr (directory-files "~/workspace")))
        (choice-list '())
        choice)
    (dolist (dir dir-list)
      (when (file-exists-p (concat "~/workspace/" dir "/.repo"))
        (add-to-list 'choice-list dir)))
    (setq choice (ido-completing-read "select the repo project: " choice-list))
    (repo-list-projects (concat "~/workspace/" choice))))

(defun _filter-line-base-on-input ()
  (interactive)
  (let (input)
    (setq input (read-input "Please input filter: "))
    (read-only-mode -1)
    (erase-buffer)
    (mapcar (lambda (unit) (when (and unit (string-match-p input unit))
                             (insert (format "%s\n" unit)))) buffer-line-list)
    (goto-char 1)
    (read-only-mode 1)))

(defun _update-content ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (mapcar (lambda (unit) (when unit (insert (format "%s\n" unit)))) buffer-line-list)
  (read-only-mode 1)
  (goto-char 1))

(defun _jump-to-project-at-point (&optional root)
  (interactive)
  (let (line-string path full-path)
    (when (eq root nil) (setq root *repo-root*))
    (setq line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (setq path (car (split-string line-string " ")))
    (setq full-path (format "%s/%s" root path))
    (if (file-exists-p full-path)
        (dired full-path)
      (error "path not exist"))))

(define-minor-mode line-select-mode
  "Select line mode"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Line-Select"
  ;; The minor mode bindings.
  :group 'line-select)


(provide 'init-work-with-repo)
