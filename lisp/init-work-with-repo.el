(require 'init-custom-compile)

(defun repo-root ()
  (interactive)
  (let ((repo (_find-file-or-dir-recursively default-directory ".repo")))
    (when (not repo)
      (error "repo not found"))
    (file-name-directory repo)
    ))

(defconst *repo-directory* "~/.platform_script")
(defun _repo-list-raw (&optional root)
  (interactive)
  (shell-command-to-string (format "cd %s && %s/repo list" root *repo-directory*)))

(defun _parse-repo-list (string root)
  (let (repo-list)
    (setq repo-list (split-string string "\n"))
    (setq repo-list (mapcar (lambda (unit) (split-string unit " : ")) repo-list))
    (setq repo-list (remove-if-not
                     (lambda (unit) (and (equal (length unit) 2)
                                         (or (file-exists-p (format "%s/%s" root (car unit)))
                                             (file-exists-p (format "%s/%s" root (cadr unit))))))
                                   repo-list))))

(defun repo-list-buffer-name (root)
  (format "[*Repo List{%s}*]" root))

(defun repo-list-buffer(root)
  (get-buffer (repo-list-buffer-name root)))

(defun repo-project-list (root)
  (if (repo-list-buffer root)
    (with-current-buffer (repo-list-buffer root)
      (buffer-local-value 'buffer-local-repo-project-list (current-buffer)))
    (with-current-buffer (get-buffer-create (repo-list-buffer-name root))
      (setq-local buffer-local-repo-project-list (_parse-repo-list (_repo-list-raw root) root)))))

(defun repo-list-projects (&optional root)
  (interactive)
  (when (not root) (setq root (repo-root)))
  (let* ((project-list (repo-project-list root))
         (buffer (repo-list-buffer root)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (goto-char 1)
      (mapcar (lambda (unit)
                (when (cadr unit)
                  (insert (format "%-70s %s\n" (car unit) (cadr unit)))))
              project-list)
      (switch-to-buffer buffer)
      (line-select-mode)
      (read-only-mode 1)
      (setq-local buffer-local-line-list (split-string (buffer-string) "\n"))
      (goto-char 1)
      (local-set-key "f" '_filter-line-base-on-input)
      (local-set-key "g" `(lambda () (interactive) (_jump-to-project-at-point ,root)))
      (local-set-key [return] `(lambda () (interactive) (_jump-to-project-at-point ,root)))
      (local-set-key (kbd "RET") `(lambda () (interactive) (_jump-to-project-at-point ,root)))
      (local-set-key "u" '_update-content)
      (local-set-key "?" (lambda () (interactive) (message "f : filter line\ng: jump to project\nreturn: jump to project\nu: update content"))))))

(defun _filter-line-base-on-input ()
  (interactive)
  (let (input)
    (setq input (read-input "Please input filter: "))
    (read-only-mode -1)
    (erase-buffer)
    (mapcar (lambda (unit) (when (and unit (string-match-p input unit))
                             (insert (format "%s\n" unit))))
            (buffer-local-value 'buffer-local-line-list (current-buffer)))
    (goto-char 1)
    (read-only-mode 1)))

(defun _update-content ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (mapcar (lambda (unit) (when unit (insert (format "%s\n" unit))))
          (buffer-local-value 'buffer-local-line-list (current-buffer)))
  (read-only-mode 1)
  (goto-char 1))

(defun _jump-to-project-at-point (&optional root)
  (let (line-string path full-path)
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
