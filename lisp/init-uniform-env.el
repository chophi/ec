;;; uniform the environment in term-mode and find-file use an util function
;;; which can input a environment name as a path parameter as the start dir
(defconst ue-emacs-env-file "~/.emacs.d/.emacs-env"
  "The file to save the unified paths")

(defun ue-read-env-alist-from-file ()
  ;; (interactive)
  (let ((start 0)
        (str
         (with-current-buffer (find-file-noselect ue-emacs-env-file)
           (buffer-substring-no-properties (point-min) (point-max))))
        (env-alist '())
        (env nil)
        (value nil))
    (while (string-match
            "^\\(export +\\)?\\([0-9a-zA-Z_-]+\\)=\\([$/~0-9a-zA-Z_.-]+\\)"
            str start)
      (setq env (match-string 2 str)
            value (match-string 3 str)
            start (match-end 0))
      (add-to-list 'env-alist `(,env ,value)))
    env-alist))

(defvar ue-env-alist
  '()
  "environment strings")

(defun ue-expand-env (env)
  ;;(message "ENV:%s" env)
  (let (value first-part second-part)
    (setq value (cadr (assoc env ue-env-alist)))
    (when (not value)
      (setq value (getenv env)))
    ;;(message "Value: %s" value)
    (setq first-part (car (split-string value "/")))
    (if (and (> (length first-part) 0) (equal (elt first-part 0) ?$))
        (concat (ue-expand-env (substring first-part 1))
                (substring value (length first-part)))
      value)))

(defun ue-env-find-file ()
  (interactive)
  (let ((choices '())
        env
        value)
    (dolist (env-pair ue-env-alist)
      (add-to-list 'choices (car env-pair)))
    (setq env (ido-completing-read "Root Env Path: " choices))
    (setq value (ue-expand-env env))
    (if (file-exists-p value)
        (if (file-directory-p value)
            (find-file (ido-read-file-name "File Name: " value))
          (find-file value))
      (error "Path {%s} doesn't exist" value))))

(defun ue-update-env-alist()
  (interactive)
  (dolist (env-li ue-env-alist)
    ;;; unset all environment in env-alist
    (setenv (car env-li)))
  (setq ue-env-alist (ue-read-env-alist-from-file))
  (dolist (env-li ue-env-alist)
    (setenv (car env-li) (ue-expand-env (car env-li)))))

(ue-update-env-alist)

(defun ue-insert-to-env-list ()
  (interactive)
  (let ((file-path (ido-read-file-name "File PATH:"))
        (name (read-string "Name: ")))
  (with-current-buffer (find-file-noselect ue-emacs-env-file)
    (goto-char (buffer-end 1))
    (insert (format "%s=%s\n" name (expand-file-name file-path)))
    (save-buffer)
    (ue-update-env-alist))))

(provide 'init-uniform-env)
