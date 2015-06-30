;;; uniform the environment in term-mode and find-file use an util function
;;; which can input a environment name as a path parameter as the start dir
(defconst emacs-env-file "~/.emacs-env")

(defun read-env-alist-from-file ()
  ;; (interactive)
  (let ((start 0)
        (str (with-current-buffer (find-file-noselect emacs-env-file)
               (buffer-substring-no-properties (point-min) (point-max))))
        (env-alist '())
        env
        value)
    (while (string-match "^\\([0-9a-zA-Z_]+\\)=\\([$/~0-9a-zA-Z_.-]+\\)" str start)
      (setq env (match-string 1 str)
            value (match-string 2 str)
            start (match-end 2))
      (add-to-list 'env-alist `(,env ,value)))
    env-alist))

(defvar my-env-alist
  '()
  "environment strings")

(defun read-env-and-push (env)
  (let (value)
    (setq value (read-string (format "%s: " env)))
    (when (> (length value 0))
      (shell-command (format "echo %s=%s >> %s" env value emacs-env-file))
      (setenv env value)
      (add-to-list 'my-env-alist `(,env ,value) )
      )))

(defun my-push-env ()
  (interactive)
  (let (env)
    (setq env (read-string "ENV: "))
    (if (getenv env)
      (error "ENV exists: [%s]=[%s]" env (getenv env))
      (read-env-and-push env)
      (setenv env (my-env-expand env)))))

(defun my-env-expand (env)
  ;;(message "ENV:%s" env)
  (let (value first-part second-part)
    (setq value (cadr (assoc env my-env-alist)))
    (when (not value)
      (setq value (getenv env)))
    ;;(message "Value: %s" value)
    (setq first-part (car (split-string value "/")))
    (if (and (> (length first-part) 0) (equal (elt first-part 0) ?$))
        (concat (my-env-expand (substring first-part 1))
                (substring value (length first-part)))
      value)))

(defun my-env-find-file ()
  ;; (interactive)
  (let ((choices '())
        env
        value)
    (dolist (env-pair my-env-alist)
      (add-to-list 'choices (car env-pair)))
    (setq env (ido-completing-read "Root Env Path: " choices))
    (setq value (my-env-expand env))
    (if (file-exists-p value)
        (if (file-directory-p value)
            (find-file (ido-read-file-name "File Name: " value))
          (find-file value))
      (error "Path {%s} doesn't exist" value))))

(defun update-env-alist()
  (interactive)
  (dolist (env-li my-env-alist)
    ;;; unset all environment in env-alist
    (setenv (car env-li)))
  (setq my-env-alist (read-env-alist-from-file))
  (dolist (env-li my-env-alist)
    (setenv (car env-li) (my-env-expand (car env-li)))))

(update-env-alist)

;; (global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-S-x C-S-f") 'my-env-find-file)
(provide 'init-uniform-env)

