(require-package 'dash)
(require-package 'magit)

;; using magit to manage git
;; (delete 'Git vc-handled-backends)
;; not use SVN
;; (delete 'SVN vc-handled-backends)
(setq vc-handled-backends nil)

;; (when (eq os 'macos)
;;   (setq magit-git-executable "/opt/local/bin/git"))

;; (setq magit-last-seen-setup-instructions "1.4.0")

(setq-default git-commit-fill-column 70)

(defun magit-log-set-grafts ()
  (interactive)
  (let ((message (magit-commit-at-point))
        (file-name
         (format "%s.git/info/grafts" (expand-file-name default-directory))))
    (if (not message)
        (error "no commit at point")
      (when (y-or-n-p
             (format "write %s to %s" message file-name)))
      (shell-command
       (format
        "echo `git log -1 --pretty=format:\"%%H\" %s` > %s"
        message file-name)))))

(defun magit-log-remove-grafts ()
  (interactive)
  (let ((file-name
         (format "%s.git/info/grafts" (expand-file-name default-directory))))
    (when (and (file-exists-p file-name)
               (y-or-n-p (format "Remove grafts: [%s]" file-name)))
      (shell-command (format "rm %s" file-name)))))

(with-eval-after-load "magit-log"
  (plist-put
   magit-log-popup :actions
   (let ((lst (plist-get magit-log-popup :actions)))
     (dolist (to-add
              '((?g "Set Grafts" magit-log-set-grafts)
                (?G "Remove Grafts" magit-log-remove-grafts))
              lst)
       (setq lst (add-to-list 'lst to-add t))))))

(provide 'init-magit)
