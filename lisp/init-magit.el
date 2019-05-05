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

(defun magit-my-ediff-show-commit ()
  (interactive)
  (magit-ediff-show-commit (magit-commit-at-point)))

;; (with-eval-after-load "magit-log"
;;   (plist-put
;;    magit-log-popup :actions
;;    (let ((lst (plist-get magit-log-popup :actions)))
;;      (dolist (to-add
;;               '((?g "Set Grafts" magit-log-set-grafts)
;;                 (?G "Remove Grafts" magit-log-remove-grafts)
;;                 (?e "Ediff commits" magit-my-ediff-show-commit))
;;               lst)
;;        (setq lst (add-to-list 'lst to-add t))))))

(defun gerrit-get-remote ()
  (interactive)
  (let* ((get-remote-command "git remote -v | grep push")
         (remote (shell-command-to-string get-remote-command)))
    (when remote
      (cu-strip-string (car (split-string remote)) t t))))

(defun gerrit-current-branch ()
  (interactive)
  (let* ((get-possible-branch-command
          "git branch -a | grep \\* | cut -d \" \" -f 2")
         (get-possible-branch-command-v2
          "git branch -a | grep \"\\->\" | cut -d \">\" -f 2")
         (possible-branch (shell-command-to-string get-possible-branch-command))
         (remote (gerrit-get-remote)))
    (when (or (string-empty-p possible-branch)
              (not (string-match-p "/" possible-branch)))
      (setq possible-branch
            (shell-command-to-string get-possible-branch-command-v2))
      (when remote
        (setq possible-branch
              (replace-regexp-in-string (concat remote "/") "" possible-branch))))
    (cu-strip-string possible-branch t t)))

(defun _gerrit_push (&optional draft)
  (let* ((remote (gerrit-get-remote))
         (branch (gerrit-current-branch))
         (command (format "git push %s HEAD:refs/%s/%s"
                          remote (if draft "drafts" "for") branch)))
    (when (y-or-n-p (format "Command: [%s]" command)))
    (shell-command command)))

(defun gerrit-draft () (interactive) (_gerrit_push t))
(defun gerrit-push () (interactive) (_gerrit_push nil))

;; (with-eval-after-load "magit-remote"
;;   (plist-put
;;    magit-push-popup :actions
;;    (let ((lst (plist-get magit-push-popup :actions)))
;;      (dolist (to-add
;;               '((?d "Draft Gerrit Patch" gerrit-draft)
;;                 (?g "Push Gerrit Patch" gerrit-push))
;;               lst)
;;        (setq lst (add-to-list 'lst to-add t))))))

(provide 'init-magit)
