(global-unset-key (kbd "C-SPC"))

(global-set-key (kbd "C-x vq") 'vr/query-replace)
(global-set-key (kbd "C-x vr") 'vr/replace)

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-x w")
                (lambda ()
                  (interactive)
                  (when (one-window-p)
                    (split-window-horizontally-instead))
                  (switch-window)))

(global-unset-key (kbd "C-x C-v"))

(defun my-magit-log-head-fast()
  (interactive)
  (let ((magit-log-arguments (remove "--graph"
                                     (remove "--decorate" magit-log-arguments))))
    (magit-log-head)))
(cu-set-key-bindings global-map "\C-c\C-v" '((?s . magit-status)
                                             (?b . magit-blame)
                                             (?B . magit-blame-mode)
                                             (?p . magit-pull)
                                             (?l . magit-log-head)
                                             (?L . my-magit-log-head-fast)))



(add-hook 'emacs-lisp-mode-hook
          (lambda ()  (define-key emacs-lisp-mode-map "\C-\M-x" nil)))
(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map "\C-\M-t" nil)))
(add-hook 'paredit-mode-hook
          (lambda () (define-key paredit-mode-map "\C-\M-p" nil)))

(defun make-shell-command-key-lambda (command)
  `(lambda () (interactive)
     (if (eq os 'macos)
         (shell-command ,command)
       (ssh-shell-command ,command))))

(when (fboundp 'control-x-f)
  (global-set-key (kbd "C-x f") 'control-x-f))

(provide 'init-keybind)
