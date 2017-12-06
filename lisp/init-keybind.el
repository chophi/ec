(global-unset-key (kbd "C-SPC"))

(global-set-key (kbd "C-x vq") 'vr/query-replace)
(global-set-key (kbd "C-x vr") 'vr/replace)
(global-set-key (kbd "\C-xw")
                (lambda ()
                  (interactive)
                  (when (one-window-p)
                    (split-window-horizontally-instead))
                  (switch-window)))


(add-hook 'emacs-lisp-mode-hook (lambda ()  (define-key emacs-lisp-mode-map "\C-\M-x" nil)))
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map "\C-\M-t" nil)))
(add-hook 'paredit-mode-hook (lambda () (define-key paredit-mode-map "\C-\M-p" nil)))

(defun make-shell-command-key-lambda (command)
  `(lambda () (interactive)
     (if (eq os 'macos)
         (shell-command ,command)
       (ssh-shell-command ,command))))

(when (fboundp 'control-x-f)
  (global-set-key (kbd "C-x f") 'control-x-f))

(provide 'init-keybind)
