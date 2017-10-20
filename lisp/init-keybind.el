(global-unset-key (kbd "C-SPC"))

(global-set-key (kbd "C-x vq") 'vr/query-replace)
(global-set-key (kbd "C-x vr") 'vr/replace)
(global-set-key (kbd "\C-xo") 'switch-window)


(add-hook 'emacs-lisp-mode-hook (lambda ()  (define-key emacs-lisp-mode-map "\C-\M-x" nil)))
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map "\C-\M-t" nil)))
(add-hook 'paredit-mode-hook (lambda () (define-key paredit-mode-map "\C-\M-p" nil)))

(defun make-shell-command-key-lambda (command)
  `(lambda () (interactive)
     (if (eq os 'macos)
         (shell-command ,command)
       (ssh-shell-command ,command)))
  )
(defun my-global-set-key (key func)
  (global-unset-key key)
  (global-set-key key func))

;; (my-global-set-key "\C-\M-g" (make-shell-command-key-lambda "open \"/Applications/Google Chrome.app/\""))
;; (my-global-set-key "\C-\M-x" (make-shell-command-key-lambda "open \"/Applications/Xcode.app/\""))
;; (my-global-set-key "\C-\M-t" (make-shell-command-key-lambda "open \"/Applications/iTerm.app/\""))
;; (my-global-set-key "\C-\M-l" (make-shell-command-key-lambda "open \"/Applications/Microsoft Lync.app/\""))
;; (my-global-set-key "\C-\M-o" (make-shell-command-key-lambda "open \"/Applications/Microsoft Outlook.app/\""))
;; (my-global-set-key "\C-\M-p" (make-shell-command-key-lambda "open \"/Applications/Preview.app/\""))

(provide 'init-keybind)
