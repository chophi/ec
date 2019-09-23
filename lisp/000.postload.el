;;; save a list of open files in ~/.emacs.d/.emacs.desktop
(desktop-save-mode 1)
(defadvice desktop-read (around trace-desktop-errors)
  (let ((debug-on-error t))
    ad-do-it))

;;; Restore histories and registers after saving
(add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (comint-input-ring        . 50)
                (shell-command-history    . 50)
                desktop-missing-file-warning
                tags-file-name
                register-alist)))
;;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))
(powerline-default-theme)

;;; Start the server at the end of the init process
(require 'server)
(setq server-socket-dir (format "%s/.emacs.d/server" (getenv "HOME")))
(server-start)

(when (window-system)
  (add-hook 'window-setup-hook 'my-maximize-frame t)
  (add-hook 'window-setup-hook 'my-adjust-font-size-for-current-frame t)
  (add-hook 'after-init-hook
            (lambda () (dolist (f (frame-list))
                         (with-selected-frame f
                           (my-adjust-font-size-for-current-frame))))
            t))
(provide '000.postload)
