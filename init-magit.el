(require-package 'dash)
(require-package 'magit)

(global-set-key "\C-xg" 'magit-status)

;; (when *is-mac-machine*
;;   (setq magit-git-executable "/opt/local/bin/git"))

;; (setq magit-last-seen-setup-instructions "1.4.0")

(provide 'init-magit)
