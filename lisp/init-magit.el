(require-package 'dash)
(require-package 'magit)

;; using magit to manage git
;; (delete 'Git vc-handled-backends)
;; not use SVN
;; (delete 'SVN vc-handled-backends)
(setq vc-handled-backends nil)

(global-set-key "\C-xg" 'magit-status)

;; (when (eq os 'macos)
;;   (setq magit-git-executable "/opt/local/bin/git"))

;; (setq magit-last-seen-setup-instructions "1.4.0")

(setq-default git-commit-fill-column 70)
(provide 'init-magit)
