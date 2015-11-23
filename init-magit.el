(require-package 'magit)

(global-set-key "\C-xg" 'magit-status)

(when *is-mac-machine*
  (setq magit-git-executable "/opt/local/bin/git"))

(provide 'init-magit)
