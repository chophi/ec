(require-package 'notmuch)
(require-package 'helm-notmuch)
(require 'notmuch-mua)

;; Don't set the notmuch-identities, use the config way.
;; Make sure config the mail username and primary_email in .notmuch-config
;; [user]
;; name=<name>
;; primary_email=<primary_email>
;; # (the below \n is literal, please check notmuch-lib.el)
;; other_email=<other_email1>\n<other_email2>
(setq notmuch-always-prompt-for-sender t)
(add-hook 'notmuch-message-mode-hook 'flyspell-mode)
(provide 'init-notmuch)
