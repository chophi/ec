;; Don't set the notmuch-identities, use the config way.
;; Make sure config the mail username and primary_email in .notmuch-config
;; [user]
;; name=<name>
;; primary_email=<primary_email>
;; # (the below \n is literal, please check notmuch-lib.el)
;; other_email=<other_email1>\n<other_email2>

(require-package 'notmuch)
(require-package 'helm-notmuch)
(require 'notmuch-mua)
(require 'notmuch-lib)

;; QQ have saved a copy to Sent folder, don't fcc it, or it will cause the
;; offlineimap sync issue.
(when (string-match-p ".*@\\(foxmail\\|qq\\).com"
                      (notmuch-user-primary-email))
  (setq notmuch-fcc-dirs nil))

(setq notmuch-always-prompt-for-sender t)
(add-hook 'notmuch-message-mode-hook 'flyspell-mode)
(provide 'init-notmuch)
