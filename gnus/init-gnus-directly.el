;; Directly use the nnimap from gnus, which is slow.
(setq gnus-select-method
      `(nnimap "MailBox"
               (nnimap-address "imap.qq.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnmail-expiry-wait 90)
               (nnimap-inbox ("INBOX" "Junk"))
               (nnimap-split-methods nnimap-split-fancy)
               (nnmail-split-methods nnimap-split-fancy)
               (nnimap-split-fancy
                (| (: gnus-registry-split-fancy-with-parent)
                   (: spam-split)
                   (from ,(concat private-foxmail-username "@foxmail\\.com") "test.myself")
                   (to "linux-fsdevel@vger\\.kernel.org" "dev.fsdevel")
                   (to "linux-ext4@vger\\.kernel.org" "dev.ext4")
                   (to "git@vger\\.kernel\\.org" "dev.git")
                   "INBOX"))))

;; To ignore the folder "^.*其他文件夹/QQ邮件订阅.*"
(with-eval-after-load "gnus-start"
  (setq gnus-ignored-newsgroups
        (mapconcat
         'identity
         `(,gnus-ignored-newsgroups "^.*/QQ.*") "\\|")))

(provide 'init-gnus-directly)
