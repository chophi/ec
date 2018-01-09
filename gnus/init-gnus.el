
;; use gnus

(add-to-list 'load-path "~/.emacs.d/gnus")
(require 'init-gnus-private)

(when (not (boundp 'private-foxmail-username))
  (error "You should set private-foxmail-username first!!"))

(setq user-full-name (capitalize private-foxmail-username)
      user-mail-address (concat private-foxmail-username "@foxmail.com"))

(add-to-list 'gnus-secondary-select-methods
             `(nnimap "MailBox"
                      (nnimap-address "imap.qq.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnmail-expiry-wait 90)
                      (nnimap-inbox ("INBOX" "Junk"))
                      (nnimap-split-methods nnimap-split-fancy)
                      (nnimap-split-fancy
                       (| (from ,(concat private-foxmail-username "@foxmail\\.com") "test.myself")
                          (to "linux-fsdevel@vger\\.kernel.org" "dev.fsdevel")
                          (to "linux-ext4@vger\\.kernel.org" "dev.ext4")
                          (to "git@vger\\.kernel\\.org" "dev.git")
                          (: gnus-registry-split-fancy-with-parent)
                          (: spam-split)
                          "INBOX"))))

;; To ignore the folder "^.*其他文件夹/QQ邮件订阅.*"
(with-eval-after-load "gnus-start"
  (setq gnus-ignored-newsgroups
        (mapconcat
         'identity
         `(,gnus-ignored-newsgroups "^.*/QQ.*") "\\|")))

(setq gnus-select-method '(nnml ""))

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;; quit gnus properly instead of leaving auto-save files around
(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
               (gnus-alive-p)
               (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
        (gnus-group-exit)))))


