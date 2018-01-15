
;; use gnus

(add-to-list 'load-path "~/.emacs.d/gnus")
(require 'init-gnus-private)
;; (require 'init-gnus-imap-directly)
(when (not (company-computer-p))
  (require 'init-gnus-with-dovecot))

(when (not (boundp 'private-foxmail-username))
  (error "You should set private-foxmail-username first!!"))

(setq user-full-name (capitalize private-foxmail-username)
      user-mail-address (concat private-foxmail-username "@foxmail.com"))

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


