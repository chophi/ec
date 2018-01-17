(with-parameters-bounded
 '(private-mail-username
   private-mail-address
   private-smtp-server
   private-smtp-port)
 (setq user-full-name (capitalize private-mail-username)
       user-mail-address private-mail-address
       smtpmail-smtp-server private-smtp-server
       smtpmail-smtp-service private-smtp-port))

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      message-kill-buffer-on-exit t
      message-confirm-send t)

(provide 'init-send-mail)
