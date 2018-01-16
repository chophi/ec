(with-parameters-bounded
 '(private-mail-username
   private-mail-address
   private-smtp-server
   private-smtp-port)
 (setq user-full-name (capitalize private-mail-username)
       user-mail-address private-mail-address
       send-mail-function 'smtpmail-send-it
       message-send-mail-function 'smtpmail-send-it
       smtpmail-smtp-server private-smtp-server
       smtpmail-smtp-service private-smtp-port
       smtpmail-stream-type 'ssl))

(provide 'init-send-mail)
