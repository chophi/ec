;;; For start dovecot service directly.
;; (defconst dovecot-program "/usr/local/Cellar/dovecot/2.2.33.2/sbin/dovecot")
;; ;; cp -r /usr/local/Cellar/dovecot/2.2.33.2/share/doc/dovecot/example-config/* \
;; ;; /usr/local/etc/dovecot/
;; (defconst dovecot-config "/usr/local/etc/dovecot/dovecot.conf")
;; (defconst dovecot-output-error-buffer "*dovecot*")

;; (defun start-dovecot ()
;;   (interactive)
;;   (when (eq (shell-command "ps axww | grep dovecot") 0)
;;     (message "dovecot is running"))
;;   (when (or (not (file-executable-p dovecot-program))
;;             (not (file-exists-p dovecot-config)))
;;     (error "please set dovecot-program and dovecot-config correctly!"))
;;   (let ((oe-buffer (get-buffer-create dovecot-output-error-buffer)))
;;     (shell-command (format "%s -c %s" dovecot-program dovecot-config)
;;                    oe-buffer oe-buffer)))

;; use the shell imap program.
(defconst dovecot-imap-shell-program
  "/usr/local/Cellar/dovecot/2.2.33.2/libexec/dovecot/imap"
  "the shell program for gnus imap")
(defconst dovecot-imap-shell-command
  (format "%s -o mail_location=maildir:~/Mail/Fox/:LAYOUT=fs"
          dovecot-imap-shell-program)
  "the imap shell command used for gnus")

(setq gnus-select-method
      `(nnimap "Fox"
               (nnimap-stream shell)
               (nnimap-shell-program ,dovecot-imap-shell-command)
               (nnimap-split-methods nnimap-split-fancy)
               (nnmail-split-methods nnimap-split-fancy)
               (nnimap-split-fancy
                (| (: gnus-registry-split-fancy-with-parent)
                   (: spam-split)
                   (from ,(concat private-mail-username "@foxmail\\.com") "test.myself")
                   (to "linux-fsdevel@vger\\.kernel.org" "dev.fsdevel")
                   (to "linux-ext4@vger\\.kernel.org" "dev.ext4")
                   (to "git@vger\\.kernel\\.org" "dev.git")
                   "INBOX"))))

(provide 'init-gnus-with-dovecot)
