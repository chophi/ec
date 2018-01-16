
;; use gnus

(add-to-list 'load-path "~/.emacs.d/gnus")
(require 'init-gnus-private)
;; (require 'init-gnus-imap-directly)
(when (not (company-computer-p))
  (require 'init-gnus-with-dovecot))

;; quit gnus properly instead of leaving auto-save files around
(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
               (gnus-alive-p)
               (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
        (gnus-group-exit)))))


