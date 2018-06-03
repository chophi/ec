(require 'server)

(when (not server-clients)

  (when (eq os 'windows)
    (setq server-auth-dir "~/.emacs.d/server"
          server-name "emacs-server-file")
    (make-directory server-auth-dir t)
    (defun server-ensure-safe-dir (dir) "Noop" t))

  (when (not (window-system))
    (setq server-auth-dir "~/.emacs.d/server"
          server-socket-dir "~/.emacs.d/server"
          server-name "emacs-server-file"))
  
  (server-start))

(provide 'init-server)
