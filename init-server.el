(require 'server)


(when *is-windows-system-p*
      (setq server-auth-dir "~/.emacs.d/server"
	    server-name "emacs-server-file")
      (defun server-ensure-safe-dir (dir) "Noop" t))


(when (not (window-system))
  (setq server-auth-dir "~/.emacs.d/server-nw"
	server-name "emacs-server-file"))

;; (when *is-windows-system-p*
;;   (defun server-ensure-safe-dir (dir) "Noop" t))

(server-start)
(provide 'init-server)
