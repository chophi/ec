(require 'server)
(setq server-socket-dir (format "%s/.emacs.d/server" (getenv "HOME")))
(server-start)
(provide 'init-server)
