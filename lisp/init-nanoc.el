(require 'init-private-custom)

(defun nanoc-daemon-public ()
  (interactive)
  (async-shell-command "nanoc-viewer.sh public" "*nanoc public*" "*nanoc public*"))

(defun nanoc-daemon-private ()
  (interactive)
  (async-shell-command "nanoc-viewer.sh private" "*nanoc private*" "*nanoc private*"))

(defun open-nanoc-public-site ()
  (interactive)
  (shell-command "open -a /Applications/Google\\ Chrome.app/ http://127.0.0.1:3000/"))

(defun open-nanoc-private-site ()
  (interactive)
  (shell-command "open -a /Applications/Google\\ Chrome.app/ http://127.0.0.1:3001/"))

(provide 'init-nanoc)
