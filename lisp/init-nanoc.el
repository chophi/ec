(defun nanoc-daemon-public ()
  (interactive)
  (cu-start-process "nanoc-public-daemon" "*nanoc-public*" "nanoc-viewer.sh" "public")
  (switch-to-buffer-other-window "*nanoc-public*"))

(defun nanoc-daemon-private ()
  (interactive)
  (cu-start-process "nanoc-private-daemon" "*nanoc-private*" "nanoc-viewer.sh" "private")
  (switch-to-buffer-other-window "*nanoc-private*"))

(defun open-nanoc-public-site ()
  (interactive)
  (shell-command "open -a /Applications/Google\\ Chrome.app/ http://127.0.0.1:3000/"))

(defun open-nanoc-private-site ()
  (interactive)
  (shell-command "open -a /Applications/Google\\ Chrome.app/ http://127.0.0.1:3001/"))

(provide 'init-nanoc)
