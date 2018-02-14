(require 'init-private-custom)

(defconst nanoc-home-dir
  (if (boundp 'private-nanoc-home-dir)
      private-nanoc-home-dir
    (expand-file-name "~/nanoc-site")))

(defun nanoc-run-internal (args buf async)
  (interactive)
  (when (and async (get-buffer-process buf))
    (error "There's already a running process with buffer %s" buf))
  (shell-command
   (format "cd %s && nanoc %s %s"
           nanoc-home-dir args
           (if async "&" ""))
   buf buf))

(defun nanoc-daemon ()
  (interactive)
  (nanoc-run-internal "view" "*nanoc-view*" t))

(defun nanoc-update ()
  (interactive)
  (nanoc-run-internal "" "*nanoc-recompile*" t))

(defun open-nanoc-site ()
  (interactive)
  (shell-command "open -a /Applications/Google\\ Chrome.app/ http://127.0.0.1:3000/"))
(provide 'init-nanoc)
