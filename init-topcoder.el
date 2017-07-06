(add-to-list 'load-path "~/.emacs.d/site-lisp/tc_emacs-0.2/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/gnuserv-3.12.8/")
;; (require 'init-server)
(require-package 'w3m)
(if *linux?*
    (add-to-list 'exec-path "D:/progenv/w3m/"))


;; emacsclient cann't get sources complete without error, so use gnuserv
;; (add-to-list 'exec-path "D:/progenv/topcoder/gnuserv")
;; (load-library "gnuserv")
;; (gnuserv-start)

(require 'topcoder)

(defun my-perfect-side-window ()
  (interactive)
  (split-right-at-83-column nil))

(defvar topcoder-applet-file-path
  (if *windows?*  "D:/progenv/topcoder/ContestAppletProd.jnlp"))

(defun open-topcoder-contest-applet ()
  (interactive)
  (let ((applet-file-path topcoder-applet-file-path))
    (shell-command (format "javaws %s &" applet-file-path))))

(provide 'init-topcoder)
