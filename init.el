;; Don't remove the below line as emacs want to see it as the first line even
;; it's commented out.
;; (package-initialize)

(defun reload-config ()
  (interactive)
  (load-file user-init-file))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(when (file-exists-p "~/repo/amazon/tools/EmacsAmazonLibs/lisp")
  (add-to-list 'load-path "~/repo/amazon/tools/EmacsAmazonLibs/lisp"))

(setq debug-on-error t)
(require 'init-emacs)
(setq debug-on-error nil)
