(require-package 'eopengrok)
(require 'eopengrok)

(defconst global-eopengrok-configuration
  nil
  "Global eopengrok configuration file")

(defun use-opengrok-the-old-way ()
  (interactive)
  (let* (home (getenv "HOME"))
    (setq global-eopengrok-configuration nil)
    (setq eopengrok-ctags (cu-join-path home "/.emacs-pkg/ctags")
          eopengrok-jar (cu-join-path home "/.emacs-pkg/bin/opengrok.jar"))
    (add-to-list 'exec-path (cu-join-path home "/.emacs-pkg"))
    (add-to-path (cu-join-path home "/.emacs-pkg") t)))

(defun use-opengrok-the-new-way ()
  (interactive)
  (let* (home (getenv "HOME"))
    (setq global-eopengrok-configuration "~/opengrok/data/configuration.xml")
    (setq eopengrok-ctags "/usr/local/bin/ctags"
          eopengrok-jar (cu-join-path home "opengrok/packages/opengrok-1.1-rc18/lib/opengrok.jar"))
    (add-to-list 'exec-path (cu-join-path home "/usr/local/bin"))
    (add-to-path (cu-join-path home "/usr/local/bin") t)))

(use-opengrok-the-old-way)

(define-key eopengrok-mode-map "o" 'eopengrok-jump-to-source)
;; following is the link explains the differences between "RET" and [(return)]
;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
(define-key eopengrok-mode-map (kbd "RET") 'eopengrok-jump-to-source)

(defun --read-ignore-file (dir)
  (let ((ignore-file (concat dir ".opengrok_ignore")))
    (if (file-exists-p ignore-file)
        (replace-regexp-in-string
         "\n" ":"
         (shell-command-to-string (format "cat %s" ignore-file)))
      "")))

(defadvice eopengrok-create-index (around add-ignore-ad)
  (message "Dir is %s" dir)
  (let ((eopengrok-ignore-file-or-directory
         (concat eopengrok-ignore-file-or-directory (--read-ignore-file dir))))
    ad-do-it))
(ad-activate 'eopengrok-create-index)

(setq eopengrok-ignore-file-or-directory
      (concat eopengrok-ignore-file-or-directory ":.scripts:.log"))

(defun use-global-configuration-maybe (orig-fun &rest args)
  (if (stringp global-eopengrok-configuration)
      (expand-file-name global-eopengrok-configuration)
      (apply orig-fun args)))
(advice-add 'eopengrok--get-configuration :around #'use-global-configuration-maybe)

;; (eopengrok--get-configuration)
(provide 'init-grok)
