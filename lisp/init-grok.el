(require-package 'eopengrok)
(require 'eopengrok)

(defun eopengrok-toggle-global-mode ()
  (interactive)
  (setq eopengrok-global-configuration-mode
        (not eopengrok-global-configuration-mode)))

(defun use-global-configuration-maybe (orig-fun &rest args)
  (if eopengrok-global-configuration-mode
      (expand-file-name eopengrok-global-configuration-file)
    (apply orig-fun args)))
(advice-add 'eopengrok--get-configuration :around #'use-global-configuration-maybe)

(defun eopengrok-toggle-use-clj-or-jar (&optional use-clj)
  (interactive)
  (setq eopengrok-use-clj-opengrok
        (not eopengrok-use-clj-opengrok))
  (let ((path (if eopengrok-use-clj-opengrok
                  "~/.emacs-pkg/opengrok/clj-version"
                "~/.emacs-pkg/opengrok/jar-version")))
    (setq exec-path (remove path exec-path))
    (add-to-list 'exec-path path))
  (setq eopengrok-source-regexp
        (if eopengrok-use-clj-opengrok
            "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)"
          "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\) \\[\\(.*\\)\\]")))

(eopengrok-toggle-use-clj-or-jar)

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

;; (eopengrok--get-configuration)
(provide 'init-grok)
