(require-package 'eopengrok)
(require 'eopengrok)

(setq eopengrok-ctags (concat (getenv "HOME") "/.emacs-pkg/ctags"))
(setq eopengrok-jar (concat (getenv "HOME") "/.emacs-pkg/bin/opengrok.jar"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.emacs-pkg"))
(add-to-path (concat (getenv "HOME") "/.emacs-pkg") t)

(defconst my-opengrok-map
  '((?d . eopengrok-find-definition)
    (?f . eopengrok-find-file)
    (?s . eopengrok-find-reference)
    (?t . eopengrok-find-text)
    (?h . eopengrok-find-history)
    (?r . eopengrok-resume)
    (?c . eopengrok-make-index)))

(define-key eopengrok-mode-map "o" 'eopengrok-jump-to-source)
;; following is the link explains the differences between "RET" and [(return)]
;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
(define-key eopengrok-mode-map (kbd "RET") 'eopengrok-jump-to-source)

(cu-set-key-bindings global-map "\C-c\C-g" `(,my-opengrok-map))

(defun --read-ignore-file (dir)
  (let ((ignore-file (concat dir ".opengrok_ignore")))
    (if (file-exists-p ignore-file)
        (replace-regexp-in-string "\n" ":" (shell-command-to-string (format "cat %s" ignore-file)))
      "")))

(defadvice eopengrok-create-index (around add-ignore-ad)
  (message "Dir is %s" dir)
  (let ((eopengrok-ignore-file-or-directory
         (concat eopengrok-ignore-file-or-directory (--read-ignore-file dir))))
    ad-do-it))
(ad-activate 'eopengrok-create-index)

(setq eopengrok-ignore-file-or-directory
      (concat eopengrok-ignore-file-or-directory ":.scripts:.log"))
(provide 'init-grok)
