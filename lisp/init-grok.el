(require-package 'eopengrok)
(require 'eopengrok)

(define-key eopengrok-mode-map "o" 'eopengrok-jump-to-source)

;; following is the link explains the differences between "RET" and [(return)]
;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
(define-key eopengrok-mode-map (kbd "RET") 'eopengrok-jump-to-source)

(defadvice eopengrok-create-index (around add-ignore-ad)
  (let* ((ignore-list (cu-read-ignore-list dir ".opengrok_ignore"))
         (eopengrok-ignore-file-or-directory
          (concat eopengrok-ignore-file-or-directory (string-join ignore-list)))
         (eopengrok-ignore-list
          (append eopengrok-ignore-list ignore-list)))
    ;; (print ignore-list)
    ;; (print eopengrok-ignore-list)
    ad-do-it))
(ad-activate 'eopengrok-create-index)

(when (not (file-exists-p eopengrok-database-root-dir))
  (make-directory eopengrok-database-root-dir))

(provide 'init-grok)
