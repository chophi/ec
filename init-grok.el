(require-package 'eopengrok)
(require 'eopengrok)

(setq eopengrok-ctags (concat (getenv "HOME") "/.emacs-pkg/ctags"))
(setq eopengrok-jar (concat (getenv "HOME") "/.emacs-pkg/clj-opengrok-0.3.0-standalone.jar"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.emacs-pkg"))

(defconst my-opengrok-map
  '(;;(?i . eopengrok-make-index)
    ;;(?I . eopengrok-make-index-with-enable-projects)
    (?d . eopengrok-find-definition)
    (?f . eopengrok-find-file)
    (?s . eopengrok-find-reference)
    (?t . eopengrok-find-text)
    (?h . eopengrok-find-history)
    (?r . eopengrok-resume)
    (?p . print-eopengrok-map-help)))

(define-key eopengrok-mode-map "o" 'eopengrok-jump-to-source)
;; following is the link explains the differences between "RET" and [(return)]
;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
(define-key eopengrok-mode-map (kbd "RET") 'eopengrok-jump-to-source)

(my-set-global-keys "\C-c\C-g" `(,my-opengrok-map))

(provide 'init-grok)
