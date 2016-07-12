(require-package 'eopengrok)
(require 'eopengrok)

(setq eopengrok-ctags (concat (getenv "HOME") "/.emacs-pkg/ctags"))
(setq eopengrok-jar (concat (getenv "HOME") "/.emacs-pkg/clj-opengrok-0.3.0-standalone.jar"))

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

(my-set-global-keys "\C-c\C-g" `(,my-opengrok-map))

(provide 'init-grok)
