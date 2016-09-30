(require-package 'yari)
(require 'yari)

(add-hook 'ruby-mode-hook (lambda () (interactive)
                            (local-set-key [(f9)] 'yari)))
(add-hook 'yari-mode-hook (lambda () (interactive)
                            (local-set-key [(f9)] 'yari)))

(require-package 'ac-inf-ruby)
(ac-inf-ruby-enable)

(require-package 'rinari)
(require-package 'ruby-additional)
(require 'ruby-additional)

(when (file-exists-p "~/.rvm/scripts/rvm")
  (let ((possible-ruby
         (shell-command-to-string
          "[[ -s \"$HOME/.rvm/scripts/rvm\" ]] && source \"$HOME/.rvm/scripts/rvm\" && echo -n `which ruby`")))
    (when (not (equal possible-ruby ""))
      (setq yari-ruby-program-name possible-ruby)
      (add-to-list 'exec-path (file-name-directory possible-ruby))
      (setenv "PATH" (concat "$PATH:" (file-name-directory possible-ruby)) t)
      )))

(require-package 'ruby-electric)
(eval-after-load "ruby-mode"
  '(add-hook 'ruby-mode-hook 'ruby-electric-mode))

(require-package 'rsense)

(provide 'init-ruby)
