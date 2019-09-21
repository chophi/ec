(require 'yari)

(with-eval-after-load "ruby-mode"
  (define-key ruby-mode-map (kbd "C-c C-d") 'yari))

(ac-inf-ruby-enable)

(require 'ruby-additional)

(when (file-exists-p "~/.rvm/scripts/rvm")
  (let ((possible-ruby
         (shell-command-to-string
          "[[ -s \"$HOME/.rvm/scripts/rvm\" ]] && \
source \"$HOME/.rvm/scripts/rvm\" && \
echo -n `which ruby`")))
    (when (not (equal possible-ruby ""))
      (setq yari-ruby-program-name possible-ruby)
      (add-to-list 'exec-path (file-name-directory possible-ruby))
      (setenv "PATH" (concat "$PATH:" (file-name-directory possible-ruby)) t)
      )))

(eval-after-load "ruby-mode"
  '(add-hook 'ruby-mode-hook 'ruby-electric-mode))

(provide 'init-ruby)
