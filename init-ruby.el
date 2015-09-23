(add-to-list 'load-path "~/.emacs.d/site-lisp/yari.el")
(require 'yari)

(add-hook 'ruby-mode-hook (lambda () (interactive)
                            (local-set-key [(f9)] 'yari)))
(add-hook 'yari-mode-hook (lambda () (interactive)
                            (local-set-key [(f9)] 'yari)))

(provide 'init-ruby)
