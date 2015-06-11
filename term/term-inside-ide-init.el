(require 'init-multi-term)
(require 'init-term-keys)

(require 'init-term-face)

(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)))

(provide 'term-inside-ide-init)



