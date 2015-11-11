(require 'init-multi-term)
(require 'init-term-keys)

(require 'init-term-face)

(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)
            (define-key term-raw-map "\C-y" 'term-send-raw)))

(require 'init-create-multi-terms)

(modify-syntax-entry ?_ "w" term-mode-syntax-table)
(modify-syntax-entry ?- "w" term-mode-syntax-table)
(modify-syntax-entry ?. "w" term-mode-syntax-table)
(modify-syntax-entry ?/ "w" term-mode-syntax-table)

(provide 'term-inside-ide-init)
