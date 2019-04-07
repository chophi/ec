(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook 'turn-on-fci-mode)

(defconst go-mode-goto-keybindings
  '((?a . go-goto-arguments)
    (?d . go-goto-docstring)
    (?f . go-goto-function)
    (?i . go-goto-imports)
    (?m . go-goto-method-receiver)
    (?n . go-goto-function-name)
    (?r . go-goto-return-values)))

(with-eval-after-load "go-mode"
  (define-key go-mode-map
    "\C-c\C-e"
    '(lambda () (interactive)
       (save-buffer)
       (compile (format "go run %s" (buffer-name)))))
  (cu-set-key-bindings go-mode-map "\C-c\C-f" go-mode-goto-keybindings))

(add-hook 'go-common-hook
          (lambda ()
            (setq skeleton-pair t)
            (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
            ))
(provide 'init-go)
