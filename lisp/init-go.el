(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook 'turn-on-fci-mode)

(let ((p (format "%s/git-repo/go_tour" (getenv "HOME"))))
  (when (file-directory-p p)
    (setenv "GOPATH" p)
    (add-to-path (cu-join-path p "bin"))))

(defconst go-mode-goto-keybindings
  '((?a . go-goto-arguments)
    (?d . go-goto-docstring)
    (?f . go-goto-function)
    (?i . go-goto-imports)
    (?m . go-goto-method-receiver)
    (?n . go-goto-function-name)
    (?r . go-goto-return-values)))

(defconst possible-guru-executable
  (format "%s/git-repo/go_tour/bin/guru" (getenv "HOME")))
(defconst possible-go-flymake-executable
  (format "%s/git-repo/go_tour/bin/goflymake" (getenv "HOME")))
(defun add-go-guru-features ()
  (require 'go-guru)
  (defconst go-mode-go-guru-keybindings
    '((?< . go-guru-callers)
      (?> . go-guru-callees)
      (?c . go-guru-peers)
      (?d . go-guru-describe)
      (?e . go-guru-whicherrs)
      (?f . go-guru-freevars)
      (?i . go-guru-implements)
      (?j . go-guru-definition)
      (?p . go-guru-pointsto)
      (?r . go-guru-referrers)
      (?s . go-guru-callstack)
      (?x . go-guru-expand-region)))
  (add-to-path (file-name-directory possible-guru-executable))
  (cu-set-key-bindings go-mode-map "\C-c\C-o" go-mode-go-guru-keybindings)
  (go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(defun add-go-flymake-features ()
  (require 'go-flymake)
  (add-hook 'go-mode-hook (lambda () (flycheck-mode 1)))
  ;; workaround the "go tool vet -> go vet" problem
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet)))))

(defun go-guru-exists-p ()
  (file-executable-p possible-guru-executable))

(defun go-flymake-exists-p ()
  (file-executable-p possible-go-flymake-executable))

(with-eval-after-load "go-mode"
  (define-key go-mode-map
    "\C-c\C-e"
    '(lambda () (interactive)
       (save-buffer)
       (compile (format "go run %s" (buffer-name)))))
  (cu-set-key-bindings go-mode-map "\C-c\C-f" go-mode-goto-keybindings)
  (when (go-guru-exists-p)
    (add-go-guru-features))
  (when (go-flymake-exists-p)
    (add-go-flymake-features)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq skeleton-pair t)
            (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
            ) t)

(provide 'init-go)
