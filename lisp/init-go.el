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
(defconst possible-go-gocode-executable
  (format "%s/git-repo/go_tour/bin/gocode" (getenv "HOME")))

(defun go-guru-exists-p ()
  (file-executable-p possible-guru-executable))

(defun go-gocode-exists-p ()
  (file-executable-p possible-go-gocode-executable))

(defun add-go-guru-features ()
  (require 'go-guru)
  (defconst go-mode-go-guru-keybindings
    '((?i . go-guru-implements)
      (?j . godef-jump)
      (?J . go-guru-definition)
      (?d . godef-describe)
      (?D . go-guru-describe)
      (?< . go-guru-callers)
      (?> . go-guru-callees)
      (?c . go-guru-peers)
      (?e . go-guru-whicherrs)
      (?f . go-guru-freevars)
      (?p . go-guru-pointsto)
      (?r . go-guru-referrers)
      (?s . go-guru-callstack)
      (?x . go-guru-expand-region)))
  (add-to-path (file-name-directory possible-guru-executable))
  (cu-set-key-bindings go-mode-map "\C-c\C-s" go-mode-go-guru-keybindings)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(defun add-go-flymake-features ()
  ;; workaround the "go tool vet -> go vet" problem
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet)))))

(defun add-go-gocode-features ()
  (require 'go-autocomplete))

(with-eval-after-load "go-mode"
  (define-key go-mode-map
    "\C-c\C-e"
    '(lambda () (interactive)
       (save-buffer)
       (compile (format "go run %s" (buffer-name)))))
  (cu-set-key-bindings go-mode-map "\C-c\C-f" go-mode-goto-keybindings)
  (cu-set-key-bindings go-mode-map "\C-ce" '((?e . cu-cycle-flycheck-error)
                                             (?c . flycheck-compile)))
  (when (go-guru-exists-p)
    (add-go-guru-features))
  (add-go-flymake-features)
  (when (go-gocode-exists-p)
    (add-go-gocode-features)))

(add-hook 'go-mode-hook 'cu-set-skeleton-pair-indent t)

(provide 'init-go)
