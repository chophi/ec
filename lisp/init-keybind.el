(global-unset-key (kbd "C-SPC"))

(global-set-key (kbd "C-x vq") 'vr/query-replace)
(global-set-key (kbd "C-x vr") 'vr/replace)

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-x w")
                (lambda ()
                  (interactive)
                  (when (one-window-p)
                    (split-window-horizontally-instead))
                  (switch-window)))

(global-unset-key (kbd "C-x C-v"))


;; Binding keys with cu-set-key-bindings

;; work with repo
(with-eval-after-load "org" (define-key org-mode-map "\C-c\C-r" nil))
(cu-set-key-bindings global-map "\C-c\C-r"
                     '((?c . change-repo-ws)
                       (?g . repo-goto-project)))

;; magit 
(defun my-magit-log-head-fast()
  (interactive)
  (let ((magit-log-arguments
         (remove "--graph" (remove "--decorate" magit-log-arguments))))
    (magit-log-head)))
(cu-set-key-bindings global-map "\C-c\C-v" '((?s . magit-status)
                                             (?b . magit-blame)
                                             (?B . magit-blame-mode)
                                             (?p . magit-pull)
                                             (?l . magit-log-head)
                                             (?L . my-magit-log-head-fast)))
;; confluence wiki
(when (company-computer-p)
  (defconst publish-org-to-confluence-wiki-keymap
    '(("d" . org-update-to-draft-page)
      ("w" . org-update-related-wiki-page)
      ("r" . org-read-related-wiki-page)
      ("v" . org-export-buffer-to-wiki-and-view)))
  (cu-set-key-bindings global-map "\C-c\C-p" publish-org-to-confluence-wiki-keymap)
  (cu-set-key-bindings org-mode-map "\C-c\C-p" publish-org-to-confluence-wiki-keymap))

;; google translate
(cu-set-key-bindings global-map "\C-c\C-t"
                     `((?t . google-translate-at-point)
                       (?r . google-translate-at-point-reverse)))

;; path utils
(with-eval-after-load "cc-mode" (define-key c-mode-base-map "\C-c\C-l" nil))
(with-eval-after-load "sh-script" (define-key sh-mode-map "\C-c\C-l" nil))
(with-eval-after-load "java-mode" (define-key java-mode-map "\C-c\C-l" nil))
(defconst cu-path-util-map
  '((?i . cu-insert-path-replace-home)
    (?I . cu-insert-path-absolute-home)
    (?s . cu-save-current-file-path)
    (?o . cu-save-current-file-path-org-style)
    (?j . cu-open-link)
    (?f . cu-visit-file-follow-symlink))
  "Util key map for path saving to ring / paste, etc")
(cu-set-key-bindings global-map "\C-c\C-l" cu-path-util-map)
(with-eval-after-load "org"
  (cu-set-key-bindings org-mode-map "\C-c\C-l"
                       `(,cu-path-util-map ((?l . org-insert-link)))))

;; yasnippet keymap and spell-check
(defvar ctrl-c-ctrl-i-keymap
  `((?i . yas-insert-snippet)
    (?n . yas-new-snippet)
    (?t . yas-tryout-snippet)
    (?v . yas-visit-snippet-file)
    (?r . yas-reload-all))
  "keymap for ctrl-c-ctrl-i")

(when (and (boundp 'my-ispell-is-enabled) my-ispell-is-enabled)
  (defconst my-ispell-keymap
    `((?w . ispell-word)
      (?b . ispell-buffer)
      (?f . flyspell-mode)
      (?c . flyspell-auto-correct-word))
    "The keymap for ispell")
  (setq ctrl-c-ctrl-i-keymap (append ctrl-c-ctrl-i-keymap my-ispell-keymap))
  (cu-set-key-bindings global-map "\C-c\C-i" ctrl-c-ctrl-i-keymap)
  (with-eval-after-load "make-mode"
    (cu-set-key-bindings makefile-mode-map "\C-c\C-i"
                         `(,ctrl-c-ctrl-i-keymap
                           ((?F . makefile-insert-gmake-function))))))

;; uniform environment
(cu-set-key-bindings global-map "\C-c\C-f"
                     '((?f . ue-env-find-file)
                       (?i . ue-insert-to-env-list)))

;; from init-cc-misc-support
(defconst eassist-key-bindings
  '(("g" . eassist-switch-h-cpp-try-replace)
    ("l" . eassist-list-methods))
  ;;("r" . semantic-symref))
  "Key bindings for eassist")
(defconst semantic-key-bindings
  '((?i . semantic-ia-fast-jump)
    (?p . semantic-analyze-proto-impl-toggle)
    (?b . semantic-mrub-switch-tags)
    (?s . semantic-ia-show-summary)
    ;;(?d . semantic-ia-show-doc)
    ;; tag folding
    (?f . semantic-tag-folding-fold-block)
    (?o . semantic-tag-folding-show-block)
    (?- . semantic-tag-folding-fold-all)
    (?+ . semantic-tag-folding-show-all)
    ;; complete
    (?m . semantic-ia-complete-symbol-menu)
    (?c . semantic-ia-complete-symbol)
    (?t . semantic-ia-complete-tip))
  "Key bindings for semantic")
(with-eval-after-load "cc-mode"
  (cu-set-key-bindings c-mode-base-map
                       "\C-c\C-s" `(,semantic-key-bindings ,eassist-key-bindings)))

(defun my-opengrok-create-index ()
  (interactive)
  (if eopengrok-global-configuration-mode
      (eopengrok-create-index eopengrok-global-source-dir nil)
    (call-interactively 'eopengrok-create-index)))

;; grok keybindings from init-grok.el
(defconst my-opengrok-map
  '((?d . eopengrok-find-definition)
    (?f . eopengrok-find-file)
    (?s . eopengrok-find-reference)
    (?t . eopengrok-find-text)
    (?h . eopengrok-find-history)
    (?r . eopengrok-resume)
    (?c . my-opengrok-create-index)
    (?m . eopengrok-toggle-global-mode)))

(cu-set-key-bindings
 global-map "\C-c\C-g" `(,my-opengrok-map)
 '(("global configuration mode" . eopengrok-global-configuration-mode)))

;; neo-tree
(cu-set-key-bindings global-map "\C-c\C-n" '((?t . neotree-toggle)))

;; ido gnus
(cu-set-key-bindings global-map "\C-c\C-m"
                     '((?g . ido-gnus-select-group)
                       (?s . ido-gnus-select-server)
                       (?e . ido-gnus-select)))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()  (define-key emacs-lisp-mode-map "\C-\M-x" nil)))
(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map "\C-\M-t" nil)))
(add-hook 'paredit-mode-hook
          (lambda () (define-key paredit-mode-map "\C-\M-p" nil)))

(defun make-shell-command-key-lambda (command)
  `(lambda () (interactive)
     (if (eq os 'macos)
         (shell-command ,command)
       (ssh-shell-command ,command))))

(when (fboundp 'control-x-f)
  (global-set-key (kbd "C-x f") 'control-x-f))

(provide 'init-keybind)
