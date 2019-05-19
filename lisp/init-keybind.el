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

(setq-default magit-blame-echo-style 'headlines)
(defun my-toggle-magit-blame-mode ()
  (interactive)
  (if (and (boundp 'magit-blame-mode) magit-blame-mode)
      (call-interactively 'magit-blame-mode)
    (call-interactively 'magit-blame-echo)
    (magit-blame-read-only-mode 1)))

(defun my-select-magit-style (&optional full)
  (interactive)
  (let ((choice (if full
                    (mapcar (lambda (x) (symbol-name (car x))) magit-blame-styles)
                  '("margin" "headlines"))))
    (setq-local magit-blame-echo-style
                (intern (ido-completing-read "Choose Style: " choice)))))

(defconst my-magit-key-map
  '((?s . magit-status)
    (?b . my-toggle-magit-blame-mode)
    (?B . my-select-magit-style)
    (?p . magit-pull)
    (?l . magit-log-head)
    (?L . my-magit-log-head-fast))
  "my keymap for magit")

(cu-set-key-bindings global-map "\C-c\C-v" my-magit-key-map
                     '(("Magit Style" . magit-blame-echo-style)))
(with-eval-after-load "sgml-mode"
  (define-key html-mode-map "\C-c\C-v" nil))
(with-eval-after-load "python"
  (cu-set-key-bindings python-mode-map "\C-c\C-v"
                       `(,my-magit-key-map
                         ((?c . python-check)))))

;; Remapping org-babel-key-prefix to \C-cv for magit key bindings.
(with-eval-after-load "org"
  (define-key org-mode-map "\C-cb" org-babel-map)
  (cu-set-key-bindings org-mode-map "\C-c\C-v" my-magit-key-map))

;; confluence wiki
(when (company-computer-p)
  (defconst publish-org-to-confluence-wiki-keymap
    `(("w" . org-update-related-wiki-page)
      ("d" . org-update-to-draft-page)
      ("r" . org-read-related-wiki-page)
      ("v" . org-export-buffer-to-wiki-and-view)
      ,@(when (fboundp 'confluence-view-the-wiki)
          (defun org-confluence-view-the-wiki ()
            (interactive)
            (confluence-view-the-wiki (or (org-get-wiki-page-id)
                                          (error "wiki page id is empty"))))
          '(("o" . org-confluence-view-the-wiki)))))
  (cu-set-key-bindings global-map "\C-c\C-p" publish-org-to-confluence-wiki-keymap)
  (cu-set-key-bindings org-mode-map "\C-c\C-p" publish-org-to-confluence-wiki-keymap))

;; google translate
(dolist (map (list org-mode-map global-map))
  (cu-set-key-bindings map "\C-c\C-t"
                       `((?e . google-translate-at-point-to-english)
                         (?c . google-translate-at-point-to-chinese)
                         (?E . google-translate-query-to-english)
                         (?C . google-translate-query-to-chinese)
                         ,@(when (eq map org-mode-map)
                             '((?t . org-todo))))))

;; path utils
(with-eval-after-load "cc-mode"
  (define-key c-mode-base-map "\C-c\C-l" nil)
  (define-key java-mode-map "\C-c\C-l" nil))
(with-eval-after-load "sh-script" (define-key sh-mode-map "\C-c\C-l" nil))
(defconst cu-path-util-map
  '((?i . cu-insert-path-replace-home)
    (?I . cu-insert-path-absolute-home)
    (?s . cu-save-current-file-path)
    (?o . cu-save-current-file-path-org-style)
    (?O . org-store-link)
    (?j . cu-open-link)
    (?f . cu-visit-file-follow-symlink)
    (?n . find-name-dired)
    (?L . lc-open-next-problem)
    (?T . lc-test-current-buffer)
    (?J . lc-judge-current-buffer)
    (?C . lc-clear-cache))
  "Util key map for path saving to ring / paste, etc")
(cu-set-key-bindings global-map "\C-c\C-l" cu-path-util-map)
(with-eval-after-load "python"
  (cu-set-key-bindings python-mode-map "\C-c\C-l"
                       `(,cu-path-util-map ((?S . python-shell-send-file)))))
(with-eval-after-load "org"
  (cu-set-key-bindings org-mode-map "\C-c\C-l"
                       `(,cu-path-util-map ((?l . org-insert-link)))))

;; yasnippet keymap and spell-check
(defvar spell-and-snippet-key-binding
  `((?i . yas-insert-snippet)
    (?n . yas-new-snippet)
    (?t . yas-tryout-snippet)
    (?v . yas-visit-snippet-file)
    (?r . yas-reload-all))
  "keymap for ctrl-c-ctrl-i")

(when (and (boundp 'my-ispell-is-enabled) my-ispell-is-enabled)
  (append spell-and-snippet-key-binding
    `((?w . ispell-word)
      (?b . ispell-buffer)
      (?f . flyspell-mode)
      (?c . flyspell-auto-correct-word)))
  (cu-set-key-bindings global-map "\C-cs" spell-and-snippet-key-binding))

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
    (?S . semantic-ia-show-summary)
    (?s . semantic-save-bookmark-to-mru-ring)
    ;;(?d . semantic-ia-show-doc)
    ;; tag folding
    (?f . semantic-tag-folding-fold-block)
    (?o . semantic-tag-folding-show-block)
    (?- . semantic-tag-folding-fold-all)
    (?+ . semantic-tag-folding-show-all)
    ;; complete
    (?m . semantic-ia-complete-symbol-menu)
    (?c . semantic-ia-complete-symbol)
    (?t . semantic-ia-complete-tip)
    (?e . cu-toggle-lsp))
  "Key bindings for semantic")

(with-eval-after-load "cc-mode"
  (cu-set-key-bindings c-mode-base-map
                       "\C-c\C-s" `(,semantic-key-bindings ,eassist-key-bindings)))

;; grok keybindings from init-grok.el
(defconst my-opengrok-map
  '((?d . eopengrok-find-definition)
    (?f . eopengrok-find-file)
    (?s . eopengrok-find-reference)
    (?t . eopengrok-find-text)
    (?h . eopengrok-find-history)
    (?r . eopengrok-resume)
    (?c . eopengrok-create-index)
    (?v . eopengrok-visit-project-root)
    (?p . eopengrok-narrow-to-project)
    (?P . eopengrok-toggle-narrow-to-current-project)
    (?l . eopengrok-list-projects)
    (?S . eopengrok-toggle-swap-mode)
    (?D . thread-grok-index-main)
    (?K . kill-grok-indexing-buffer)
    (?n . eopengrok-use-newer-index-file)))

(cu-set-key-bindings
 global-map "\C-c\C-g" `(,my-opengrok-map)
 '(("narrowed project" . (eopengrok-get-current-narrowed-project))
   ("narrow to current project" . eopengrok-search-current-project)
   ("swap mode" . eopengrok-swap-mode)))



(add-hook 'emacs-lisp-mode-hook
          (lambda ()  (define-key emacs-lisp-mode-map "\C-\M-x" nil)))
(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map "\C-\M-t" nil)))
(add-hook 'paredit-mode-hook
          (lambda () (define-key paredit-mode-map "\C-\M-p" nil)))

;; grep
(cu-set-key-bindings global-map "\C-cg"
                     '((?a . ag)
                       (?h . helm-do-grep-ag)
                       (?g . helm-grep-do-git-grep)
                       (?m . list-matching-lines)
                       (?f . find-name-dired)))

;; ;; ido gnus deprecated
;; (cu-set-key-bindings global-map "\C-c\C-m"
;;                      '((?g . ido-gnus-select-group)
;;                        (?s . ido-gnus-select-server)
;;                        (?e . ido-gnus-select)))
;; mail
(cu-set-key-bindings global-map "\C-c\C-m"
                     '((?m . helm-notmuch)
                       (?n . notmuch-mua-new-mail)
                       (?j . notmuch-jump-search)))

(global-set-key "\C-cc" 'cp-custom-compile)

(defun my-compilation-shell-minor-mode ()
  (interactive)
  (let ((keymap '((?n . compilation-next-error)
                  (?p . compilation-previous-error)
                  (?g . compile-goto-error)
                  (?f . compilation-next-file)
                  (?F . compilation-previous-file))))
    (if compilation-shell-minor-mode
        (progn
          (message "Disable compilation-shell-minor-mode")
          (compilation-shell-minor-mode -1)
          (dolist (key keymap)
            (define-key term-raw-map (char-to-string (car key)) 'term-send-raw)))
      (message "Enable compilation-shell-minor-mode")
      (compilation-shell-minor-mode 1)
      (dolist (key keymap)
        (define-key term-raw-map (char-to-string (car key)) (cdr key))))))

(defun graphviz-set-extension()
  (interactive)
  (setq graphviz-dot-preview-extension
        (ido-completing-read "Extension: " '("svg" "png" "jpeg"))))

(with-eval-after-load "graphviz-dot-mode"
  (define-key graphviz-dot-mode-map
    (kbd "<tab>")
    'graphviz-indent-or-complet-word))

(with-eval-after-load "python"
  (cu-set-key-bindings python-mode-map
                       "\C-c\C-s"
                       '((?s . python-shell-send-string)
                         (?d . jedi:goto-definition)
                         (?i . jedi:show-doc)
                         (?c . jedi:complete)
                         (?n . jedi:goto-definition-next))))

(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(cu-set-key-bindings projectile-mode-map "\C-c\C-f"
                     '((?f . projectile-find-file)
                       (?r . projectile-recentf)
                       (?w . projectile-find-file-dwim)
                       (?d . projectile-find-dir)
                       (?t . projectile-find-test-file)
                       (?o . projectile-find-other-file)
                       (?g . projectile-find-tag)
                       (?i . projectile-find-file-in-directory)
                       (?n . projectile-find-file-in-known-projects)))

(cu-set-key-bindings
 projectile-mode-map "\C-cf"
 `((?v . projectile-vc)
   (?r . projectile-recentf)
   (?s . projectile-replace)
   (?d . projectile-dired)
   (?g . projectile-multi-occur)
   (?o . projectile-project-buffers-other-buffer)
   (?i . projectile-ibuffer)
   (?b . projectile-switch-to-buffer)
   (?k . projectile-kill-buffers)
   (?m . projectile-commander)
   (?p . projectile-switch-project)
   (?q . projectile-switch-open-project)
   (?! . projectile-run-shell-command-in-root)
   (?& . projectile-run-async-shell-command-in-root)
   (?c . projectile-compile-project)
   (?C . projectile-configure-project)
   (?E . projectile-edit-dir-locals)
   (?P . projectile-test-project)
   (?R . projectile-regenerate-tags)
   (?S . projectile-save-project-buffers)
   (?V . projectile-browse-dirty-projects)
   (?I . projectile-invalidate-cache)
   (?t . projectile-toggle-between-implementation-and-test)
   (?u . projectile-run-project)
   (?z . projectile-cache-current-file)
   (?5 . ,(cu-make-keymap-func
           "projectile-display-in-other-frame"
           '((?D . projectile-dired-other-frame)
             (?b . projectile-switch-to-buffer-other-frame)
             (?a . projectile-find-other-file-other-frame)
             (?d . projectile-find-dir-other-frame)
             (?f . projectile-find-file-other-frame)
             (?g . projectile-find-file-dwim-other-frame)
             (?t . projectile-find-implementation-or-test-other-frame))))
   (?4 . ,(cu-make-keymap-func
           "projectile-display-in-other-window"
           '((?o . projectile-display-buffer)
             (?D . projectile-dired-other-window)
             (?b . projectile-switch-to-buffer-other-window)
             (?a . projectile-find-other-file-other-window)
             (?d . projectile-find-dir-other-window)
             (?f . projectile-find-file-other-window)
             (?g . projectile-find-file-dwim-other-window)
             (?t . projectile-find-implementation-or-test-other-window))))
   (?x . ,(cu-make-keymap-func
           "projectile-run"
           '((?e . projectile-run-eshell)
             (?s . projectile-run-shell)
             (?t . projectile-run-term))))
   (?s . ,(cu-make-keymap-func
           "projectile-search"
           '((?g . projectile-grep)
             (?r . projectile-ripgrep)
             (?s . projectile-ag))))))

(provide 'init-keybind)
