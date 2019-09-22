(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-:") 'ace-jump-word-mode)

(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(global-unset-key (kbd "C-SPC"))
(global-set-key "%" 'match-paren)
(global-set-key (kbd "<backspace>") 'my-hungry-delete-backwards)

(define-key apples-mode-map "\C-c\C-e" 'my-run)

(global-set-key [remap query-replace-regexp] 'vr/query-replace)
(global-set-key [remap replace-regexp] 'vr/replace)
(global-set-key (kbd "C-x vq") 'vr/query-replace)
(global-set-key (kbd "C-x vr") 'vr/replace)

(global-unset-key (kbd "C-x o"))

(global-set-key (kbd "C-c p") 'duplicate-line)
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key [remap backward-up-list] 'backward-up-sexp)

(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "\C-xf") 'other-frame)
(global-set-key "\M-;" 'comment-line-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Switch frames, deprecated, as hammerspoon done this for me
;; (global-set-key "\C-cf" 'my-switch-screen)
;; (global-set-key "\C-xf" 'my-make-switch-frame-commands)
;; (global-set-key "\C-ct" 'my-switch-to-terminal-frame)

;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

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

(add-hook
 'nxml-mode-hook (lambda ()  (define-key nxml-mode-map "\C-c\C-v" nil)))

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
  (defconst publish-org-to-xwiki-keymap
    `(("p" . org-to-xwiki)
      ("o" . org-open-xwiki)))
  (cu-set-key-bindings global-map "\C-c\C-p" publish-org-to-xwiki-keymap)
  (cu-set-key-bindings org-mode-map "\C-c\C-p" publish-org-to-xwiki-keymap))

(defun my-toggle-treemacs (&optional switch-back)
  (interactive)
  (let ((curbuf (current-buffer)))
    (treemacs)
    (if switch-back
        (switch-to-buffer curbuf))))

(global-set-key "\C-ct" 'my-treemacs)
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
  (define-key java-mode-map "\C-c\C-l" nil)
  (define-key java-mode-map "\C-c\C-s" nil))

(with-eval-after-load "sh-script" (define-key sh-mode-map "\C-c\C-l" nil))
(defconst cu-path-util-map
  '((?p . cu-insert-path-replace-home)
    (?P . cu-insert-path-absolute-home)
    (?s . cu-save-current-file-path)
    (?o . cu-save-current-file-path-org-style)
    (?O . org-store-link)
    (?i . cu-open-with-idea)
    (?j . cu-open-link)
    (?e . cu-open-current-file-with-external-app)
    (?f . cu-visit-file-follow-symlink)
    (?n . find-name-dired)
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
    (?T . cu-toggle-lsp))
  "Key bindings for semantic")

(defun control-c-control-s ()
  (interactive)
  (cond
   ;; In lsp mode
   ((and (boundp 'lsp-mode) lsp-mode)
    (call-interactively
     (cu-make-commands-map-with-help-msg
      `((?T . cu-toggle-lsp)
        (?d . lsp-find-definition)
        (?r . lsp-find-references)
        (?u . lsp-ui-mode)
        (?e . cu-lsp-execute-command)
        (?f . lsp-format-buffer)
        (?t . lsp-describe-thing-at-point)
        (?b . lsp-java-build-project)
        (?i . lsp-java-organize-imports)
        (?R . ,(cu-make-keymap-func
                "lsp-java-refactor"
                '((?r . lsp-rename)
                  (?e . lsp-java-extract-to-constant)
                  (?u . lsp-java-add-unimplemented-methods)
                  (?p . lsp-java-create-parameter)
                  (?f . lsp-java-create-field)
                  (?l . lsp-java-create-local)
                  (?m . lsp-java-extract-method)
                  (?i . lsp-java-add-import))))
        (?g . ,(cu-make-keymap-func
                "lsp-generate-commands"
                '((?s . lsp-java-generate-to-string)
                  (?e . lsp-java-generate-equals-and-hash-code)
                  (?o . lsp-java-generate-overrides)
                  (?g . lsp-java-generate-getters-and-setters))))
        (?w . ,(cu-make-keymap-func
                "lsp-workspace-commands"
                '((?a . lsp-workspace-folders-add)
                  (?d . lsp-workspace-folders-remove)
                  (?r . lsp-restart-workspace)
                  (?s . lsp-workspace-folders-switch))))))))
   (t (call-interactively
       (cu-make-commands-map-with-help-msg
        `(,@semantic-key-bindings ,@eassist-key-bindings))))))

(global-set-key "\C-c\C-s" 'control-c-control-s)

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
   (?p . projectile-replace)
   (?d . projectile-dired)
   (?g . projectile-multi-occur)
   (?o . projectile-project-buffers-other-buffer)
   (?i . projectile-ibuffer)
   (?b . projectile-switch-to-buffer)
   (?k . projectile-kill-buffers)
   (?m . projectile-commander)
   (?f . projectile-switch-project)
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

(let ((key-binding
       '((?1 . my-only-one-window)
         (?2 . split-treemacs-with-other-window)
         (?3 . split-treemacs-with-other-two-windows)
         (?| . split-treemacs-with-other-two-windows-vertical)
         (?a . select-treemacs-from-split-windows)
         (?q . select-treemacs-from-split-windows)
         (?w . select-middle-window-from-split-windows)
         (?s . select-middle-window-from-split-windows)
         (?e . select-right-up-window-from-split-windows)
         (?d . select-right-down-window-from-split-windows))))
  (cu-set-key-bindings global-map "\C-xx" key-binding
                       nil ;; mode-list
                       -1 ;; show-message
                       )
  (cu-set-key-bindings global-map "\C-x\C-x" key-binding
                       nil ;; mode-list
                       -1 ;; show-message
                       ))

;;; Terminal
(global-unset-key "\C-z")
(cu-set-key-bindings global-map "\C-z"
                     '((?c . multi-term)
                       (?n . multi-term-next)
                       (?p . multi-term-prev)
                       (?g . uf-send-cwd-to-term)
                       (?w . uf-watch-current-directory)
                       (?r . uf-term-rename-buffer)
                       (?s . uf-switch-to-term-buffer)
                       (?p . uf-clear-prompt-command)
                       (?l . uf-send-current-line-command-to-term)
                       (?a . uf-toggle-active-status)
                       (?j . cu-open-link)
                       (?d . duplicate-term-and-switch)
                       (?k . uf-term-toggle-char-mode)
                       (?t . uf-change-cwd-to)
                       (?1 . uf-switch-to-term-1)
                       (?2 . uf-switch-to-term-2)
                       (?3 . uf-switch-to-term-3)
                       (?4 . uf-switch-to-term-4)
                       (?5 . uf-switch-to-term-5)
                       (?6 . uf-switch-to-term-6)
                       (?7 . uf-switch-to-term-7)
                       (?8 . uf-switch-to-term-8)
                       (?9 . uf-switch-to-term-9))
                     nil -1)

(provide '000.keybind)
