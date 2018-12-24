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
(defun my-toggle-magit-blame-mode ()
  (interactive)
  (if (and (boundp 'magit-blame-mode) magit-blame-mode)
      (call-interactively 'magit-blame-mode)
    (call-interactively 'magit-blame)))
(defvar my-magit-key-map
  '((?s . magit-status)
    (?b . my-toggle-magit-blame-mode)
    (?p . magit-pull)
    (?l . magit-log-head)
    (?L . my-magit-log-head-fast))
  "my keymap for magit")

(cu-set-key-bindings global-map "\C-c\C-v" my-magit-key-map)
(with-eval-after-load "sgml-mode"
  (define-key html-mode-map "\C-c\C-v" nil))
(with-eval-after-load "python"
  (cu-set-key-bindings python-mode-map "\C-c\C-v"
                       `(,my-magit-key-map
                         ((?c . python-check)))))

;; Remapping org-babel-key-prefix to \C-cv for magit key bindings.
(with-eval-after-load "org"
  (setq org-babel-key-prefix "\C-cv")
  (define-key org-mode-map org-babel-key-prefix org-babel-map)
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
    (?l . eopengrok-list-projects)
    (?S . eopengrok-toggle-swap-mode)
    (?D . thread-grok-index-main)
    (?K . kill-grok-indexing-buffer)
    (?n . eopengrok-use-newer-index-file)))

(cu-set-key-bindings
 global-map "\C-c\C-g" `(,my-opengrok-map)
 '(("narrowed project" . (eopengrok-get-current-narrowed-project))
   ("swap mode" . eopengrok-swap-mode)))

(defun android-doc-local-server()
  (interactive)
  (shell-command
   "dev_appserver.py ~/EDocs/android-docs/online-sac &"
   "*android local doc*" "*android local doc*"))

;; neo-tree
(cu-set-key-bindings global-map "\C-c\C-n"
                     `((?t . neotree-toggle)
                       (?O . open-nanoc-private-site)
                       (?D . nanoc-daemon-private)
                       (?o . open-nanoc-public-site)
                       (?d . nanoc-daemon-public)
                       ,@(if (file-exists-p "~/EDocs/android-docs")
                             `((?a . android-doc-local-server)))))



(add-hook 'emacs-lisp-mode-hook
          (lambda ()  (define-key emacs-lisp-mode-map "\C-\M-x" nil)))
(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map "\C-\M-t" nil)))
(add-hook 'paredit-mode-hook
          (lambda () (define-key paredit-mode-map "\C-\M-p" nil)))

(when (fboundp 'control-x-f)
  (global-set-key "\C-cw" 'control-x-f))

(global-set-key "\C-ct" 'my-switch-to-terminal-frame)

(defun _my-switch-screen ()
  (interactive)
  (if window-system
    (call-interactively
     (_make-commands-map-with-help-msg
      '((?l . my-switch-to-leftmost-screen)
        (?r . my-switch-to-rightmost-screen)
        (?t . my-switch-to-topmost-screen)
        (?d . my-switch-to-downmost-screen)
        (?n . my-switch-to-next-screen-clockwise)
        (?p . my-switch-to-next-screen-anticlockwise)
        (?s . my-select-frame))))
    (my-select-frame)))

(global-set-key "\C-cf" '_my-switch-screen)

(global-set-key "\C-xf"
                (lambda () (interactive)
                  (call-interactively
                   (_make-commands-map-with-help-msg
                    `((?c . my-make-frame)
                      (?d . delete-frame)
                      (?w . my-delete-other-frames)
                      (?r . my-set-frame-name)
                      ,@(when (fboundp 'control-x-f)
                          `((?f . control-x-f)))
                      (?s . my-select-frame)
                      (?t . my-set-term-frame)
                      ,@(if (not window-system)
                          `((?n . my-next-frame)
                            (?p . my-previous-frame))
                          `((?n . my-switch-to-next-screen-clockwise)
                            (?p . my-switch-to-next-screen-anticlockwise))))))))

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

(add-to-list
 'term-bind-key-alist
 `("C-c e" .
   ,(_make-commands-map-with-help-msg
     '((?c . my-compilation-shell-minor-mode)
       (?n . compilation-next-error)
       (?p . compilation-previous-error)
       (?g . compile-goto-error)
       (?f . compilation-next-file)
       (?F . compilation-previous-file))))
 t)

(defun graphviz-set-extension()
  (interactive)
  (setq graphviz-dot-preview-extension
        (ido-completing-read "Extension: " '("svg" "png" "jpeg"))))

(with-eval-after-load "graphviz-dot-mode"
  (define-key graphviz-dot-mode-map
    (kbd "<tab>")
    'graphviz-indent-or-complet-word)
  
  (cu-set-key-bindings
   graphviz-dot-mode-map
   "\C-c\C-d"
   '((?p . graphviz-dot-preview)
     (?v . graphviz-dot-view)
     (?e . graphviz-set-extension))
   '(("Output Image Extension" . graphviz-dot-preview-extension))))

(defun toggle-plantuml-convert-to-latex ()
  (interactive)
  (setq plantuml-convert-to-latex (not plantuml-convert-to-latex)))

(with-eval-after-load "plantuml-mode"
  (cu-set-key-bindings
   plantuml-mode-map
   "\C-c\C-d"
   '((?p . plantuml-execute)
     (?v . plantuml-preview)
     (?e . graphviz-set-extension)
     (?t . toggle-plantuml-convert-to-latex))
   '(("Output Image Extension" . graphviz-dot-preview-extension)
     ("Convert to latex First" . plantuml-convert-to-latex))))

(with-eval-after-load "tex-buf"
  (cu-set-key-bindings LaTeX-mode-map
                       "\C-c\C-d"
                       '((?p . compile-tikz-to-svg))))

(with-eval-after-load "python"
  (cu-set-key-bindings python-mode-map
                       "\C-c\C-s"
                       '((?s . python-shell-send-string)
                         (?d . jedi:goto-definition)
                         (?i . jedi:show-doc)
                         (?c . jedi:complete)
                         (?n . jedi:goto-definition-next))))

(with-eval-after-load "ace-jump-mode"
  (setq ace-jump-word-mode-use-query-char nil)
  (cu-set-key-bindings global-map
                       "\C-xj"
                       '((?c . ace-jump-char-mode)
                         (?w . ace-jump-word-mode)
                         (?l . ace-jump-line-mode))))

(defun cp-custom-compile-no-rule ()
  (interactive)
  (call-interactively
   (_make-commands-map-with-help-msg
    '((?c . smart-compile-compile)
      (?e . smart-compile-run)
      (?r . recompile-quietly)))))

(when (boundp 'pri-jira-home)
  (cu-set-key-bindings global-map
                       "\C-ch"
                       '((?i . pri-jira-open-index-file)
                         (?h . pri-jira-goto-issue-home)
                         (?o . pri-jira-open-issue-org-file)
                         (?u . pri-jira-update-issues)
                         (?U . pri-jira-force-update-issues))))

(cu-set-key-bindings dmesg-mode-map
                     "\C-c\C-d"
                     '((?D . dmesg-associate-device)
                       (?d . dmesg-get-device-dsn)
                       (?h . dmesg-highlight-init-rc-files)))

(cu-set-key-bindings
 projectile-mode-map
 "\C-cp"
 `((?v . projectile-vc)
   (?r . projectile-replace)
   (?e . projectile-recentf)
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
   (?f . ,(cu-make-keymap-func
           "projectile-find"
           '((?n . projectile-find-file-in-known-projects)
             (?t . projectile-find-test-file)
             (?o . projectile-find-other-file)
             (?d . projectile-find-dir)
             (?f . projectile-find-file)
             (?w . projectile-find-file-dwim)
             (?g . projectile-find-tag)
             (?i . projectile-find-file-in-directory))))
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
