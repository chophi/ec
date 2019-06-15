;; (cu-set-key-bindings dmesg-mode-map
;;                      "\C-c\C-d"
;;                      '((?D . dmesg-associate-device)
;;                        (?d . dmesg-get-device-dsn)
;;                        (?h . dmesg-highlight-init-rc-files)))

;; (add-to-list
;;  'term-bind-key-alist
;;  `("C-c e" .
;;    ,(cu-make-commands-map-with-help-msg
;;      '((?c . my-compilation-shell-minor-mode)
;;        (?n . compilation-next-error)
;;        (?p . compilation-previous-error)
;;        (?g . compile-goto-error)
;;        (?f . compilation-next-file)
;;        (?F . compilation-previous-file))))
;;  t)

;; android local doc
;; (defun android-doc-local-server()
;;   (interactive)
;;   (shell-command
;;    "dev_appserver.py ~/EDocs/android-docs/online-sac &"
;;    "*android local doc*" "*android local doc*"))

;; ;; neo-tree
;; (cu-set-key-bindings global-map "\C-c\C-n"
;;                      `((?O . open-nanoc-private-site)
;;                        (?D . nanoc-daemon-private)
;;                        (?o . open-nanoc-public-site)
;;                        (?d . nanoc-daemon-public)
;;                        ,@(if (file-exists-p "~/EDocs/android-docs")
;;                              `((?a . android-doc-local-server)))))

;; ;; uniform environment
;; (cu-set-key-bindings global-map "\C-c\C-f"
;;                      '((?f . ue-env-find-file)
;;                        (?i . ue-insert-to-env-list)))

;; (with-eval-after-load "ace-jump-mode"
;;   (setq ace-jump-word-mode-use-query-char nil)
;;   (cu-set-key-bindings global-map
;;                        "\C-xj"
;;                        '((?c . ace-jump-char-mode)
;;                          (?w . ace-jump-word-mode)
;;                          (?l . ace-jump-line-mode))))

;; (when (boundp 'pri-jira-home)
;;   (cu-set-key-bindings global-map
;;                        "\C-ch"
;;                        '((?i . pri-jira-open-index-file)
;;                          (?h . pri-jira-goto-issue-home)
;;                          (?o . pri-jira-open-issue-org-file)
;;                          (?u . pri-jira-update-issues)
;;                          (?U . pri-jira-force-update-issues))))

(provide 'init-keybind-no-freq)
