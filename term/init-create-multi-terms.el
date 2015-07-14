(defvar multi-term-config-list '()
  "init multi terms according to this config list")

(require '.multi-term-config ".multi-term-config.el" t)

(defun create-term-without-switch (term-name command-list)
  "Create new term buffer without switch to it"
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer current-prefix-arg))
    (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    (dolist (command command-list)
      (term-send-raw-string (concat command "\n")))
    (rename-buffer (format "*%s<%d>*" term-name (length multi-term-buffer-list)))))

(defun duplicate-term-and-switch ()
  (interactive)
  (let (term-name command-list term-name-list)
    (setq term-name-list '())
    (dolist (mterm multi-term-config-list)
      (add-to-list 'term-name-list (car mterm)))
    (setq term-name (ido-completing-read "Choose a Terminal to Duplicate: " term-name-list))
    (setq command-list (cadr (assoc term-name multi-term-config-list)))
    (switch-to-buffer (create-term-without-switch term-name command-list))))

(global-set-key "\C-zd" 'duplicate-term-and-switch)

(dolist (term-config multi-term-config-list)
  (create-term-without-switch (car term-config) (cadr term-config)))

(provide 'init-create-multi-terms)
