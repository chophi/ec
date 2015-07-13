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
    (rename-buffer term-name)
    (dolist (command command-list)
      (term-send-raw-string (concat command "\n")))))

(dolist (term-config multi-term-config-list)
  (create-term-without-switch (car term-config) (cadr term-config)))

(provide 'init-create-multi-terms)
