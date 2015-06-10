(when (file-exists-p "~/.emacs.d/lisp-snippets/az-common-env.el")
  (load-file "~/.emacs.d/lisp-snippets/az-common-env.el")

  (defun my-spec-command-str (addr libname)
    (message (format "%s -e %s/%s %s" arm-eabi-addr2line-path arm-ebai-lib-path libname addr)))

  (setq replace-from-str
        build-path-prefix
        replace-to-str
        my-repo-prefix
        )

  (defun get-libname ()
    (interactive)
    (let (str)
      (setq str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (string-match "\\(lib[a-zA-Z_]*\.so\\)" str)
      (message (match-string 0 str))
      ))

  (defun addrline-to-file ()
    (interactive)
    (forward-word)
    (let ((end (point))
          filename
          temp)
      (backward-word)
      (kill-ring-save (point) end)
      (setq filename (shell-command-to-string (my-spec-command-str (current-kill 0) (get-libname))))
      (setq filename (substring filename 0 (1- (length filename))))
      (setq temp (split-string filename ":"))
      (setq filename (car temp)
            temp (cadr temp))
      (find-file-other-window (replace-regexp-in-string replace-from-str replace-to-str filename))
      (goto-line (string-to-number temp))
      ))

  (defun jump-to-lib-assm ()
    (interactive)
    (let* ((libname (get-libname))
           (assname (concat (file-name-sans-extension libname) ".s")))
      (if (file-exists-p assname)
          (find-file-other-window assname)
        (progn (shell-command (message "%s -S %s/%s > %s" arm-eabi-objdump-path arm-ebai-lib-path libname assname))
               (find-file-other-window assname)))))

  )
