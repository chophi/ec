(setq-default python-indent-offset 4)

(defconst enable-jedi nil)
(when enable-jedi
  (with-eval-after-load "python"
        (add-hook 'python-mode-hook 'jedi:setup)
    (add-hook 'python-mode-hook 'jedi:ac-setup)
    (setq jedi:complete-on-dot t)))

(require 'smart-shift)
(with-eval-after-load "python"
  (add-hook 'python-mode-hook 'smart-shift-mode-on)
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))

(defun kill-buffer-when-process-finished ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))))))))

;; Kill the *Python* buffer when exited after running "exit()"
(add-hook 'inferior-python-mode-hook 'kill-buffer-when-process-finished)

;;; Don't enable it by default, or everytime you save the python source code,
;;; it will re-format the file.
;;; (add-hook 'python-mode-hook 'yapf-mode)

(provide 'init-python)
