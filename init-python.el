(when *is-windows-system-p*
  (setq python-shell-interpreter "D:\\ProgEnv\\Python34\\python.exe"
        python-shell-interpreter-args "-i D:\\ProgEnv\\Python34\\Scripts\\ipython-script.py"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(setq-default python-indent-offset 4)

;;; ---------------------------------------
;;; https://github.com/jorgenschaefer/elpy
;;; ---------------------------------------
;;; pip install rope jedi flake8 importmagic autopep8 yapf
;; (defun pip-installed? (packages)
;;   (let ((pip-packages (shell-command-to-string "pip list"))
;;         (all-installed t))
;;     (dolist (pkg packages)
;;       (when (not (string-match pkg pip-packages))
;;         (setq all-installed nil))
;;       )
;;     all-installed))

;; (if (pip-installed? '("rope" "jedi" "flake8" "importmagic" "autopep8" "yapf"))
;;     (progn
;;       (require-package 'elpy)
;;       (elpy-enable))
;;   (progn
;;     (message "please install the required packages:\n sudo -H pip install rope jedi flake8 importmagic autopep8 yapf")))


;; (setq py-shell-name "d:/ProgEnv/Python34/Scripts/ipython.exe")

(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)

(require-package 'helm-pydoc)
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))

(provide 'init-python)
