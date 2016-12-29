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


;; if it's ipython 0.10, python-shell-completion-string-code is different as below.                                    
;; (setq python-shell-completion-string-code
;;       "';'.join(__IP.complete('''%s'''))\n"
;;       python-shell-completion-module-string-code "")

;; (add-to-list 'load-path (concat (getenv "HOME") "\\.emacs.d\\site-lisp"))
;; (require 'pymacs)

;; (add-to-list 'load-path (concat (getenv "HOME") "\\.emacs.d\\elpa\\python-mode-6.1.3"))
;; (setq py-install-directory (concat (getenv "HOME") "\\.emacs.d\\elpa\\python-mode-6.1.3"))
;; (require 'python-mode)

;; (setq py-shell-name "d:/ProgEnv/Python34/Scripts/ipython.exe")
(when (not *is-mac-machine*)
  (require-package 'jedi)
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup))

(setq-default python-indent-offset 2)

(require-package 'helm-pydoc)
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))

(when *is-mac-machine*
  (add-to-list 'exec-path "/opt/local/bin/"))

;; use ido instead of helm with following use-ido-list
(let ((use-ido-list
       '(describe-function
         describe-variable
         describe-symbol)))
  (dolist (use-ido use-ido-list)
    (setf (cdr (assoc use-ido helm-completing-read-handlers-alist)) 'ido)))

(provide 'init-python)
