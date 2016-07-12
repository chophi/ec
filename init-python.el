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
(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq-default python-indent-offset 4)

(when *is-mac-machine*
  (add-to-list 'exec-path "/opt/local/bin/"))

(provide 'init-python)
