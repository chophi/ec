(defconst os
  (cond ((eq system-type 'darwin) 'macos)
        ((eq system-type 'gnu/linux) 'linux)
        ((eq system-type 'windows-nt) 'windows)
        (t nil))
  "Operating System, can be 'macos, 'linux or 'windows")

(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Put it here, because I may want to separate it for different OSs.
(setq custom-file "~/.emacs.d/lisp/custom.el")

(defun company-computer-p ()
  (file-exists-p "~/.COMPANY_COMPUTER"))

(provide 'init-computer-check)
