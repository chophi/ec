(defconst *mac?* (eq system-type 'darwin))
(defconst *linux?* (eq system-type 'gnu/linux))
(defconst *windows?* (eq system-type 'windows-nt))
(defconst *amazon-machine?* (file-exists-p "~/.AMAZON"))

(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defconst *path-separator*
  (if *windows?* ";" ":"))

(defconst *custom-file* (format "~/.emacs.d/custom-%s-%s.el" *amazon-machine?* *mac?*))
(when (and (not (file-exists-p *custom-file*)) (file-exists-p "~/.emacs.d/custom.el")
           (rename-file "~/.emacs.d/custom.el" *custom-file*)

(provide 'init-system-check)

