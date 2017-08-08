(defconst *mac?* (eq system-type 'darwin))
(defconst *linux?* (eq system-type 'gnu/linux))
(defconst *windows?* (eq system-type 'windows-nt))
(defconst *amazon-machine?* (file-exists-p "~/.AMAZON"))

(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defconst *path-separator*
  (if *windows?* ";" ":"))

(setq custom-file
  (format "~/.emacs.d/custom-%s-%s.el"
          (if *mac?* "1" "0")
          (if *amazon-machine?* "1" "0")))

(when (and (not (file-exists-p custom-file)) (file-exists-p "~/.emacs.d/custom.el"))
  (rename-file "~/.emacs.d/custom.el" custom-file))

(provide 'init-system-check)
