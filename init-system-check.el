(defconst *mac?* (eq system-type 'darwin))
(defconst *linux?* (eq system-type 'gnu/linux))
(defconst *windows?* (eq system-type 'windows-nt))

(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defconst *path-separator*
  (if *windows?* ";" ":"))

(provide 'init-system-check)

