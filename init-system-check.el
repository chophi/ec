(defun shell-result (command)
  (let ((s (shell-command-to-string command)))
    (substring s 0 (max 1 (- (length s) 1)))))

(defvar *is-amazon-machine* (file-exists-p "~/AMAZON_MACHINE"))

(defvar *is-windows-system-p* (eq system-type 'windows-nt))
(defvar *is-linux-system-p* (eq system-type 'gnu/linux))

(provide 'init-system-check)
