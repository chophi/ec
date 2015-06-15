(defun shell-result (command)
  (let ((s (shell-command-to-string command)))
    (substring s 0 (max 1 (- (length s) 1)))))

(defvar *is-amazon-machine* (file-exists-p "~/AMAZON_MACHINE"))

(defvar *is-windows-system-p* (eq system-type 'windows-nt))
(defvar *is-linux-system-p* (eq system-type 'gnu/linux))

(defvar *is-linux-not-amazon*
  (and *is-linux-system-p* (not *is-amazon-machine*)))

(defvar *is-amazon-linux*
  (and *is-amazon-machine* *is-linux-system-p*))

(defvar *is-amazon-linux-putty*
  (and *is-amazon-linux*  (not (window-system))))

(defvar *is-amazon-linux-window*
  (and *is-amazon-linux*  (window-system)))

(provide 'init-system-check)
