(require 'multi-term)
(defun disabled-return()
  (interactive)
  (message "disabled return key"))

(setq my-extra-needed-key
      '(
        ;; ("C-a" . move-beginning-of-line)
        ;; ("C-e" . move-end-of-line)
        ;; ("C-@" . set-mark-command)
        ("M-w" . kill-ring-save)
        ;; ("C-k" . kill-line)
        ;;("C-b" . term-send-left)
        ;;("C-f" . term-send-right)
        ("C-c C-k" . term-line-mode)
        ("C-y" . term-paste)
        ([(return)] . (lambda()(interactive) (term-send-raw-string "\C-j")))
        ))

(dolist (pair my-extra-needed-key)
  (add-to-list 'term-bind-key-alist pair))

(global-unset-key "\C-z")
(global-set-key "\C-zc" 'multi-term)
(global-set-key "\C-zn" 'multi-term-next)
(global-set-key "\C-zp" 'multi-term-prev)

(global-set-key "\M-n" 'tabbar-forward)
(global-set-key "\M-p" 'tabbar-backward)

(define-key term-mode-map "\C-c\C-k" 'term-toggle-between-modes)
;; setting keys \C-z + i(which from 1 to 8) to switch to the the ith term frame


(defun update-terms-keys ()
  (dotimes (i (length multi-term-buffer-list))
    (let* ((buf (nth i multi-term-buffer-list))
           (bufname (buffer-name buf))
           (order  (1+ i)))
      (with-current-buffer buf
        (rename-buffer (replace-regexp-in-string "<[0-9]*>" (format "<%d>" order) bufname))))))

(dotimes (i 8)
  (global-set-key
   (concat "\C-z" (number-to-string (+ i 1)))
   `(lambda() (interactive)
      (update-terms-keys)
      (let (tn) (when (setq tn (nth ,i (if (fboundp 'multi-term-list)
                                           (multi-term-list)
                                         multi-term-buffer-list)))
                  (switch-to-buffer tn))))))


(defun uf-send-cwd-to-term ()
  (interactive)
  (let ((cwd default-directory)
        (term-buf nil))
    (catch 'found
      (dolist (buf-win (window-list))
        (when (eq 'term-mode (with-current-buffer (window-buffer buf-win)
                               major-mode))
          (setq term-buf (window-buffer buf-win))
          (throw 'found term-buf))))
    (when (not term-buf)
      (setq term-buf (get-buffer (ido-completing-read "Choose A Term Buffer: " (mapcar (lambda (para) (buffer-name para)) multi-term-buffer-list)))))
    (with-current-buffer term-buf
      (term-send-raw-string (format "cd %s\n" cwd)))
    (switch-to-buffer-other-window term-buf)
    (end-of-buffer)))

(defconst *temp-cwd-exchange-file* "~/.temp-cwd-exchange-file")

(defun uf-watch-current-directory ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (term-send-raw-string (format "echo `pwd` > %s\n" *temp-cwd-exchange-file*))
  (sleep-for 0.5)
  (let ((path (my-shell-command-to-string (format "cat %s" *temp-cwd-exchange-file*))))
    (find-file-other-window path)))

(global-set-key "\C-zg" 'uf-send-cwd-to-term)
(global-set-key "\C-zw" 'uf-watch-current-directory)

(provide 'init-term-keys)
