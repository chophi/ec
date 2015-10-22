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

;; setting keys \C-z + i(which from 1 to max-terminal-count) to switch to the the ith term frame
(defconst max-terminal-count 9)
(dotimes (i max-terminal-count)
  (global-set-key
   (concat "\C-z" (number-to-string (+ i 1)))
   `(lambda() (interactive)
      (update-terms-name)
      (let (tn) (when (setq tn (nth ,i (if (fboundp 'multi-term-list)
                                           (multi-term-list)
                                         multi-term-buffer-list)))
                  (switch-to-buffer tn))))))

(setq multi-term-buffer-name "TM")
(defvar term-name-template "*TM<1>*")

(defun update-term-external-env ()
  (interactive)
  (dolist (term-buf multi-term-buffer-list)
    (with-current-buffer term-buf
      (term-send-raw-string (format "export TERM_BUFFER_NAME=\"%s\"\n" (buffer-name))))))

(defun update-terms-name ()
  "update all terminal names"
  (dotimes (i (length multi-term-buffer-list))
    (let* ((buf (nth i multi-term-buffer-list))
           (bufname (buffer-name buf))
           (order  (1+ i)))
      (with-current-buffer buf
        (rename-buffer (replace-regexp-in-string "<[0-9]*>" (format "<%d>" order) bufname))))))

(defadvice multi-term (around multi-term-ad)
  (when (>= (length multi-term-buffer-list) max-terminal-count)
    (error "too many terminal now, try to reuse!"))
  ad-do-it
  (update-terms-name)
  (update-term-external-env))
(ad-activate 'multi-term)

(defun term-prefix (buf)
  (substring (buffer-name buf) 0 (length term-name-template)))

(defun uf-send-command-to-term (command &optional switch-to-buffer-p)
  (let ((term-buf nil))
    (catch 'found
      (dolist (buf-win (window-list))
        (when (eq 'term-mode (with-current-buffer (window-buffer buf-win)
                               major-mode))
          (setq term-buf (window-buffer buf-win))
          (throw 'found term-buf))))
    (when (not term-buf)
      (setq term-buf (get-buffer (ido-completing-read "Choose A Term Buffer: " (mapcar (lambda (para) (buffer-name para)) multi-term-buffer-list)))))
    (with-current-buffer term-buf
      (term-send-raw-string command))
    (when switch-to-buffer-p
        (switch-to-buffer-other-window term-buf)
      (end-of-buffer))))

(defun uf-send-cwd-to-term ()
  (interactive)
  (let ((cwd default-directory))
    (uf-send-command-to-term (format "cd %s\n" cwd) t)))

(defun compile-with-term ()
  (interactive)
  (let ((cwd default-directory)
        (compile-command (read-string "Compile Command: ")))
    (uf-send-command-to-term (format "cd %s && %s 2>&1 | tee compile.log\n" cwd compile-command) nil)
    (catch 'compile-file-generated
      (dotimes (time 10)
        (if (file-exists-p "compile.log")
          (throw 'compile-file-generated t)
          (sleep-for 0.1))))
    (find-file-other-window "compile.log")
    (compilation-mode)))

(defconst *temp-cwd-exchange-file* "~/.temp-cwd-exchange-file")

(defun uf-watch-current-directory ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (term-send-raw-string (format "echo `pwd` > %s\n" *temp-cwd-exchange-file*))
  (sleep-for 0.5)
  (let ((path (my-shell-command-to-string (format "cat %s" *temp-cwd-exchange-file*))))
    (find-file-other-window path)))

(defun uf-term-rename-buffer ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (update-terms-name)
  (rename-buffer (concat (term-prefix (current-buffer)) "[" (read-string "Buffer Name[Term prefix keep]: ") "]")))

(defun uf-switch-to-term-buffer ()
  (interactive)
  (let ((buffer-list '()))
    (dolist (term multi-term-buffer-list)
             (add-to-list 'buffer-list (buffer-name term)))
    (switch-to-buffer (get-buffer  (ido-completing-read "Switch to Term: " buffer-list)))))

(defun uf-clear-prompt-command ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (term-send-raw-string (format "export PROMPT_COMMAND=\"\"\n")))

(defun --append-terminal-name (term-buf-name append-name)
  (let ((term-buf-prefix (substring term-buf-name 0 (length term-name-template))))
    (dolist (term multi-term-buffer-list)
      (when (equal (term-prefix term) term-buf-prefix)
        (with-current-buffer term
          (rename-buffer (concat term-buf-prefix "[" append-name "]")))))))

(global-set-key "\C-zg" 'uf-send-cwd-to-term)
(global-set-key "\C-zw" 'uf-watch-current-directory)
(global-set-key "\C-zr" 'uf-term-rename-buffer)
(global-set-key "\C-zs" 'uf-switch-to-term-buffer)
(global-set-key "\C-zl" 'uf-clear-prompt-command)

(provide 'init-term-keys)
