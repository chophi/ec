(require 'multi-term)

;;; Override some keybind in multi-term mode
(dolist (override '(("M-w" . kill-ring-save)
                    ("C-u" . universal-argument)
                    ("C-c C-k" . term-line-mode)
                    ("C-c C-z" . term-quit-subjob)
                    ([(return)] . (lambda()(interactive) (term-send-raw-string "\C-j")))))
  (add-to-list 'term-bind-key-alist override))

;;; update all terminal names
(defun update-terms-name ()
  "update all terminal names"
  (dotimes (i (length multi-term-buffer-list))
    (let* ((buf (nth i multi-term-buffer-list))
           (bufname (buffer-name buf))
           (order  (1+ i)))
      (with-current-buffer buf
        (rename-buffer (replace-regexp-in-string "<[0-9]*>" (format "<%d>" order) bufname))))))

;;; switch to one of the terminal
;; setting keys \C-z + i(which from 0 to max-terminal-count) to switch to the the ith term frame
(defconst max-terminal-count 9)
(defun uf-switch-to-term-i (&optional num)
  (interactive)
  (let ((num (or num (string-to-number
                      (char-to-string (read-char "The terminal order (1-9):"))))))
    (update-terms-name)
    (let (tn)
      (when
          (setq tn (nth (1- num) (if (fboundp 'multi-term-list)
                                     (multi-term-list)
                                   multi-term-buffer-list)))
        (switch-to-buffer tn)))))

;; TODO: make it a loop to define the functions.
(defun uf-switch-to-term-1 () (interactive) (uf-switch-to-term-i 1))
(defun uf-switch-to-term-2 () (interactive) (uf-switch-to-term-i 2))
(defun uf-switch-to-term-3 () (interactive) (uf-switch-to-term-i 3))
(defun uf-switch-to-term-4 () (interactive) (uf-switch-to-term-i 4))
(defun uf-switch-to-term-5 () (interactive) (uf-switch-to-term-i 5))
(defun uf-switch-to-term-6 () (interactive) (uf-switch-to-term-i 6))
(defun uf-switch-to-term-7 () (interactive) (uf-switch-to-term-i 7))
(defun uf-switch-to-term-8 () (interactive) (uf-switch-to-term-i 8))
(defun uf-switch-to-term-9 () (interactive) (uf-switch-to-term-i 9))

(setq multi-term-buffer-name "TM")
(defvar-local term--uuid nil "Terminal UUID")
(defvar term-name-template "*TM<1>*")

(defun send-to-all-terminal ()
  (interactive)
  (let ((comm (read-string "Input the command to send to all terminal: ")))
    (dolist (term-buf multi-term-buffer-list)
      (with-current-buffer term-buf
        (term-send-raw-string (format "%s\n" comm))))))


(defun move-terminal-as-nth ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (let ((n (read-number
            (format
             "Please enter the number you want to move this terminal to be nth (1-%d): "
             (length multi-term-buffer-list))))
        (cur-buf (current-buffer))
        (temp-list '())
        (old-list multi-term-buffer-list))
    (setq old-list (delete (current-buffer) old-list))
    (dotimes (i (1- n))
      (add-to-list 'temp-list (car old-list) t)
      (setq old-list (cdr old-list)))
    (add-to-list 'temp-list cur-buf t)
    (setq temp-list (append temp-list old-list))
    (setq multi-term-buffer-list temp-list)))

(defun move-terminal-as-first ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (setq multi-term-buffer-list (delete (current-buffer) multi-term-buffer-list))
  (setq multi-term-buffer-list (add-to-list 'multi-term-buffer-list (current-buffer)))
  (update-terms-name))

(defadvice multi-term (around multi-term-ad)
  (when (>= (length multi-term-buffer-list) max-terminal-count)
    (error "too many terminal now, try to reuse!"))
  (let* ((random-uuid (cu-uuid))
         (process-environment
          (nconc
           (list (format "TERM_UUID=%s" random-uuid))
           process-environment)))
    ad-do-it
    (update-terms-name)
    (setq-local term--uuid random-uuid)))
(ad-activate 'multi-term)

(defadvice term-paste (around check-paste-length)
  (when (> (length (current-kill 0)) 1024)
    (error "the item to paste is too long"))
  ad-do-it)
(ad-activate 'term-paste)

(defun term-prefix (buf)
  (substring (buffer-name buf) 0 (length term-name-template)))

(defun uf-send-command-to-term (command &optional switch-to-buffer-p term-buffer)
  (let ((term-buf nil))
    (catch 'found
      (when (and term-buffer (buffer-live-p term-buffer))
        (setq term-buf term-buffer)
        (throw 'found term-buffer))
      (dolist (buf-win (window-list))
        (when (and (eq 'term-mode (with-current-buffer (window-buffer buf-win)
                                    major-mode))
                   (not (eq (window-buffer buf-win) (current-buffer))))
          (setq term-buf (window-buffer buf-win))
          (throw 'found term-buf))))
    (when (not term-buf)
      (let ((choices (mapcar (lambda (para) (buffer-name para)) multi-term-buffer-list)))
        (setq term-buf (get-buffer (ido-completing-read "Choose A Term Buffer: " choices)))))
    (with-current-buffer term-buf
      (term-send-raw-string command))
    (when switch-to-buffer-p
      (switch-to-buffer-other-window term-buf)
      (end-of-buffer))))

(defun uf-send-cwd-to-term ()
  (interactive)
  (let ((cwd default-directory))
    (uf-send-command-to-term (format "cd %s\n" cwd) t)))

(defun uf-change-cwd-to ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (uf-send-command-to-term
   (format "cd %s\n" (ido-read-directory-name "CD to: ")) nil (current-buffer)))

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
  (let ((path default-directory))
    (find-file-other-window path)))

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
  (term-send-raw-string (format "export PROMPT_COMMAND=__prompt_command\n")))

(defun get-term-buffer (term-id)
  (let (buf)
    (dolist (term multi-term-buffer-list)
      (with-current-buffer term
        (when (and (equal term--uuid term-id))
          (setq buf term))))
    buf))

(defun _terminal_fields (&optional term-id)
  (interactive)
  (with-current-buffer (if term-id (get-term-buffer term-id) (current-buffer))
    (let* ((index 0)
           (buf-name (buffer-name))
           (buf-len (length buf-name))
           (field-str-table (make-hash-table :test 'equal)))
      (while (and (>= index 0) (< index buf-len)
                  (string-match "\\\[\\(.*?\\):\\(.*?\\)\\\]" buf-name index))
        (let ((key (match-string 1 buf-name))
              (val (match-string 2 buf-name))
              (str (match-string 0 buf-name)))
          (puthash key val field-str-table)
          (setq index (+ index (length str)))))
      field-str-table)))

(defun _make_buffer_name (opr &optional term-id field-str-table)
  "OPR is either 'clear or 'add,
if it's 'clear and FIELD-STR-TABLE is not specified, then all fields will be cleared.
if it's add, then field-str-table must be specified, these fields will be added, and then all fields will be sorted.
"
  (interactive)
  (with-current-buffer (if term-id (get-term-buffer term-id) (current-buffer))
    (let ((buf-name (buffer-name))
          (lst nil)
          (append-name ""))
      (if (eq 'clear opr)
          (let ((fst (or field-str-table (_terminal_fields term-id))))
            (maphash (lambda (k v)
                       (setq buf-name
                             (replace-regexp-in-string
                              (regexp-quote (format "[%s:%s]" k v)) "" buf-name)))
                     fst))
        (or (eq 'add opr) (error "the opr should be add or clear"))
        (or (hash-table-p field-str-table) (error "the FIELD-STR-TABLE should be hash table"))
        ;; the add fields should overwrite the original field if they has the same key.
        (maphash
         (lambda (k v)
           (when (not (gethash k field-str-table))
             (puthash k v field-str-table)))
         (_terminal_fields term-id))
        (maphash
         (lambda (k v)
           (push (list k v) lst))
         field-str-table)
        (setq lst (sort lst (lambda (a b) (string< (car a) (car b)))))
        (dolist (x lst)
          (setq append-name (format "%s[%s:%s]" append-name (car x) (cadr x))))
        (setq buf-name (concat (_make_buffer_name 'clear term-id nil) append-name)))
      buf-name)))

(defun terminal-name-add-field (term-id key val)
  (let ((term-buf (get-term-buffer term-id)))
    (when (not term-buf)
      (error "terminal %s doesn't exist" term-id))
    (with-current-buffer term-buf
      (let ((hash-table (make-hash-table :test 'equal)))
        (puthash key val hash-table)
        (rename-buffer (_make_buffer_name
                        'add term-id hash-table))))))

(defun uf-term-rename-buffer (clear)
  (interactive "P")
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (if clear
      (rename-buffer (_make_buffer_name 'clear term--uuid))
    (let ((field-str-table (_terminal_fields term--uuid))
          (field (read-string "Field: "))
          (name (read-string "Name: "))
          lst)
      (if (equal name "")
          ;; remove the field
          (when (gethash field field-str-table)
            (remhash field field-str-table))
        (puthash field name field-str-table))
      (rename-buffer (_make_buffer_name 'clear term--uuid))
      (rename-buffer (_make_buffer_name 'add term--uuid field-str-table)))))

(defun uf-send-current-line-command-to-term (&optional no-confirm)
  (interactive)
  (let ((command
         (cu-strip-string
          (buffer-substring-no-properties (line-beginning-position) (line-end-position))
          t t)))
    (when (and (not no-confirm) (y-or-n-p (format "Sending command to terminal:\n [%s]\n" command)))
      (uf-send-command-to-term (concat command "\n")))))

(defun uf-send-current-buffer-to-term ()
  (interactive)
  (with-current-buffer (current-buffer)
    (when (y-or-n-p "Send the entire buffer to terminal, Really?")
      (let* ((str (buffer-substring-no-properties (point-min) (point-max)))
             (lines (split-string str "\n")))
        (dolist (line lines)
          (setq line (cu-strip-string line t t))
          (when (not (equal "" line))
            (uf-send-command-to-term (format "%s\n" line))
            (sleep-for 0.1)
            (message "send [%s]\n" line)))))))

(defun uf-toggle-active-status ()
  (interactive)
  (when (not (eq 'term-mode major-mode))
    (error "only use this command with term-mode buffer"))
  (if (or (not (boundp 'term--status)) (eq term--status 'active))
      (progn (setq-local term--status 'deactive)
             (setq multi-term-buffer-list
                   (delete (current-buffer) multi-term-buffer-list))
             (update-terms-name))
    (setq-local term--status 'active)
    (add-to-list 'multi-term-buffer-list (current-buffer))
    (update-terms-name)))

(defun uf-term-toggle-char-mode()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode))
  (message "Switched terminal to %s mode" (if (term-in-line-mode) "Line" "Char")))

;;; Set fonts for term-mode
(defun set-term-mode-face ()
  "Set face for Term mode"
  (defface term-mode-face
    '((t :family "Monaco for Powerline"
         :size 16))
    "Buffer local face for term mode")
  (buffer-face-set 'term-mode-face))
(add-hook 'term-mode-hook 'set-term-mode-face)

(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)
            (define-key term-raw-map "\C-y" 'term-send-raw)
            (define-key term-raw-map "\M-y" 'term-paste)))

(modify-syntax-entry ?_ "w" term-mode-syntax-table)
(modify-syntax-entry ?- "w" term-mode-syntax-table)
(modify-syntax-entry ?. "w" term-mode-syntax-table)
(modify-syntax-entry ?/ "w" term-mode-syntax-table)

(provide 'init-multi-term)
