;; a major used for view kernel logs

(defvar dmesg-events
  '())

(defvar dmesg-keywords
  '("Kernel command line" "Freeing init memory" "WARN" "ERROR" "WARNING" "DEBUG"))

(defvar dmesg-tab-width nil "Width of a tab for DMESG mode")

(defconst dmesg-font-lock-defaults
  `((
     ("@\\|$" . font-lock-keyword-face)
     ("\\[[0-9]+,\\s-+[a-zA-Z].+?\\]" . font-lock-function-name-face)
     ("\\[\\s-+[0-9]+\.[0-9]+?\\]" . font-lock-string-face)
     ("0x[0-9a-fA-F]+" . font-lock-variable-name-face)
     ("<[0-3]>*" . font-lock-warning-face)
     ( ,(regexp-opt dmesg-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt dmesg-events 'words) . font-lock-constant-face)
     )))

(define-derived-mode dmesg-mode fundamental-mode "dmesg script"
  "dmesg mode is a major mode for editing DMESG  files"
  (setq font-lock-defaults dmesg-font-lock-defaults)
  (when dmesg-tab-width
    (setq tab-width dmesg-tab-width))
  )

(defun dmesg-associate-device ()
  (interactive)
  (let ((devices))
    (setq devices (split-string
                   (shell-command-to-string
                    "adb devices | tac | head -n -1 | cut -f 1")))
    (setq-local device-dsn (ido-completing-read "Choose a device: " devices))))

(defun dmesg-get-device-dsn (&optional no-print no-error)
  (interactive)
  (with-current-buffer (current-buffer)
    (if (boundp 'device-dsn)
        (if no-print device-dsn (message "Bounded device: %s" device-dsn))
      (if no-error nil (error "No device bounded")))))

(defun _adb (device command)
  (equal 0 (shell-command (format "adb -s %s %s" device command))))

(defun _try_pull (device file)
  (let* ((relative-dir (file-name-directory file))
         (base-name (file-name-nondirectory file))
         (dir (cu-join-path "pulled-initrc" relative-dir))
         (target-file (cu-join-path dir base-name)))
    (make-directory (cu-join-path dir) t)
    (when (file-exists-p target-file)
      (delete-file target-file))
    (if (_adb device (format "pull %s %s" (cu-join-path file) target-file))
        target-file
      nil)))

(defun dmesg-goto-file ()
  (interactive)
  (let ((file-location (get-text-property (point)'file-location)))
    (if file-location
        (find-file-other-window (message file-location))
      (error "no file-location"))))

(defconst dmesg-goto-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'dmesg-goto-file)
    (define-key map (kbd "RET") #'dmesg-goto-file)
    (define-key map "o" #'dmesg-goto-file)
    map))

(defun dmesg-highlight-init-rc-files ()
  (interactive)
  (let* (start
         end
         (dsn (dmesg-get-device-dsn t t))
         (pull-files (when (and dsn (_adb dsn "root"))
                       (y-or-n-p "Pull files?")))
         (file-name t))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "init: Parsing file \\(/.*rc\\)" nil t)
      (setq start (match-beginning 1)
            end (match-end 1))
      (when pull-files
        (setq file-name (_try_pull dsn (buffer-substring start end))))
      (when file-name
        (add-text-properties start end `(file-location
                                         ,file-name
                                         keymap
                                         ,dmesg-goto-file-map))
        (overlay-put (make-overlay start end) 'face 'link))))))

(add-to-list 'auto-mode-alist '("/[kd]me?sg.*\.log$" . dmesg-mode))
(provide 'init-dmesg-mode)
