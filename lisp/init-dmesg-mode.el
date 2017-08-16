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

(provide 'init-dmesg-mode)
