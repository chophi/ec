(require-package 'fill-column-indicator)
(require 'fill-column-indicator)

;; ?│ (\u2502) or ?┃ (\u2503), thin or thick version.
(setq fci-rule-color "#8b2252") ;; maroon4

(defvar enable-fci-mode-on-hooks nil
  "Whether to enable fci mode on some specified hooks")

(defun toggle-fci-on-specified-modes ()
  (interactive)
  (setq enable-fci-mode-on-hooks
        (not enable-fci-mode-on-hooks))
  (message "%s fci mode" (if enable-fci-mode-on-hooks "Enable" "Disable"))
  (dolist (hook
           '(c++-mode-hook
             c-mode-hook
             python-mode-hook
	         cv-mode-hook
             js-mode-hook
             lisp-mode-hook
	         html-mode-hook
             sawfish-mode-hook
             tex-mode-hook
	         emacs-lisp-mode-hook
	         latex-mode-hook))
    (if enable-fci-mode-on-hooks
        (add-hook hook 'turn-on-fci-mode)
      (remove-hook hook 'turn-on-fci-mode))))

(setq-default fill-column 80)
(toggle-fci-on-specified-modes)

;; ;; wired character show-ed in buffers opened by org-open-at-point when
;; ;; I set the fci-rule-character to ?│, below is the workaround to erase them
;; ;; by re-turn on the fci mode if it's enabled in the buffer.
;; (with-eval-after-load "org"
;;   (defadvice org-open-at-point (around fci-mode-work-around)
;;     ad-do-it
;;     (when (and (boundp fci-mode) fci-mode)
;;       (turn-on-fci-mode)))
;;   (ad-activate 'org-open-at-point))

(provide 'init-fill-column-indicator)
