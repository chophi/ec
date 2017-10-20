(defconst programming-hook-list
  '(c++-mode-hook
    c-mode-hook
    java-mode-hook
    python-mode-hook
	cv-mode-hook
    js-mode-hook
    lisp-mode-hook
	html-mode-hook
    sawfish-mode-hook
    tex-mode-hook
	emacs-lisp-mode-hook
	latex-mode-hook))

(require-package 'fill-column-indicator)
(require 'fill-column-indicator)

(defun enable-fci-mode-hook ()
  (setq fci-rule-color "#8b2252") ;; maroon4
  (fci-mode 1))

(defconst *enable-prog-fci-mode* t)
(defun global-programming-fci-mode ()
  (interactive)
  (dolist (hook programming-hook-list)
    (if *enable-prog-fci-mode*
        (add-hook hook 'enable-fci-mode-hook)
      (remove-hook hook 'enable-fci-mode-hook))
    )
  (if *enable-prog-fci-mode*
      (setq *enable-prog-fci-mode* nil)
    (setq *enable-prog-fci-mode* t)))

(eval-after-load 'fill-column-indicator
  (progn
    (setq-default fill-column 80)
    (global-programming-fci-mode)))

(provide 'init-fill-column-indicator)
