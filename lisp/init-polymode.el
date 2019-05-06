(require-package 'polymode)

(require-package 'poly-org)

;; TODO: FIXME: funny char shows when poly-org-mode enabled.
(setq auto-mode-alist  (delete '("\\.org\\'" . poly-org-mode) auto-mode-alist))

(provide 'init-polymode)
