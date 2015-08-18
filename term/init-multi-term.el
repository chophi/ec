(require-package 'multi-term)

;; the cd function provide will cause some delay when
;; change directory to a directory which has a multi-byte name.
;; I just make it empty to solve this problem, it's a lazy solution.
(when (not *is-amazon-linux*)
    (defun cd(dir)      
      )
    )

;;; set C-c C-k to term-toggle-between-mode
(defun term-toggle-between-modes()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(setq multi-term-program "/bin/bash")

(provide 'init-multi-term)
