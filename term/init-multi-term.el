(require-package 'multi-term)

;; the cd function provide will cause some delay when
;; change directory to a directory which has a multi-byte name.
;; I just make it empty to solve this problem, it's a lazy solution.
(when (and  (not (company-computer-p)) (not (eq os 'macos)))
    (defun cd(dir)      
      )
    )

(cond ((and shell-file-name (file-exists-p shell-file-name))
       (setq multi-term-program shell-file-name)))

(provide 'init-multi-term)
