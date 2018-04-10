(require-package 'multi-term)

;; the cd function provide will cause some delay when
;; change directory to a directory which has a multi-byte name.
;; I just make it empty to solve this problem, it's a lazy solution.
(when (and  (not (company-computer-p)) (not (eq os 'macos)))
    (defun cd(dir)      
      )
    )

(setq multi-term-program "/bin/bash")

(when (and (eq os 'macos) (not (company-computer-p)))
  (when (file-exists-p "/usr/local/bin/bash")
    (setq multi-term-program "/usr/local/bin/bash")
    )
  )

(provide 'init-multi-term)
