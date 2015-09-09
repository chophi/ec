(require-package 'dired+)

;; tried but choose not to
;; (require-package 'dired-k)
;; (define-key dired-mode-map (kbd "K") 'dired-k)

;; tried but choose not to
;; (require-package 'dired-fdclone)
;; (dired-fdclone)

;; tried but choose not to
;; (require-package 'dired-filter)
;; (define-key dired-mode-map (kbd "F") dired-filter-map)

(require-package 'dired-narrow)
(define-key dired-mode-map (kbd "F") 'dired-narrow)

;;; install MacPort, then sudo port install coreutils, and the ls program will
;;; be available
(if *is-mac-machine*
    (setq insert-directory-program "/opt/local/libexec/gnubin/ls"))
(provide 'init-dired)
