;;; (require-package 'color-theme-sanityinc-tomorrow)
;;; (color-theme-sanityinc-tomorrow-night)

;; Not necessary, but silences flycheck errors for referencing free
;; variables.
(require-package 'paper-theme)
;; It's not necessary to modify these variables, they all have sane
;; defaults.
(setf paper-paper-colour 'paper-parchment ; Custom background.
      paper-tint-factor 45)      ; Tint factor for org-level-* faces
;; Activate the theme.
(load-theme 'paper t)


(provide 'init-theme)
