;; Not necessary, but silences flycheck errors for referencing free
;; variables.
(when (equal use-theme "paper")
  (require-package 'paper-theme)
  ;; It's not necessary to modify these variables, they all have sane
  ;; defaults.
  (setf paper-paper-colour 'paper-parchment ; Custom background.
        paper-tint-factor 45)      ; Tint factor for org-level-* faces
  ;; Activate the theme.
  (load-theme 'paper t))

(when (equal use-theme "sanityinc-tommorrow")
  (require 'custom)
  (require-package 'color-theme-sanityinc-tomorrow)
  ;;(color-theme-sanityinc-tomorrow-night)
  )

(when global-background-color
  (set-background-color global-background-color))
(when global-foreground-color
  (set-foreground-color global-foreground-color))

(provide 'init-theme)
