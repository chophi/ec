(setq global-background-color "#2D2D2D"
      global-foreground-color "#CCCCCC"
      ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]
      ansi-color-names-vector (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))

(setq global-use-theme "sanityinc-tommorrow")

(when (equal global-use-theme "paper")
  (require-package 'paper-theme)
  ;; It's not necessary to modify these variables, they all have sane
  ;; defaults.
  (setf paper-paper-colour 'paper-parchment ; Custom background.
        paper-tint-factor 45) ; Tint factor for org-level-* faces
  ;; Activate the theme.
  (load-theme 'paper t))

(when (equal global-use-theme "sanityinc-tommorrow")
  (require 'custom)
  (require-package 'color-theme-sanityinc-tomorrow))

(when global-background-color
  (set-background-color global-background-color))
(when global-foreground-color
  (set-foreground-color global-foreground-color))

(provide 'init-theme)
