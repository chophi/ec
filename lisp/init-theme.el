(defconst global-use-theme
  (case os
    ('macos "zenburn")
    ('linux "zenburn")
    (t "sanityinc-tommorrow"))
  "theme selection")

(defvar global-background-color nil "global background color")
(defvar global-foreground-color nil "global foreground color")

(cond ((equal global-use-theme "sanityinc-tommorrow")
       (require 'custom)
       (require-package 'color-theme-sanityinc-tomorrow)
       (load-theme 'sanityinc-tomorrow-eighties t)
       (setq global-background-color "#2D2D2D"
             global-foreground-color "#CCCCCC"))
      ((equal global-use-theme "zenburn")
       (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes")
       (load-theme 'zenburn t)
       (setq global-background-color
             (cdr (assoc "zenburn-bg" zenburn-default-colors-alist))
             global-foreground-color
             (cdr (assoc "zenburn-fg" zenburn-default-colors-alist))))
      ((equal global-use-theme "paper")
       (require-package 'paper-theme)
       ;; It's not necessary to modify these variables, they all have sane
       ;; defaults.
       (setf paper-paper-colour 'paper-parchment ; Custom background.
             paper-tint-factor 45) ; Tint factor for org-level-* faces
       ;; Activate the theme.
       (load-theme 'paper t))
      (t nil))


(setq ansi-color-faces-vector
      [default bold shadow italic underline bold bold-italic bold]
      ansi-color-names-vector
      ["#c5c8c6" "#cc6666" "#b5bd68" "#f0c674"
       "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"])

(when global-background-color
  (set-background-color global-background-color))
(when global-foreground-color
  (set-foreground-color global-foreground-color))

(provide 'init-theme)
