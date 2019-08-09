(require 'cl)
(defconst global-use-theme
  (cond
   ((company-computer-p) "leuven")
   ((equal os 'macos) "leuven")
   ((equal os 'linux) "zenburn")
   (t "sanityinc-tommorrow"))
  "theme selection")

(defvar global-background-color nil "global background color")
(defvar global-foreground-color nil "global foreground color")

(defun choose-color-theme (&optional color-theme)
  (interactive)
  (setq color-theme
        (or color-theme
            (ido-completing-read "Choose a theme: "
                                 '("zenburn" "leuven" "sanityinc-tommorrow" "dracula"))))
  (cond ((equal color-theme "sanityinc-tommorrow")
         (require-package 'color-theme-sanityinc-tomorrow)
         (load-theme 'sanityinc-tomorrow-eighties t)
         (setq global-background-color "#2D2D2D"
               global-foreground-color "#CCCCCC")
         (setq ansi-color-faces-vector
               [default bold shadow italic underline bold bold-italic bold]
               ansi-color-names-vector
               ["#c5c8c6" "#cc6666" "#b5bd68" "#f0c674"
                "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"]))
        ((equal color-theme "zenburn")
         (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes")
         (load-theme 'zenburn t)
         (setq global-background-color
               (cdr (assoc "zenburn-bg" zenburn-default-colors-alist))
               global-foreground-color
               (cdr (assoc "zenburn-fg" zenburn-default-colors-alist)))
         (with-eval-after-load "isearch"
           (set-face-background 'isearch "yellow")
           (set-face-foreground 'isearch "black")
           (set-face-background 'lazy-highlight "black")
           (set-face-foreground 'lazy-highlight "yellow")
           (custom-set-faces '(isearch-fail
                               ((((class color))
                                 (:background "green")
                                 (:foreground "black")))))))
        ((equal color-theme "leuven")
         (require-package 'leuven-theme)
         (load-theme 'leuven t)
         (setq global-background-color "#FFFFFF"
               global-foreground-color "#333333"))
        ((equal color-theme "dracula")
         (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes")
         (load-theme 'dracula t)
         (setq global-background-color "#282a36"
               global-foreground-color "#f8f8f2"))
        ((equal global-use-theme "paper")
         (require-package 'paper-theme)
         ;; It's not necessary to modify these variables, they all have sane
         ;; defaults.
         (setf paper-paper-colour 'paper-parchment ; Custom background.
               paper-tint-factor 45) ; Tint factor for org-level-* faces
         ;; Activate the theme.
         (load-theme 'paper t))
        (t nil)))

(choose-color-theme global-use-theme)

(when global-background-color
  (set-background-color global-background-color))
(when global-foreground-color
  (set-foreground-color global-foreground-color))

(provide 'init-theme)
