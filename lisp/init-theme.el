(require 'cl)

(defvar global-background-color nil "global background color")
(defvar global-foreground-color nil "global foreground color")

(defun _my-set-or-append (alist key value)
  (if (assoc key alist)
      (setf (cdr (assoc key alist)) value)
    (setq alist (append alist `(,key . ,value)))))

(defun post-select-color-theme ()
  (interactive)
  (when global-background-color
    (set-background-color global-background-color))
  (when global-foreground-color
    (set-foreground-color global-foreground-color))
  ;; hide the extra org mode stars perfectly
  (with-eval-after-load "org-faces"
    (set-face-background 'org-hide global-background-color)
    (set-face-foreground 'org-hide global-background-color))
  (setq initial-frame-alist
        (delete (assoc 'background-color initial-frame-alist)
                initial-frame-alist))
  (add-to-list 'initial-frame-alist
               `(background-color . ,(if global-background-color
                                         global-background-color
                                       (frame-parameter nil 'background-color))))
  (_my-set-or-append default-frame-alist 'background-color
                     (if global-background-color
                         global-background-color
                       (frame-parameter nil 'background-color)))
  (_my-set-or-append default-frame-alist 'foreground-color
                     (if global-foreground-color
                         global-foreground-color
                       (frame-parameter nil 'foreground-color))))

(defun select-color-theme (&optional color-theme)
  (interactive)
  (setq color-theme
        (or color-theme
            (ido-completing-read "Choose a theme: "
                                 '("zenburn" "leuven" "sanityinc-tommorrow" "dracula"))))
  ;; Disable the previous theme
  (when global-use-theme
    (disable-theme (intern global-use-theme)))
  
  (setq global-use-theme color-theme)
  
  (cond ((equal color-theme "sanityinc-tommorrow")
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
                  (load-theme 'leuven t)
         (setq global-background-color "#FFFFFF"
               global-foreground-color "#333333"))
        ((equal color-theme "dracula")
         (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes")
         (load-theme 'dracula t)
         (setq global-background-color "#282a36"
               global-foreground-color "#f8f8f2"))
        ((equal color-theme "paper")
                  ;; It's not necessary to modify these variables, they all have sane
         ;; defaults.
         (setf paper-paper-colour 'paper-parchment ; Custom background.
               paper-tint-factor 45) ; Tint factor for org-level-* faces
         ;; Activate the theme.
         (load-theme 'paper t))
        (t nil))
  (post-select-color-theme))

(defconst global-use-theme nil "theme selection")
(select-color-theme "leuven")

(provide 'init-theme)
