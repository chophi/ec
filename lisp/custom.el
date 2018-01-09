(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4427a3246e50bedf4041ab92cd89cc18198220e98e3e8646dde3dc317a97dc53" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#515151")
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (ido-gnus adaptive-wrap polymode swift-mode google-translate yapfify flycheck-swift helm-flycheck exec-path-from-shell flycheck yari yaml-mode wgrep visual-regexp unfill undo-tree tabbar switch-window smex skewer-mode session scratch ruby-electric rsense rinari regex-tool project-local-variables pointback org-fstree openwith noflet mwe-log-commands multi-term maxframe markdown-mode+ json-mode jedi idomenu ido-ubiquitous helm-pydoc glsl-mode ggtags fill-column-indicator expand-region eopengrok elpy edit-server-htmlize dired-narrow dired+ diminish cpputils-cmake confluence color-theme-sanityinc-tomorrow cmake-mode clang-format browse-kill-ring apples-mode anything ace-jump-mode ac-inf-ruby ac-helm)))
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")
     (eval let*
           ((x
             (dir-locals-find-file default-directory))
            (this-directory
             (if
                 (listp x)
                 (car x)
               (file-name-directory x))))
           (unless
               (featurep
                (quote swift-project-settings))
             (add-to-list
              (quote load-path)
              (concat this-directory "utils")
              :append)
             (let
                 ((swift-project-directory this-directory))
               (require
                (quote swift-project-settings))))
           (set
            (make-local-variable
             (quote swift-project-directory))
            this-directory))
     (indent-tabs-mode . true)
     (eval progn
           (c-set-offset
            (quote innamespace)
            (quote 0))
           (c-set-offset
            (quote inline-open)
            (quote 0)))
     (require-final-newline))))
 '(session-use-package t nil (session))
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
