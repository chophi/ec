(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-color "#515151")
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (poly-org dap-mode lsp-ui company-lsp use-package lsp-java modern-cpp-font-lock ensime flycheck-rust rust-mode go-autocomplete go-guru go-imports go-mode go-projectile go-rename go-snippets groovy-mode flycheck-gradle gradle-mode ob-kotlin flycheck-kotlin kotlin-mode rainbow-mode helm-ag flycheck-plantuml plantuml-mode graphviz-dot-mode ag helm-notmuch notmuch ido-gnus flycheck-swift helm-flycheck exec-path-from-shell flycheck undo-tree scratch project-local-variables pointback org-fstree openwith noflet mwe-log-commands multi-term maxframe markdown-mode+ jedi idomenu ido-ubiquitous helm-pydoc glsl-mode ggtags fill-column-indicator expand-region eopengrok elpy edit-server-htmlize dired-narrow dired+ diminish cpputils-cmake confluence color-theme-sanityinc-tomorrow cmake-mode clang-format browse-kill-ring apples-mode anything ace-jump-mode ac-inf-ruby ac-helm)))
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
