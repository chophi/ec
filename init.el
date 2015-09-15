(add-to-list 'load-path user-emacs-directory)
(require 'init-system-check)

(require 'init-server)

(require 'init-path)
(require 'init-elpa)
(require 'init-benchmarking)

(require 'init-theme)

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)
(require-package 'regex-tool)

(require 'init-util-functions)
(require 'init-fonts)
(require 'init-locales)
(require 'init-tidy)
(require 'init-ibuffer)
(require 'init-uniquify)
(require 'init-fci)
(require 'init-editing-utils)

(require 'init-recentf)
(require 'init-ido)

(require 'init-sessions)

(require 'init-maxframe)
(require 'init-windows)
(when *is-linux-system-p*
  (maximize-frame "Emacs::IDE"))
(when *is-windows-system-p*
  (w32-maximize-frame))


;; heavy configures follows
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-python)

;; c++ configuration
(require 'init-cc-mode)
(require 'init-smart-compile)
(require 'init-smart-run)


;; (set-background-color "#2E3436")
;; (set-foreground-color "medium spring green")
;; (set-foreground-color "medium sea green")

;; org-mode configuration
(require 'init-org)
(require 'init-org-publish-settings)
;; (require 'init-org-remember)

(require 'init-keybind)
(require 'init-cedet)
(require 'init-cmake)


(require 'init-image-support)

;; (require 'init-w32-symlinks)
;; (require 'init-egg)
;; (require 'init-groovy)
(require 'init-glsl-mode)
;; (require 'init-lua)

(require 'init-magit)
(require 'init-latex)
(require 'init-openwith)
;; (require 'init-cnblogs)
(require 'init-handy)
(require 'init-js)
(require 'init-ruhoh)

;;; deprecated from windows
;; (require 'init-topcoder)

(require 'init-lisps)
(require 'init-rainbow-delimiters)


;; (require 'init-metapost)
(require 'init-pandoc)

(require 'important-parameter-settings)
;; (require 'init-chrome-emacs)

(require 'init-load-snippets)
(require 'init-sort-dired)


(require 'init-dired)
(require 'init-uniform-env)


(require 'init-neotree)
(require 'init-smali)
(require 'init-cc-format)

(require 'init-groovy)

(require 'init-logcat-mode)
(require 'init-markdown-mode)
(require 'init-yml-mode)

(when *is-mac-machine*
    (global-set-key "\C-\M-g" (lambda () (interactive) (shell-command "open \"/Applications/Google Chrome.app/\"")))
    (global-set-key "\C-\M-x" (lambda () (interactive) (shell-command "open \"/Applications/Xcode.app/\"")))
    (define-key emacs-lisp-mode-map "\C-\M-x" nil)
  )


(put 'erase-buffer 'disabled nil)
 

;;;(when (not *is-mac-machine*)
(require 'init-system-default-frame-alist)
;;;)

(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(when (file-exists-p "~/.emacs.d/init-private-custom.el")
  (require 'init-private-custom))

;; hide the extra org mode stars perfectly
(set-face-background 'org-hide (face-background 'default))
(set-face-foreground 'org-hide (face-background 'default))

;; !!!!! put the term code the last !!!!!!!!!!!!
;; import the multi-term function to linux.
(when (or *is-linux-system-p* *is-mac-machine*) 
  (add-to-list 'load-path (concat user-emacs-directory "term"))
  (require 'term-inside-ide-init))
