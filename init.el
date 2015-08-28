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
(if *is-linux-system-p*
    (maximize-frame "Emacs::IDE")
  (w32-maximize-frame))

;; heavy configures follows
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-python)

;; c++ configuration
(require 'init-cc-mode)
(require 'init-smart-compile)
(require 'init-smart-run)


(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)
(set-background-color "#2E3436")
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
(require 'init-cnblogs)
(require 'init-handy)
(require 'init-js)
(require 'init-ruhoh)


(require 'init-topcoder)

(require 'init-lisps)
(require 'init-rainbow-delimiters)

(when *is-amazon-machine*)


;; (require 'init-metapost)
(require 'init-pandoc)

(require 'important-parameter-settings)
(require 'init-chrome-emacs)

(require 'init-load-snippets)
(require 'init-sort-dired)


(require 'init-dired)
(require 'init-uniform-env)
(require 'init-system-default-frame-alist)

(require 'init-neotree)
(require 'init-smali)
(require 'init-cc-format)
;; !!!!! put the term code the last !!!!!!!!!!!!
;; import the multi-term function to linux.
(when *is-linux-system-p*
  (add-to-list 'load-path (concat user-emacs-directory "term"))
  (require 'term-inside-ide-init))


(put 'erase-buffer 'disabled nil)
