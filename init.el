;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(defconst user-lisp-directory (concat user-emacs-directory "lisp"))
(add-to-list 'load-path user-lisp-directory)
(setq warning-suppress-types '((initialization)))
(require 'init-elpa)
(require 'init-system-check)

(require 'init-common-utils)
(require 'init-global-settings)

(when (file-exists-p (concat user-lisp-directory "init-private-custom.el"))
  (require 'init-private-custom))

(require 'init-server)

(require 'init-path)
(require 'init-benchmarking)

(require 'init-theme)

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)
(require-package 'regex-tool)

(require 'init-fonts)
(require 'init-locales)
(require 'init-tidy)
(require 'init-ibuffer)
(require 'init-uniquify)
(require 'init-fci)
(require 'init-editing-utils)

(require 'init-recentf)
(require 'init-ido)
;; FIXME: seems no-flet not compatible with other packages.
;; (require 'kill-ring-ido)

(require 'init-sessions)

(require 'init-maxframe)
(require 'init-windows)
(when *linux?*
  (maximize-frame "Emacs::IDE"))
(when *windows?*
  (w32-maximize-frame))


;; heavy configures follows
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-python)
;;(require 'init-helm)
;;(require 'init-anything)
(require 'init-ruby)

;; c++ configuration
(require 'init-cc-mode)
(require 'init-smart-compile)
(require 'init-smart-run)


;; org-mode configuration
(require 'init-org)
(require 'init-org-publish-settings)
;; (require 'init-org-remember)

(require 'init-cedet)
(require 'init-cmake)


(require 'init-image-support)

;; (require 'init-w32-symlinks)
(require 'init-glsl-mode)
(require 'init-lua)

(require 'init-magit)
(require 'init-latex)
(require 'init-openwith)
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

(require 'init-logcat-mode)
(require 'init-markdown-mode)
(require 'init-yml-mode)

(require 'init-browse-kill-ring)

(put 'erase-buffer 'disabled nil)
 
(load-file custom-file)

;;;(when (not *mac?*)
(require 'init-system-default-frame-alist)
;;;)

;; !!!!! put the term code the last !!!!!!!!!!!!
;; import the multi-term function to linux.
(when (or *linux?* *mac?*) 
  (add-to-list 'load-path (concat user-emacs-directory "term"))
  (require 'term-inside-ide-init))

;; (when *linux?*
;;   (require 'init-ibus))

(require 'init-dmesg-mode)
(require 'init-dictionary)

(when (or *mac?* *linux?*)
  (setq shell-file-name "/bin/bash"))

(require 'init-work-with-repo)
(require 'init-grok)

(require 'init-ediff-binary)

(when (and *amazon-machine?* *linux?*)
  (add-to-list 'exec-path "/usr/share-2/bin")
  (require 'init-amazon-linux))


(require 'init-patch-buffer)
(require 'init-json)
(require 'init-confluence)
(require 'init-protobuf)

(require 'init-applescript-mode)
(require 'init-custom-compile)
(require 'init-preview-org)
(require 'init-flycheck)
(require 'init-swift)
(require 'init-polymode)
(require 'init-adaptive-wrap)
(require 'init-keybind)
