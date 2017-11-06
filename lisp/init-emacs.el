
(require 'init-benchmarking)
(require 'init-computer-check)

;; load the private custom file, which is in the gitignore list,
;; and is basically for toggle some features on different desktops/laptops.
(defun require-if-exist (feature)
  "Search FEATURE in the `user-emacs-directory'/lisp folder, load it if exist."
  (when (file-exists-p
         (format "%s/lisp/%s.el" user-emacs-directory
                 (symbol-name feature)))
    (require feature)))
(require-if-exist 'init-private-custom)

(require 'init-elpa)

(require 'init-common-utils)
(require 'init-theme)

(require 'init-server)
(require 'init-path)

;; For editing the grep buffer and save the edited contents back.
(require-package 'wgrep)
;; Logging the key stroke and the mapped commands.
(require-package 'mwe-log-commands)
;; regex testing: show the content, regex and matched text alive in three buffers.
(require-package 'regex-tool)

(require 'init-fonts)
(require 'init-locales)
(require 'init-ibuffer)
(require 'init-uniquify)
(require 'init-fill-column-indicator)
(require 'init-editing-utils)

(require 'init-recentf)
(require 'init-ido)

(require 'init-sessions)

(require 'init-maxframe)
(require 'init-windows)

(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-python)
(require 'init-ruby)

;; c++ configuration
(require 'init-cc-mode)
(require 'init-cc-misc-support)
(require 'init-cmake)
(require 'init-smart-compile)
(require 'init-run-c-progam)

;; org-mode configuration
(require 'init-org)
(require 'init-org-export-settings)
(require 'init-org-publish-settings)

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
(when (memq os '(linux macos))
  (add-to-list 'load-path (concat user-emacs-directory "term"))
  (require 'term-inside-ide-init))

;; (when (eq os 'linux)
;;   (require 'init-ibus))

(require 'init-dmesg-mode)
(require 'init-dictionary)

(when (memq os '(linux macos))
  (setq shell-file-name "/bin/bash"))

(require 'init-work-with-repo)
(require 'init-grok)

(require 'init-ediff-binary)

(when (and (company-computer-p) (eq os 'linux))
  (add-to-list 'exec-path "/usr/share-2/bin")
  (require 'init-amazon-linux))


(require 'init-patch-buffer)
(require 'init-json)
(require 'init-confluence)
(require 'init-protobuf)

(require 'init-applescript-mode)
(require 'init-custom-compile)
(require 'init-flycheck)
(require 'init-swift)
(require 'init-polymode)
(require 'init-adaptive-wrap)
(require 'init-keybind)

(setq sanityinc/require-times
      (sort sanityinc/require-times (lambda (a b) (> (cdr a) (cdr b)))))

(provide 'init-emacs)
