
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

(defmacro with-parameters-bounded (parameters &rest body)
  `(let ((all-para-bounded t))
     (dolist (p ,parameters)
       (when (not (boundp p))
         (message
          "%s is not bounded, please define it in init-private-custom"
          (symbol-name p))
         (setq all-para-bounded nil)))
     (when all-para-bounded
       ,@body)))

(require 'init-elpa)

(require 'init-common-utils)
(require 'init-theme)

(require 'init-server)
(require 'init-path)

;; For editing the grep buffer and save the edited contents back.
(require-package 'wgrep)
(setq wgrep-change-readonly-file t)
;; brew install the_silver_searcher
;; sudo apt-get install silversearcher-ag
(require-package 'ag)
(require-package 'wgrep-ag)


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
(require 'init-spell-check)
(require 'init-google-translate)

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

(require 'init-glsl-mode)
(require 'init-lua)

(require 'init-magit)
(require 'init-latex)
(require 'init-openwith)
(require 'init-handy)
(require 'init-binary-coding)
(require 'init-js)

(require 'init-lisp-and-scheme)
(require 'init-rainbow-delimiters)

(require 'init-pandoc)

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

(when (memq os '(linux macos))
  (setq shell-file-name "/bin/bash"))

(require 'init-work-with-repo)
(require 'init-grok)

(require 'init-ediff-binary)

(require 'init-json)
(require 'init-org-to-confluence-wiki)
(require 'init-protobuf)

(require 'init-applescript-mode)
(require 'init-custom-compile)
(require 'init-flycheck)
(require 'init-swift)
(require 'init-polymode)
(require 'init-adaptive-wrap)
(require 'init-sepolicy)
(require 'init-ido-gnus)
(require 'init-send-mail)
(when (cu-program-exists-p "notmuch")
  (require 'init-notmuch))
(require 'init-nanoc)
(require 'init-cobalt-dev)
(require 'init-gn-mode)
(require 'init-frame-utils)
(require 'init-projectile)
(require 'init-graphviz)
(require 'init-plantuml)
(require 'init-tikz)
(require 'init-leetcode-cli)
(require 'init-grok-daemon)
(require 'init-powerline)
(require 'init-keybind)
(setq sanityinc/require-times
      (sort sanityinc/require-times (lambda (a b) (> (cdr a) (cdr b)))))

(provide 'init-emacs)
