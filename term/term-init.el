(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "term"))

(require 'init-system-check)

(require 'init-elpa)
(require 'init-fonts)
(require 'init-ido)
;; (require 'init-dired)
;; (require 'init-gui-frames)
(require 'init-handy)
(setq desktop-base-file-name ".emacs.term.desktop"
      desktop-dirname "~/.emacs.d"
      desktop-base-lock-name ".emacs.term.desktop.lock")
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-lisps)
(require 'init-keybind)
;; (require 'init-lisp)

(require 'init-multi-term)
(require 'init-tabbar)

(require 'init-term-keys)

;; this should before init-term-face
(load "~/.emacs.d/custom.el")
(require 'init-term-face)

(require 'init-tidy)
(desktop-read "~/.emacs.d")
(multi-term)

(require 'init-maxframe)
(require 'init-windows)
(if (eq os 'linux)
    (maximize-frame "Emacs::Terminal")
  (w32-maximize-frame))
