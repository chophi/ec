(require 'multi-term)

;; (set-face-attribute 'term-color-black nil :foreground "#2b2b2b")
;; (set-face-attribute 'term-color-red nil :foreground "#fa8072")
;; (set-face-attribute 'term-color-green nil :foreground "#7ccd7c")
;; (set-face-attribute 'term-color-yellow nil :foreground "#cae684")
;; (set-face-attribute 'term-color-blue nil :foreground "#87ceeb")
;; (set-face-attribute 'term-color-magenta nil :foreground "#ee799f")
;; (set-face-attribute 'term-color-cyan nil :foreground "#008b8b")
;; (set-face-attribute 'term-color-white nil :foreground "#e6e6fa")


(defun my-term-mode-set-buffer-local-face ()
  (when (x-list-fonts "Monaco for Powerline")
    (defface temp-buffer-local-term-mode-face
      '((t
         :family "Monaco for Powerline"
         :size 16))
      "Temporary buffer local face for term mode")
    (buffer-face-set 'temp-buffer-local-term-mode-face)))

(add-hook 'term-mode-hook 'my-term-mode-change-font-hook)

(provide 'init-term-face)
