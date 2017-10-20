(require 'multi-term)

;; I used x-list-fonts to check if "Monaco for Powerline" exists
;; but x-list-fonts only exists if (window-system) return non-nil
;; TODO: Use powerline in terminal, the face itself might be also in terminal
;; even (window-system) is nil
(when (window-system)

  (defun set-term-mode-face ()
    "Set face for Term mode"
    (defface term-mode-face
      '((t :family "Monaco for Powerline"
           :size 16))
      "Buffer local face for term mode")
    (buffer-face-set 'term-mode-face))

  (add-hook 'term-mode-hook 'set-term-mode-face))

(provide 'init-term-face)
