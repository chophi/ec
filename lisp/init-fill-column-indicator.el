;; ;; wired character show-ed in buffers opened by org-open-at-point when
;; ;; I set the fci-rule-character to ?│, below is the workaround to erase them
;; ;; by re-turn on the fci mode if it's enabled in the buffer.
;; (with-eval-after-load "org"
;;   (defadvice org-open-at-point (around fci-mode-work-around)
;;     ad-do-it
;;     (when (and (boundp fci-mode) fci-mode)
;;       (turn-on-fci-mode)))
;;   (ad-activate 'org-open-at-point))

(provide 'init-fill-column-indicator)
