(require-package 'graphviz-dot-mode)
(eval-after-load 'org-mode
  (if (assoc "dot" org-src-lang-modes)
      (setf (cdr (assoc "dot" org-src-lang-modes)) 'graphviz-dot)
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

(defun graphviz-indent-or-complet-word (&optional arg)
  (interactive "P")
  (if (and (not (equal (point) (buffer-end 0)))
           (or (equal (point) (buffer-end 1))
               (equal (buffer-substring-no-properties (point) (1+ (point))) " "))
           (cu-is-alphabet (string-to-char (buffer-substring-no-properties (1- (point)) (point)))))
      (graphviz-dot-complete-word)
    (indent-for-tab-command arg)))

(with-eval-after-load "auto-complete" 
  (add-to-list 'ac-modes 'graphviz-dot-mode))
(provide 'init-graphviz)
