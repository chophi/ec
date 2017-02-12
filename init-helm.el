(require-package 'helm)
(require-package 'ac-helm)

;; use ido instead of helm with following use-ido-list
(with-eval-after-load "helm"
  (let ((use-ido-list
         '(describe-function
           describe-variable
           describe-symbol)))
    (dolist (use-ido use-ido-list)
      (setf (cdr (assoc use-ido helm-completing-read-handlers-alist)) 'ido))))

(require-package 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(provide 'init-helm)
