(defun my-dired-sort-func ()
  (interactive)
  (let (choice choice-switcher-list switcher)
    (setq choice (read-char
                  "Sort Dir By: s[size] x[extension] c[ctime] u[utime] t[time] o[no extra]")
          choice-switcher-list '((?s "S")
                                 (?x "X")
                                 (?c "ct")
                                 (?u "ut")
                                 (?t "t")
                                 (?o ""))
          switcher (cadr (assoc choice choice-switcher-list)))
    (dired-sort-other (concat dired-listing-switches switcher))))

(setq dired-listing-switches (concat "--group-directories-first " dired-listing-switches))
(define-key dired-mode-map (kbd "s") 'my-dired-sort-func)

(provide 'init-sort-dired)
