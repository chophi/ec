(org-remember-insinuate)                

(setq org-directory "~/journal/")
(setq org-default-notes-file 
      (concat org-directory (concat (format-time-string "%Y-%m") ".org")))
(define-key global-map "\C-cr" 'org-remember)

(provide 'init-org-remember)
