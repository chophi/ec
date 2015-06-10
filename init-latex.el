(require-package 'auctex)
(add-hook
 'LaTeX-mode-hook
 (lambda()
   (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
   (add-to-list 'TeX-command-list '("XeLaTeX(minted)" "%`xelatex%(mode) -shell-escape %' %t" TeX-run-TeX nil t))
   (setq TeX-command-default "XeLaTeX(minted)")
   (setq TeX-save-querynil )
   (setq TeX-show-compilation t)
   ))
(provide 'init-latex)
