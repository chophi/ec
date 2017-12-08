
(add-to-list 'load-path "~/.emacs.d/site-lisp/neotree")
(require 'neotree)

(cu-set-key-bindings global-map "\C-c\C-n" '((?t . neotree-toggle)))
(provide 'init-neotree)
