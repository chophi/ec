(require-package 'egg)
(require 'egg)

;; using egg to manage git
(delete 'Git vc-handled-backends)
;; not use SVN
(delete 'SVN vc-handled-backends)
(provide 'init-egg)
