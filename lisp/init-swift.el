(require-package 'swift-mode)
(require-package 'flycheck-swift)
(eval-after-load 'flycheck '(flycheck-swift-setup))

(when *linux?*
  (add-to-list 'exec-path "~/software/swift-dev/bin/")
  (add-to-path "~/software/swift-dev/bin/")
  (setq swift-mode:repl-executable "repl_swift"))

(provide 'init-swift)
