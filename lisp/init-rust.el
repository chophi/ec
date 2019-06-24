(require-package 'rust-mode)
(require-package 'flycheck-rust)
(require-package 'cargo)
(require-package 'ob-rust)
(require-package 'rust-playground)
(custom-set-variables '(rust-playground-basedir "~/rust-playground"))

(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook
          '(lambda ()
             (let ((poss-rust-bin-dirs
                    '("~/.rustup/toolchains/stable-x86_64-apple-darwin/bin")))
               (dolist (dir poss-rust-bin-dirs)
                 (when (file-directory-p dir)
                   (add-to-path dir))))))
(add-hook 'rust-mode-hook 'cu-set-skeleton-pair-indent t)
(provide 'init-rust)
