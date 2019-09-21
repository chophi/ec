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
(define-key rust-mode-map (kbd ";") 'cu-insert-semicolon)

(defun my-rust-playground-dir-name (&optional snippet-name)
  (let ((choice
         (ido-completing-read
          "Choose snippet: "
          (append (cu-list-files-recursively-general
                   rust-playground-basedir (lambda (entry) t) 1 t)
                  `(,(time-stamp-string "new.at-%:y-%02m-%02d-%02H%02M%02S"))))))
    (concat
     (cu-join-path rust-playground-basedir
                   (if (equal (string-match "new.*" choice) 0)
                       (substring choice 4)
                     choice))
     "/")))

(defun my-rust-playground ()
  "Run playground for Rust language in a new buffer."
  (interactive)
  ;; get the dir name
  (let* ((snippet-dir (my-rust-playground-dir-name))
         (snippet-file-name (rust-playground-snippet-main-file-name snippet-dir))
         (snippet-cargo-toml (rust-playground-toml-file-name snippet-dir)))
    (if (file-exists-p snippet-file-name)
        (progn
          (find-file snippet-file-name)
          (rust-playground-mode))
      ;; create a buffer for Cargo.toml and switch to it
      (make-directory snippet-dir t)
      (set-buffer (create-file-buffer snippet-cargo-toml))
      (set-visited-file-name snippet-cargo-toml t)
      (rust-playground-mode)
      (rust-playground-insert-template-head "snippet of code" snippet-dir)
      (insert rust-playground-cargo-toml-template)
      (save-buffer)
      ;;now do src/main.rs
      (make-directory (concat snippet-dir "src"))
      (switch-to-buffer (create-file-buffer snippet-file-name))
      (set-visited-file-name snippet-file-name t)
      (rust-playground-insert-template-head "snippet of code" snippet-dir)
      (insert rust-playground-main-rs-template)
      ;; back up to a good place to edit from
      (backward-char 27)
      (rust-playground-mode))))

(with-eval-after-load "rust-playground"
  (fset 'rust-playground (symbol-function 'my-rust-playground)))

(provide 'init-rust)
