(progn
  (defun my-cargo-run(&optional enforce-select)
    (interactive "P")
    (let* ((root (cargo-process--project-root))
           (toml-file (cu-join-path root "Cargo.toml")))
      (with-current-buffer (find-file-noselect toml-file)
        (cu-send-command-to-buffer-local-terminal
         (format "%s run --manifest-path %s"
                 cargo-process--custom-path-to-bin
                 toml-file)))))
  (append
   ;; For graphviz-dot-mode
   (cu-make-mode-specific-custom-compile-rule-map
    'rust-mode
    t
    '((?e . cargo-process-bench)
      (?b . cargo-process-build)
      (?l . cargo-process-clean)
      (?d . cargo-process-doc)
      (?v . cargo-process-doc-open)
      (?n . cargo-process-new)
      (?i . cargo-process-init)
      (?r . cargo-process-run)
      (?R . my-cargo-run)
      (?x . cargo-process-run-example)
      (?s . cargo-process-search)
      (?t . cargo-process-test)
      (?u . cargo-process-update)
      (?c . cargo-process-repeat)
      (?f . cargo-process-current-test)
      (?o . cargo-process-current-file-tests)
      (?m . cargo-process-fmt)
      (?k . cargo-process-check)
      (?K . cargo-process-clippy)
      (?a . cargo-process-add)
      (?D . cargo-process-rm)
      (?U . cargo-process-upgrade)))
   nil
   ) ;; append
  )

