;; This is the framework for customize compilation with emacs lisp.
;; The customize rule files will be either located in ~/.emacs.d/compile-rules
;; or as ${workspace}/compile.el

;; ;; Sample for compile.el in local dir
;; '((unit "compile"
;;         (compile "ndk-build" "jni"))
;;   (unit "run"
;;         (run "adb root")
;;         (run "add remount")
;;         (run "adb push libs/armeabi/test-libstl /data/")
;;         (run "adb shell /data/test-libstl")))
;; ;; Sample compile files in ~/vif/elisp-compile-files
;; '("project-root"
;;   ((unit "compile"
;;          (compile "ndk-build" "jni"))
;;    (unit "run"
;;          (run "adb root")
;;          (run "add remount")
;;          (run "adb push libs/armeabi/test-libstl /data/")
;;          (run "adb shell /data/test-libstl"))))

(defun get-custom-compile-log-buffer (dir &optional switch-to)
  "Get custom compile log buffer for DIR, also switch to the buffer if
SWITCH-TO is not nil"
  (interactive "P")
  (let* ((log-buffer (get-buffer-create
                      (format "*custom-compile [%s]*" (sha1 (or dir ""))))))
    (when switch-to (switch-to-buffer log-buffer))
    log-buffer))

(defvar g-compile-rule-dirlist '("~/vif/elisp-compile-files"
                              "~/.emacs.d/compile-rules/")
  "The directories where the global compile rule files locates.")

(require 'cl-lib)
(defun cp-make-compile-exprs (path)
  "Find the compile configurations and make the compile expressions."
  (interactive "P")
  (setq path (or path default-directory))
  ;;; compile-exprs should be the list of pairs of (project-root . exprs)
  (let ((compile-exprs '())
        (processed-config nil)
        (local-config (cu-find-nearest-ancestor-match path "compile.el")))
    ;; search project local configuration.
    (when local-config
      (push local-config processed-config)
      (add-to-list 'compile-exprs `(,(file-name-directory local-config)
                                    ,local-config
                                    ,(cu-eval-file local-config))))
    ;; find the matched global configuration.
    (dolist (dir (seq-filter 'file-directory-p g-compile-rule-dirlist))
      (dolist (config
               (cl-remove-if-not
                (lambda (str)
                  (and (> (length str) 3) (equal (substring str -3) ".el")))
                (directory-files dir)))
        (setq config (cu-join-path dir config))
        (let ((is-processed-file nil))
          (dolist (file processed-config)
            (if (equal (file-truename (file-chase-links file))
                       (file-truename (file-chase-links config)))
                (setq is-processed-file t)))
          (when (not is-processed-file)
            (dolist (compile-expr (cu-eval-file config))
              (print compile-expr)
              (add-to-list 'compile-exprs (list (car compile-expr)
                                                config
                                                (cadr compile-expr))))
            (push config processed-config)))))
    compile-exprs))

(defun cp-custom-compile-no-rule ()
  (interactive)
  (call-interactively
   (cu-make-commands-map-with-help-msg
    '((?r . smart-compile-run)
      (?c . smart-compile-compile)
      (?q . recompile-quietly)))))

(defun cp-reshape-compile-exprs (compile-exprs)
  "Reshape the compile expressions.
to '((program-name project-root compilation-configuration expression) ...)"
  (let ((ret-exprs '()))
    (dolist (expr compile-exprs)
      (let ((proot (car expr))
            (compile-file (cadr expr))
            (expr (caddr expr)))
        (dolist (e expr)
          (let* ((prog-name (cadr e))
                 (prog-commands (cddr e)))
            (add-to-list 'ret-exprs (list prog-name proot compile-file e))))))
    ret-exprs))

(defun* cp-custom-compile (enforce-reselect)
  (interactive "P")
  (let* ((path default-directory)
         (run-on-file (buffer-file-name))
         (curbuf (current-buffer))
         ;; make reshaped multi level choice list.
         (reshaped-mlcl
          (cu-reshape-multi-level-choice-list*
           (cp-reshape-compile-exprs
            (cp-make-compile-exprs path))
           3))
         ;; choose an item to execute.
         (item-selected
          (if reshaped-mlcl
              (cu-choose-from-reshaped-mlcl
               reshaped-mlcl 3
               '("Command" "Project Root" "Config File"))
            (progn (message "No custom compile rule file applies!")
                   (if (fboundp 'cp-custom-compile-no-rule)
                       (call-interactively 'cp-custom-compile-no-rule)
                     (return-from cp-custom-compile nil)))))
         ;; command to execute
         (command-name (car item-selected))
         ;; project root
         (project-root (cadr item-selected))
         ;; the rule file
         (rule-file (caddr item-selected))
         ;; command sequence to execute.
         (commands (nthcdr 2 (nth 3 item-selected)))
         ;; compile log to print the command executed and to store
         ;; file local variables
         (compile-log (get-custom-compile-log-buffer
                       (concat project-root rule-file) 
                       nil)))
    ;; Execute each command in the selected command sequence.
    (dolist (command commands)
      (let ((command-type (car command))
            (command (cadr command))
            (relative-path (caddr command)))
        (when (not (memq command-type '(compile shell term elisp)))
          (error "Unknown type %S" command-type))
        (when (not (eq command-type 'elisp))
          (setq command (eval command)))
        ;; Insert the executed commands into log buffer.
        (with-current-buffer (get-buffer-create compile-log)
          (end-of-buffer)
          (insert (format "Command Type: %s\n"
                          (case command-type
                            ('compile "Compile")
                            ('run "Run")
                            (t "None"))))
          (insert (format "Command: %S\n" command))
          (insert (format "Relative Path: %s\n\n" relative-path)))
        ;; Insert the relative path to appropriate position if its specified.
        (when (and relative-path (not (equal relative-path "")) (not (eq command-type 'elisp)))
          (setq command (concat "cd " relative-path " && " command)))

        ;; (message "Run: %S" command)
        ;; Run the command.
        (with-current-buffer (get-buffer-create compile-log)
          (setq-local custom-compile-project-root project-root)
          (setq-local custom-compile-run-on-file run-on-file)
          (cd project-root)
          (case command-type
            ('compile (compile command))
            ('shell (shell-command command))
            ('term
             (cu-send-command-to-terminal
              (cu-get-or-select-buffer-local-terminal enforce-reselect)
              (format "cd %s &&\\\n %s" project-root command)))
            ('elisp (let ((in-custom-compile-environment t)
                          (current-custom-compile-log-buffer compile-log))
                      (let ((cp-custom-compile-current-buffer curbuf))
                        (eval command))))
            ('t (message "Unknown command type, exiting")
                (return-from cp-custom-compile nil))))))))

(provide 'init-custom-compile)
