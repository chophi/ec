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

(defun get-or-select-buffer-local-terminal (&optional enforce-reselect)
  "Get project local terminal or select one from the existed terminal if no
terminal was selected before or ENFORCE-RESELECT is not nil"
  (interactive "P")
  (when (or (not (boundp 'buffer-local-terminal))
            (not (buffer-live-p buffer-local-terminal))
            enforce-reselect)
    (set (if (not (boundp 'buffer-local-terminal))
             (make-local-variable 'buffer-local-terminal)
           'buffer-local-terminal)
         (let ((buffer-list '()))
           (dolist (term multi-term-buffer-list)
             (add-to-list 'buffer-list (buffer-name term)))
           (get-buffer
            (ido-completing-read "Select a terminal: " buffer-list)))))
  buffer-local-terminal)

(defun _send-command-to-terminal (term command)
  "Send the COMMAND to the TERMINAL"
  (with-current-buffer term
    (term-send-raw-string (concat command "\n"))
    (when (or (not (get-buffer-window term))
              (not (eq (window-frame (get-buffer-window term)) (selected-frame))))
      (switch-to-buffer-other-window (current-buffer))
      (end-of-buffer))))

(defun send-command-to-terminal (&optional enforce-reselect)
  "Send the COMMAND to the TERMINAL, will reselect a terminal if
ENFORCE-RESELECT is not nil"
  (interactive "P")
  (_send-command-to-terminal (get-or-select-project-terminal enforce-reselect)
                             (read-string "Input the Command: ")))

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
              (add-to-list 'compile-exprs (list (car compile-expr)
                                                config
                                                (cadr compile-expr))))
            (push config processed-config)))))
    compile-exprs))

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

        (message "Run: %S" command)
        ;; Run the command.
        (with-current-buffer (get-buffer-create compile-log)
          (cd project-root)
          (case command-type
            ('compile (compile command))
            ('shell (shell-command command))
            ('term
             (_send-command-to-terminal
              (get-or-select-buffer-local-terminal enforce-reselect)
              (format "cd %s &&\\\n %s" project-root command)))
            ('elisp (eval command))
            ('t (message "Unknown command type, exiting")
                (return-from cp-custom-compile nil))))))))

(provide 'init-custom-compile)
