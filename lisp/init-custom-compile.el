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

(require 'init-handy)

(defun* _find-file-or-dir-recursively (path name)
  (when (or (equal path "/") (equal path nil)) (return-from _find-file-or-dir-recursively nil))
  (let ((possible-name (concat path name)))
    (if (file-exists-p possible-name)
        (progn
          (message "find %s as %s\n" name possible-name)
          (return-from _find-file-or-dir-recursively possible-name))
      (_find-file-or-dir-recursively (file-name-directory (directory-file-name path)) name))))

(defun find-custom-compile-file (path)
  (interactive "P")
  (when (not path) (setq path default-directory))
  (when (and (cu-is-dir-or-dirlink? path) (not (equal (substring path (1- (length path))) "/")))
    (setq path (concat path "/")))
  (setq path (file-name-directory (expand-file-name path)))
  (_find-file-or-dir-recursively path "compile.el"))

(defun choose-buffer-local-terminal (to-select)
  (interactive "P")
  (when (or (not (boundp 'choosed-terminal)) (not (buffer-live-p choosed-terminal)) to-select)
    (set (if (not (boundp 'choosed-terminal))
             (make-local-variable 'choosed-terminal)
           'choosed-terminal)
         (let ((buffer-list '()))
           (dolist (term multi-term-buffer-list)
             (add-to-list 'buffer-list (buffer-name term)))
           (get-buffer (ido-completing-read "Choose terminal to execute: " buffer-list)))))
  choosed-terminal
  )

(defun send-command-to-terminal (term command)
  (with-current-buffer term
    (term-send-raw-string (concat command "\n"))
    (when (or (not (get-buffer-window term))
              (not (eq (window-frame (get-buffer-window term)) (selected-frame))))
      (switch-to-buffer-other-window (current-buffer))
      (end-of-buffer))))

(defun unset-buffer-local-terminal ()
  (interactive)
  (when (boundp 'choosed-terminal)
    (setq choosed-terminal nil)))

(defun cp-get-custom-compile-log (dir &optional not-switch-to)
  (interactive "P")
  (let ((compile-log-name
         (format "*custom-compile-log[%s]*"
                 (let ((str dir))
                   (sha1 (if str str ""))))))
    (if (not not-switch-to)
        (if (get-buffer compile-log-name)
            (switch-to-buffer (get-buffer compile-log-name))
          (error "the buffer wasn't existed %s" compile-log-name))
      compile-log-name)))

(defvar *global-compile-directory-list*
  '("~/vif/elisp-compile-files"
    "~/.emacs.d/compile-rules/"))

(require 'cl-lib)
(defun cp-make-compile-exprs (path)
  (interactive "P")
  (when (not path) (setq path default-directory))
  ;;; compile-exprs should be the list of pairs of (project-root . exprs)
  (let (compile-exprs
        (processed-config nil))
    (setq compile-exprs '())
    (let ((compile-config (find-custom-compile-file path)))
      ;; (message "find compile-config: %s\n" compile-config)
      (when compile-config
        (push compile-config processed-config)
        (add-to-list 'compile-exprs `(,(file-name-directory compile-config)
                                      ,compile-config
                                      ,(eval-file-as-lisp-expression compile-config))))
      )
    (dolist (global-compile-dir *global-compile-directory-list*)
      (when (file-exists-p global-compile-dir)
        (dolist (compile-file
                 (cl-remove-if-not
                  (lambda (str)
                    (and (> (length str) 3) (equal (substring str (- (length str) 3) (length str)) ".el")))
                  (directory-files global-compile-dir)))
          (let ((compile-config (concat global-compile-dir "/" compile-file))
                (is-processed-file nil))
            (dolist (file processed-config)
              (if (equal (file-truename (file-chase-links file))
                         (file-truename (file-chase-links compile-config)))
                  (setq is-processed-file t)))
            (when (not is-processed-file)
              (dolist (compile-expr (eval-file-as-lisp-expression compile-config))
                (add-to-list 'compile-exprs (list (car compile-expr)
                                                  compile-config
                                                  (cadr compile-expr)
                                                  )))
              (push compile-config processed-config))))))
    compile-exprs))

(defun cp-process-duplicate-and-reshape (compile-exprs)
  (let (ret-exprs)
    (setq ret-exprs '())
    (dolist (expr compile-exprs)
      (let ((proot (car expr))
            (compile-file (cadr expr))
            (expr (caddr expr)))
        (dolist (e expr)
          (let* ((prog-name (cadr e))
                 (prog-commands (cddr e)))
            (add-to-list 'ret-exprs (list prog-name proot compile-file e))))
        ))
    ret-exprs))

(defun* _previous-n-all-equal (l1 l2 n)
  (when (or (< (length l1) n) (< (length l2) n))
    (return-from _previous-n-all-equal nil))
  (setq n (1- n))
  (while (>= n 0)
    (when (not (equal (nth n l1) (nth n l2)))
      (return-from _previous-n-all-equal nil))
    (setq n (1- n)))
  t)

(defun* _reshape-ndim-list (li ndim)
  (when (or (not li) (not (listp li)))
    (return-from _reshape-ndim-list nil))

  (let* ((len (length li))
         index
         dim
         temp-li
         count)

    ;;; sort from dim (ndim-2 to 0)
    (setq dim (- ndim 2))
    (while (>= dim 0)
      (setq li (sort li
                     (lambda (ele1 ele2)
                       (string-lessp (nth dim ele1) (nth dim ele2)))))
      (setq dim (1- dim)))

    ;; (message "after sort")
    ;; (print li)

    ;;; remove duplicate
    (setq dim (1- ndim)
          index 1)
    (setq temp-li (list (car li)))
    (while (< index len)
      (when (not (_previous-n-all-equal (nth index li) (nth (1- index) li) dim))
        (push (nth index li) temp-li))
      (setq index (1+ index)))
    (setq li (reverse temp-li))

    ;; (message "after remove duplicate")
    ;; (print li)

    ;;; pack
    (setq dim (- ndim 2))
    (while (>= dim 1)
      (setq index 1
            temp-li (list (append (subseq (car li) 0 dim)
                                  (list (list (subseq (car li) dim)))))
            len (length li))
      (while (< index len)
        (if (_previous-n-all-equal (nth index li) (nth (1- index) li) dim)
            (push (subseq (nth index li) dim) (nth dim (nth 0 temp-li)))
          (progn
            (push (append (subseq (nth index li) 0 dim)
                          (list (list (subseq (nth index li) dim))))
                  temp-li)))
        (setq index (1+ index)))

      (setq li (reverse temp-li))

      ;; (message "after pack %d" dim)
      ;; (print li)

      (setq dim (1- dim))
      )
    li
    ))

(defun _choose_from_reshaped_ndim_list (li ndim &optional desc_list)
  (let (choice
        (temp-li li)
        (temp-choosed-li nil)
        (ret nil)
        (level 0))
    (setq choice (ido-completing-read
                  (if desc_list
                      (format "Choose %s (%dth level key): " (nth level desc_list) level)
                    (format "Choose Level %d Key: " level))
                  (mapcar 'car temp-li))
          temp-choosed-li (assoc choice temp-li)
          temp-li (cadr temp-choosed-li)
          ret (cons choice ret))
    (setq level 1)
    (while (< level (1- ndim))
      (setq choice
            (if (> (length temp-li) 1)
                (ido-completing-read
                 (if desc_list
                     (format "Choose %s (%dth level key): " (nth level desc_list) level)
                   (format "Choose Level %d Key: " level))
                 (mapcar 'car temp-li))
              (caar temp-li))
            temp-choosed-li (assoc choice temp-li)
            temp-li (cadr temp-choosed-li)
            ret (cons choice ret))
      (setq level (1+ level)))
    (setq ret (cons (cadr temp-choosed-li) ret))
    (reverse ret)))

(defun* cp-custom-compile (to-select-terminal)
  (interactive "P")
  (let* ((path default-directory)
         (compile-log (cp-get-custom-compile-log default-directory t))
         (choice-arr
          (_reshape-ndim-list
           (cp-process-duplicate-and-reshape
            (cp-make-compile-exprs path))
           4))
         (choosed-command
          (if choice-arr
              (_choose_from_reshaped_ndim_list
               choice-arr
               4
               '("Command Name" "Project Root" "Lisp Compile File"))
            (progn (message "no compile file found!")
                   (return-from cp-custom-compile nil))))
         (command-name (car choosed-command))
         (project-root (cadr choosed-command))
         (lisp-compile-file (caddr choosed-command))
         (commands (nthcdr 2 (nth 3 choosed-command))))

    (dolist (command commands)
      (let ((command-type (car command))
            (command-string (eval (cadr command)))
            (relative-path (caddr command)))

        (with-current-buffer (get-buffer-create compile-log)
          (end-of-buffer)
          (insert (format "Command Type: %s\n"
                          (case command-type
                            ('compile "Compile")
                            ('run "Run")
                            (t "None"))))
          (insert (format "Command: %s\n" command-string))
          (insert (format "Relative Path: %s\n\n" relative-path)))

        (when (and relative-path (not (equal relative-path "")))
          (setq command-string (concat "cd " relative-path " && " command-string)))

        (with-current-buffer (get-buffer-create compile-log)
          (cd project-root)
          (case command-type
            ('compile (compile command-string))
            ('run (shell-command command-string))
            ('term
             (send-command-to-terminal
              (choose-buffer-local-terminal to-select-terminal)
              (format "cd %s &&\\\n %s" project-root command-string)))
            ('nop nil)
            ('t (message "Unknown command type, exiting")
                (return-from cp-custom-compile nil))))))))

(global-set-key "\C-ccc" 'cp-custom-compile)
(provide 'init-custom-compile)