(require-package 'smart-compile)

(require 'init-smart-compile)

(defun pretty-format-string-list (list)
  (let (result)
    (dotimes (i (length list) result)
      (setq result (concat result (if (= i 0) nil ", ") (elt list i))))))

(defun run-program (run-lists)
  (let ((msg "")
        choices)
    (dolist (list run-lists)
      (let ((run-key (car list))
            (required-files (cadr list))
            (run-command (caddr list))
            )
        (push run-key choices)
        (cond ((stringp run-command)
               (setq msg (concat msg (char-to-string run-key) ": " (smart-compile-string run-command) "\n")))
              ((fboundp (car run-command))
               (setq msg (concat msg (char-to-string run-key) ": (" (prin1-to-string (car run-command)) ))
               (dolist (var (cdr run-command))
                 (setq msg (concat msg " " (if (stringp var) (smart-compile-string var) (print1-to-string var)))))
               (setq msg (concat msg ")\n"))
               ))))
    ;; (message msg)
    (let* ((run-list (assoc (read-char-choice msg choices) run-lists))
           (required-files (cadr run-list))
           (run-command (caddr run-list))
           not-exists-files)
      (dolist (file required-files)
        (when (not (file-exists-p (smart-compile-string  file)))
          (push (smart-compile-string file) not-exists-files)))
      (if not-exists-files
          (error (concat "files not exists: " (pretty-format-string-list not-exists-files))))
      (cond ((stringp run-command)
             (shell-command (smart-compile-string run-command)))
            ((fboundp (car run-command))
             (apply (car run-command) (mapcar (lambda(format) (smart-compile-string format)) (cdr run-command))))
            )
      )))

;;----------------------------------------------------------------------------
;; %F  absolute pathname            ( /usr/local/bin/netscape.bin )
;; %f  file name without directory  ( netscape.bin )
;; %n  file name without extension  ( netscape )
;; %e  extension of file name       ( bin )
;;----------------------------------------------------------------------------
;; run-program with a list of run list, each list formated like
;; (key (requried_file_list) command_to_execute)
(require 'init-common-utils)
(defun run-program-with-input-selection(execute-name)
  (let* ((input-file-name (ido-read-file-name "read-input-file: "))
         (output-file-name (concat (file-name-sans-extension input-file-name) ".out"))
         (command-string (format "%s < %s > %s" execute-name input-file-name output-file-name)))    
    (shell-command (message command-string))))


(defun trash-in-out-files()
  (interactive)
  (let ((in-out-flist (directory-files "." t "\\(in\\|out\\)$")))
    (when (yes-or-no-p (concat "remove all these files: "
                         (let ((ret ""))
                           (dolist (name in-out-flist ret)
                                    (setq ret (concat ret "\n" name))))))
      (dolist (name in-out-flist)
        (delete-file name)))))

(require 'init-windows)
(defun edit-an-input-file (file-name)
  (split-right-at-83-column nil)
  (find-file-other-window file-name))

(defun compose-c-run-program-list()
  (let ((execute-str (if (memq os '(linux macos)) "./%n" "%n.exe")))
    `((?e (,execute-str) ,(format "%s &" execute-str))
      (?i nil (edit-an-input-file "%n.in"))
      (?< (,execute-str) ,(format "%s < %%n.in &" execute-str))
      (?s (,execute-str) (run-program-with-input-selection ,execute-str))
      (?t nil (trash-in-out-files)))))

(defun run-c-program ()
  (interactive)
  (run-program
   (compose-c-run-program-list)))

;; (add-hook 'c-mode-hook
;;           (lambda () (local-set-key "\C-c\C-e" 'run-c-program)))
;; (add-hook 'c++-mode-hook
;;           (lambda () (local-set-key "\C-c\C-e" 'run-c-program)))

(provide 'init-smart-run)
