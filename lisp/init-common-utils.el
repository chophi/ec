(defun program-exists-p (program)
  (not (equal (shell-command-to-string (concat "which " program)) "")))

(defun show-gerrit-at-point (&optional refresh)
  "Query the gerrit change at point with
'ssh -p 9418 <server> gerrit query --current-patch-set <id> --format json'
and pretty print the output into *gerrit [<id>]* buffer"
  (interactive "P")
  (let* ((str (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))
         (regex "https://\\(.*\\)/gerrit/.*?\\([0-9]+\\)")
         (link (progn (string-match regex str) (match-string 0 str)))
         (server (progn (string-match regex str) (match-string 1 str)))
         (change-id (progn (string-match regex str) (match-string 2 str)))
         (command
          (format
           "ssh -p 9418 %s gerrit query --current-patch-set %s --format json"
           server change-id))
         (buffer-name (format "*gerrit [%s]*" change-id)))
    (if (and (get-buffer buffer-name) (not refresh))
        nil
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert (shell-command-to-string command))
        (json-mode)
        (json-pretty-print-buffer)))
    (switch-to-buffer-other-window buffer-name)))

(with-eval-after-load "smart-compile"
  (defun smart-run (run-lists)
    "Run some commands from RUN-LISTS.
The run lists look like this:
((?e (\"./%n\") \"./%n &\")
 (?i nil (open-the-input-file \"%n.in\"))
 (?< (\"./%n\") \"./%n < %n.in &\")
 (?s (\"./%n\") (select-input-file-and-run \"./%n\"))
 (?t nil (delete-input-output-files)))
each list formated like: '(key (requried_file_list) command_to_execute),
and the file name in required_file_list and command_to_execute will be
re-interpreted via using smart-compile-string.
"
    (let ((help-msg "") choices)
      (dolist (list run-lists)
        (let* ((run-key (car list))
               (required-files (cadr list))
               (run-command (caddr list))
               (command-string
                (cond ((stringp run-command)
                       (smart-compile-string run-command))
                      ((fboundp (car run-command))
                       (if (cdr run-command)
                           (format "(%s %s)" (prin1-to-string (car run-command))
                                   (reduce
                                    (lambda (x y) (concat x " " y))
                                    (mapcar
                                     (lambda (x)
                                       (if (stringp x)
                                           (smart-compile-string x)
                                         (prin1-to-string x)))
                                     (cdr run-command))))
                         (prin1-to-string (car run-command))))
                      (t (error "command error: %s" prin1-to-string run-command)))))
          (setq help-msg
                (concat help-msg
                        (format "%s : %s\n" (char-to-string run-key) command-string)))
          (push run-key choices)))
      
      (let* ((run-list (assoc (read-char-choice help-msg choices) run-lists))
             (required-files (cadr run-list))
             (run-command (caddr run-list))
             (non-existed-files
              (seq-filter
               (lambda (file) (not (file-exists-p file)))
               (mapcar 'smart-compile-string required-files))))
        (when non-existed-files
          (error (concat
                  "files not exists: "
                  (reduce (lambda (x y) (concat x ", " y)) non-existed-files))))
        (cond ((stringp run-command)
               (shell-command (smart-compile-string run-command)))
              ((fboundp (car run-command))
               (apply (car run-command)
                      (mapcar
                       (lambda (arg)
                         (smart-compile-string arg))
                       (cdr run-command)))))))))

(defun seq-starts-with (a b)
  "Check if sequence A starts with B, compare per element with `equal'"
  (let ((ret nil) (la (length a)) (lb (length b)))
    (and (>= la lb)
         (not (catch 'found-not-equal
                (dotimes (i lb ret)
                  (when (not (equal (elt a i) (elt b i)))
                    (throw 'found-not-equal t))))))))

(defun seq-ends-with (a b)
  "Check if sequence A ends with B, compare per element with `equal'"
  (let ((ret nil) (la (length a)) (lb (length b))
        (ai (1- (length a))) (bi (1- (length b))))
    (and (>= la lb)
         (not (catch 'found-not-equal
                (dotimes (i lb ret)
                  (when (not (equal (elt a (- ai i)) (elt b (- bi i))))
                    (throw 'found-not-equal t))))))))

(defun cu-join-path (root &rest args)
  "Join the path with \"/\" and erase the redundant \"/\""
  (reduce (lambda (a b)
            (concat (if (seq-ends-with a "/") (substring a 0 -1) a)
                    "/"
                    (if (seq-starts-with b "/") (substring b 1) b)))
          args
          :initial-value root))

(provide 'init-common-utils)

