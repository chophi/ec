(require 'cl)
(defun cu-program-exists-p (program)
  (not (equal (shell-command-to-string (concat "which " program)) "")))

(defun cu-show-gerrit-at-point (&optional refresh)
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
  (defun cu-smart-run (run-lists)
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

(defun cu-seq-starts-with (a b)
  "Check if sequence A starts with B, compare per element with `equal'"
  (let ((ret nil) (la (length a)) (lb (length b)))
    (and (>= la lb)
         (not (catch 'found-not-equal
                (dotimes (i lb ret)
                  (when (not (equal (elt a i) (elt b i)))
                    (throw 'found-not-equal t))))))))

(defun cu-seq-ends-with (a b)
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
            (concat (if (cu-seq-ends-with a "/") (substring a 0 -1) a)
                    "/"
                    (if (cu-seq-starts-with b "/") (substring b 1) b)))
          args
          :initial-value root))

(defun __cu-list-files-recursively (dir re depth max-depth)
  "Private support function for `cu-list-files-recursively'"
  (when (file-accessible-directory-p dir)
    (let ((files (directory-files dir t))
          (matched nil))
      (dolist (fullname files matched)
        (let ((basename (file-name-nondirectory fullname)))
          (cond
           ;; filter out the "." and ".."
           ((or (string= basename ".")
                (string= basename "..")) nil)
           ;; append the matched regular file to matched
           ((and (file-regular-p fullname)
                 (string-match re basename))
            (setq matched (cons fullname matched)))
           ;; find the matching files recursively in subdirectories
           ((and (file-directory-p fullname)
                 (< depth max-depth))
            (let ((more-files
                   (__cu-list-files-recursively fullname re (1+ depth) max-depth)))
              (when more-files (setq matched (append matched more-files)))))))))))

(defun cu-list-files-recursively (dir re &optional max-depth)
  "Returns list of files in DIR matching to given regexp RE"
  (when (not max-depth)
    (setq max-depth 10))
  (__cu-list-files-recursively dir re 0 max-depth))

(defun cu-set-key-bindings (prefix binding-lists binding-type)
  "Binding multiple binding lists to PREFIX and binding PREFIX + ? to print the
help message.
There must not be the same key exist in two different list in BINDING-LISTS.
The BINDING-TYPE should be either global or local, which causing this function
to call global-set-key or local-set-key to bind the key.

Example:
(defconst map-1 '((?a . a-func) (?b . b-func)))
(defconst map-2 '((?c . c-func) (?c . c-func)))
(cu-set-key-bindings \"\C-c\C-s\" '(map-1 map-2) 'local)"
  (let ((helpmsg "")
        (binding-func nil)
        (unbinding-func nil))
    (cond ((eq binding-type 'global)
           (setq binding-func 'global-set-key
                 unbinding-func 'global-unset-key))
          ((eq binding-type 'local)
           (setq binding-func 'local-set-key
                 unbinding-func 'local-unset-key))
          (t (error "The binding type should be either 'global or 'local")))
    (funcall unbinding-func prefix)
    (dolist (blist binding-lists)
      (dolist (bitem blist)
        (let* ((key (car bitem))
               (key-string (if (characterp key) (char-to-string key) key))
               (func (cdr bitem))
               (func-name (symbol-name func)))
          (setq helpmsg (concat helpmsg (format "%s : %s\n" key-string func-name)))
          (funcall binding-func (concat prefix key-string) func))))
    ;; Bind the key "prefix ?" to print the help message
    (funcall binding-func (concat prefix "?")
             `(lambda () (interactive) (message ,helpmsg)))))

(defun cu-buffer-content-without-comment-lines (buf comment-prefix)
  "Return the content in BUF with comment lines removed"
  (with-current-buffer buf
    (seq-reduce
     (lambda (a b) (concat a b "\n"))
     (seq-filter
      (lambda (str)
        (or (< (length str) (length comment-prefix))
            (not (equal comment-prefix
                        (substring str 0 (length comment-prefix))))))
      (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
     "")))

(defun cu-is-dir-or-dirlink-p (path)
  (and (file-exists-p path)
       (let ((first-attr (car (file-attributes path))))
         (or (eq first-attr t)
             (and (stringp first-attr) (cu-is-dir-or-dirlink-p first-attr))))))

(defun* __cu-find-nearest-ancestor-match (dir filename &optional is-regexp)
  (when (or (not dir) (equal dir "/"))
    (return-from __cu-find-nearest-ancestor-match nil))
  ;; check if there's a match in current folder.
  (if is-regexp
      (dolist (file (directory-files dir))
        (when (string-match-p filename file)
          (return-from __cu-find-nearest-ancestor-match (cu-join-path dir file))))
    (let ((file (cu-join-path dir filename)))
      (when (file-exists-p file)
        (return-from __cu-find-nearest-ancestor-match file))))
  (__cu-find-nearest-ancestor-match
   (file-name-directory (directory-file-name dir))
   filename is-regexp))

(defun cu-find-nearest-ancestor-match (path filename &optional is-regexp)
  "Search the nearest ancestor file, begin at DIR, whose name matches FILENAME,
if IS-REGEXP is not nil, use `string-match-p' to search the match file, otherwise
just compare the filename with `string='"
  ;; append "/" for directory.
  (when (and (cu-is-dir-or-dirlink-p path)
             (not (equal (substring path (1- (length path))) "/")))
    (setq path (concat path "/")))
  ;; extract the dir part.
  (setq path (file-name-directory (expand-file-name path)))
  (__cu-find-nearest-ancestor-match path filename is-regexp))

(defun cu-check-image-support ()
  "Check if the image types: '(png jpeg tiff gif xpm svg) are supported.
Return a list that a supported"
  (interactive)
  (seq-filter (lambda (type)
                (if (image-type-available-p type)
                    (progn
                      (message (format "%s is supported" (symbol-name type)))
                      t)
                  (progn
                    (message (format "%s is not supported" (symbol-name type)))
                    nil)))
              '(png jpeg tiff gif xpm svg)))

(provide 'init-common-utils)

