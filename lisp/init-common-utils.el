(require 'cl)
(defun cu-program-exists-p (program)
  (eq (shell-command (concat "which " program)) 0))

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

(defun* cu-seq-pre-subseq-n-equal-p (seq1 seq2 n)
  "Check if the pre N length subsequence of SEQ1 and SEQ2 equals.
And return t if equals, compare the item with `equal'."
  (when (or (not (listp seq1)) (not (listp seq2)))
    (return-from cu-seq-pre-subseq-n-equal-p nil))
  (when (or (< (length seq1) n) (< (length seq2) n))
    (return-from cu-seq-pre-subseq-n-equal-p nil))
  (setq n (1- n))
  (while (>= n 0)
    (when (not (equal (nth n seq1) (nth n seq2)))
      (return-from cu-seq-pre-subseq-n-equal-p nil))
    (setq n (1- n)))
  t)

(defun cu-join-path (root &rest args)
  "Join the path with \"/\" and erase the redundant \"/\""
  (reduce (lambda (a b)
            (concat (if (cu-seq-ends-with a "/") (substring a 0 -1) a)
                    "/"
                    (if (cu-seq-starts-with b "/") (substring b 1) b)))
          args
          :initial-value root))

(defun __cu-list-files-recursively-general (dir filter-function depth max-depth relative)
  "Private support function for `cu-list-files-recursively'"
  (when (file-accessible-directory-p dir)
    (let ((files (directory-files dir t))
          (matched nil))
      (dolist (fullname files matched)
        (let* ((basename (file-name-nondirectory fullname))
               (relative-path (when relative
                                (if (eq relative t)
                                    basename
                                  (cu-join-path relative basename)))))
          (cond
           ;; filter out the "." and ".."
           ((or (string= basename ".")
                (string= basename "..")) nil)
           ;; append the files which meets the requirement
           ((funcall filter-function fullname)
            (setq matched (cons (or relative-path fullname) matched)))
           ;; find the matching files recursively in subdirectories
           ((and (file-directory-p fullname)
                 (< depth max-depth))
            (let ((more-files
                   (__cu-list-files-recursively-general fullname filter-function (1+ depth) max-depth relative-path)))
              (when more-files (setq matched (append matched more-files)))))))))))

(defun cu-list-files-recursively-general (dir filter-function &optional max-depth relative)
  "Returns list of files which FILTER-FUNCTION(fullpath) return `t'"
  (when (not max-depth)
    (setq max-depth 10))
  (__cu-list-files-recursively-general dir filter-function 0 max-depth relative))

(defun cu-list-files-recursively (dir re &optional max-depth relative)
  "Returns list of files in DIR matching to given regexp RE"
  (cu-list-files-recursively-general
   dir
   #'(lambda (fullpath)
       (and (file-regular-p fullpath)
            (string-match re (file-name-nondirectory fullpath))))
   max-depth relative))

(defun* __cu-search-child-symlink-recursively-in (dir target-dir depth max-depth)
  "Private support function for `cu-list-files-recursively'"
  (when (>= depth max-depth)
    (return-from __cu-search-child-symlink-recursively-in nil))
  (when (file-accessible-directory-p dir)
    (let ((files (directory-files dir t))
          (matched nil))
      (dolist (fullname files matched)
        (let ((basename (file-name-nondirectory fullname)))
          (cond
           ;; filter out the "." and ".."
           ((or (string= basename ".")
                (string= basename "..")
                (file-regular-p fullname))
            nil)
           ;; find the matching files recursively in subdirectories
           ((and (file-directory-p fullname)
                 (string-match-p (file-truename fullname) target-dir))
            (return-from __cu-search-child-symlink-recursively-in fullname))
           ((file-directory-p fullname)
            (let ((possible (__cu-search-child-symlink-recursively-in fullname target-dir (1+ depth) max-depth)))
              (when possible (return-from __cu-search-child-symlink-recursively-in possible))))))))))

(defun cu-search-child-symlink-recursively-in (dir target-dir &optional max-depth)
  "Return the directory in DIR which symlink to TARGET-DIR"
  (when (not max-depth)
    (setq max-depth 3))
  (__cu-search-child-symlink-recursively-in dir (file-truename target-dir) 0 max-depth))

(defun cu-generate-mode-list-string (mode-list)
  (seq-reduce
   (lambda (init ml)
     (concat init "[" (car ml) " : " (prin1-to-string (eval (cdr ml))) "]\n"))
   mode-list
   ""))

(defvar global-show-keybindings-help-message t)

(defun cu-make-commands-map-with-help-msg (binding-lists &optional mode-list show-message)
  (let ((converted-list nil)
        (to-test (caar binding-lists))
        (list-copy nil)
        (show-detailed-message-symbol (intern (format "key-binding-show-detailed-message:%s"
                                                      (sha1 (prin1-to-string binding-lists))))))
    (if (not (boundp show-detailed-message-symbol))
        (set show-detailed-message-symbol (if (not show-message)
                                              global-show-keybindings-help-message
                                            (and (numberp show-message) (> show-message 0)))))
    (if (or (stringp to-test) (characterp to-test))
        (setq converted-list binding-lists)
      (progn (setq to-test (caaar binding-lists))
             (when (not (or (stringp to-test) (characterp to-test)))
               (error "invalid parameter 'binding-lists'"))
             (setq converted-list (seq-reduce 'append binding-lists nil))))
    (dolist (ele converted-list)
      (when (not (or (stringp (car ele)) (characterp (car ele))))
        (error "the key should be character or string"))
      (setq list-copy
            (if (characterp (car ele))
                (add-to-list 'list-copy `(,(char-to-string (car ele)) . ,(cdr ele)))
              (add-to-list 'list-copy ele))))
    `(lambda () (interactive)
       (let ((help-msg "Key bindings are as below:\n")
             (msg "")
             (choices nil))
         (when ',mode-list
             (setq help-msg (concat help-msg (cu-generate-mode-list-string ',mode-list))))
         (dolist (key (reverse ',list-copy))
           (setq help-msg (concat help-msg (format "%c [%s] => %-70s %c\n"
                                                   ?│ (car key) (cdr key) ?│)))
           (setq choices (add-to-list 'choices (string-to-char (car key)) t)))
         (when (eval ,show-detailed-message-symbol)
           (setq msg help-msg))
         (setq msg (concat msg "Please input: "))
         ;; read key and get it run;
         (setq choices (add-to-list 'choices ?? t))
         (let* ((key (read-char-choice msg choices)))
           (if (equal key ??)
               (progn (setq ,show-detailed-message-symbol (not (eval ,show-detailed-message-symbol)))
                      (if (eval ,show-detailed-message-symbol)
                          (message "Help message is as below:\n %s" help-msg)
                        (message nil)))
             (setq func (cdr (assoc (format "%c" key) ',list-copy)))
             (if func
                 (progn
                   (message nil)
                   (call-interactively func))
               (error "key <%s> was not binded\n" key))))))))

(defmacro cu-make-keymap-func (sym binding-lists &optional mode-list show-message)
  (let ((fun (intern (format "%s:gen" sym)))
        (doc "Function generated by cu-make-keymap-func"))
    `(defun ,fun () ,doc
            (interactive)
            (call-interactively (cu-make-commands-map-with-help-msg
                                 ,binding-lists ,mode-list ,show-message)))))


(defun cu-set-key-bindings (keymap prefix binding-lists &optional mode-list show-message)
  "Binding multiple binding lists to PREFIX and binding PREFIX + ? to print the
help message.
There must not be the same key exist in two different list in BINDING-LISTS.
The BINDING-TYPE should be either global or local, which causing this function
to call global-set-key or local-set-key to bind the key.

Example:
(defconst map-1 '((?a . a-func) (?b . b-func)))
(defconst map-2 '((?c . c-func) (?c . c-func)))
(cu-set-key-bindings global-map \"\C-c\C-s\" '(map-1 map-2))"
  (define-key keymap prefix (cu-make-commands-map-with-help-msg binding-lists mode-list show-message)))

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

;; TODO: make it a absolute and relative pair
(defun* cu-possible-path-at-point()
  "If there's a path at current point, return the path,
otherwise, return nil"
  (interactive)
  (let* ((line
         (cu-strip-string
          (buffer-substring-no-properties
           (line-beginning-position) (point))
          t t))
         (len (length line)))
    (dotimes (beg len)
      (let ((substr (substring line beg len)))
        (when (file-exists-p substr)
          (return-from cu-possible-path-at-point substr))))))

(with-eval-after-load "ido"

  (defun _cu-insert-path(replace-home)
    (let* ((possible-path (cu-possible-path-at-point))
           (prompt-path (or possible-path default-directory))
           (prompt-path (expand-file-name prompt-path))
           (path (ido-read-file-name "Insert a abstract: " prompt-path)))
      (when possible-path (setq path (substring path (length prompt-path))))
      (if replace-home
          (insert (replace-regexp-in-string (getenv "HOME") "~" path))
        (insert path))))
  (defun cu-insert-path-replace-home () (interactive) (_cu-insert-path t))
  (defun cu-insert-path-absolute-home () (interactive) (_cu-insert-path nil))
  (defun cu-save-current-file-path ()
    (interactive)
    (kill-new
     (replace-regexp-in-string (getenv "HOME") "~" (buffer-file-name))))

  (defun cu-save-current-file-path-org-style ()
    (interactive)
    (let ((name (replace-regexp-in-string
                 (getenv "HOME") "~"
                 (or (buffer-file-name)
                     (substring default-directory 0 (1- (length default-directory)))))))
      (kill-new (format "[[%s][%s]]" name (file-name-nondirectory name)))))

  (defun cu-string-sequences-at-point ()
    (interactive)
    ;; Define the separator
    (defun is-separator (str i)
      "To check if the char at I of STR is a separator"
      (let ((ret nil)
            (ch (elt str i)))
        (dolist (c (append "\t\n,\"; ()[]{}" nil) ret)
          (when (equal ch c) (setq ret t)))))
    ;;
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (cur (- (point) start))
           (str (buffer-substring-no-properties start (1+ end)))
           (len (length str))
           (cur (if (not (is-separator str cur))
                    cur
                  (setq cur 0)
                  (while (and (< cur len) (is-separator str cur))
                    (incf cur))
                  cur))
           (start (do ((i cur))
                      ((or (< i 0) (is-separator str i)) (1+ i))
                    (decf i)))
           (end (do ((i cur))
                    ((or (>= i len) (is-separator str i)) i)
                  (incf i)))
           (end (min end len))
           (maybe-filename nil))
      (when (< start end) (setq maybe-filename (substring str start end)))
      (message "The string sequence is [%s]" maybe-filename)
      maybe-filename))

  (defun* can-split-out-a-filename (name)
    (when (not (stringp name))
      (error "name is not string"))
    (when (file-exists-p (expand-file-name name))
      (return-from can-split-out-a-filename (expand-file-name name)))
    (dolist (n (split-string name "[ :=\f\t\n\r\v]+"))
      (when (file-exists-p (expand-file-name n))
        (return-from can-split-out-a-filename (expand-file-name n)))))

  (defconst cu-link-list
    '(("File" can-split-out-a-filename find-file-other-window)
      ("External link" cu-find-external-link cu-open-external-link)))

  (defun* cu-find-external-link (string)
    (when (boundp 'cu-private-external-link-list)
      (dolist (ll cu-private-external-link-list)
        (let ((pattern (car ll))
              (format-str (cadr ll)))
          (when (string-match pattern string)
            (return-from cu-find-external-link
              (format format-str (match-string 0 string))))))))

  (defun* cu-open-external-link (string)
    (when (fboundp 'pc-open-with-mac-browser-command)
      (shell-command (format "%s %s" (pc-open-with-mac-browser-command) string))))

  (with-eval-after-load "org"
    (defun* cu-open-link ()
      (interactive)
      (let ((maybe-filename
             (if (and (equal major-mode 'org-mode)
                      (org-in-regexp org-bracket-link-regexp 1))
                 (org-link-unescape (match-string-no-properties 1))
               (cu-string-sequences-at-point))))
        (dolist (ele cu-link-list)
          (let ((type (car ele))
                (check-func (cadr ele))
                (apply-func (caddr ele))
                (matched nil))
            (when (fboundp check-func)
              (setq matched (funcall check-func maybe-filename))
              (when matched
                (message "Found matched type: %s" type)
                (if (fboundp apply-func)
                    (funcall apply-func (if (eq matched t) maybe-filename matched))
                  (error (format "%s not defined" (symbol-name apply-func))))
                (return-from cu-open-link t)))))
        (error "no matched link found"))))


  (defun cu-visit-file-follow-symlink ()
    (interactive)
    (let* ((curname (buffer-file-name))
           (truename (file-truename curname)))
      (when (and (not (equal curname truename))
                 (y-or-n-p (format
                            "Change visited file from: \n[%s] -> [%s]\n(y or n?):"
                            curname truename)))
        (setq-local buffer-file-name truename)
        (setq-local default-directory (file-name-directory truename))))))

(defun cu-read-string-from-file (file)
  (let (str)
    (save-excursion
      (with-temp-buffer
        (insert-file-contents file)
        (setq str (buffer-string))
        (kill-buffer)))
    str))

(defun cu-eval-file (file)
  "Return the eval result of filename as expression"
  (let* ((buf-str (cu-read-string-from-file file))
         (expr (read-from-string buf-str)))
    (eval-expression (car expr))))

(defun cu-add-exec-path-maybe (path &optional append)
  "Add the path to exec-path if PATH exists and it's a directory,
 add the path to last if APPEND is not nil"
  (if (cu-is-dir-or-dirlink-p path)
      (add-to-list 'exec-path path append)
    (message "WARNING: %s is not a directory" path)))

(defun cu-strip-string (str beforep afterp)
    "Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space."
    (string-match (concat "\\`" (if beforep "\\s-*")
                        "\\(.*?\\)" (if afterp "\\s-*\n?")
                        "\\'") str)
    (match-string 1 str))

(defun cu-search-brew-executable (program)
  (let* ((command
          (format "brew list %s | grep \"bin/%s\"" program program))
         (possible-name
          (cu-strip-string (shell-command-to-string command) t t)))
    (if (and possible-name
             (file-executable-p possible-name))
        possible-name
      nil)))


;; Buffer regexp search utils

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
      (split-string (buffer-substring-no-properties
                     (point-min) (point-max)) "\n"))
     "")))

(defun cu-buffer-matched-lists(buf comment-prefix regex part)
  "Search the buffer content, and find matches in REGEX and return a list with
matched PART, the comment lines will be skipped."
  (let* ((pos 1)
         (result '())
         (str (cu-buffer-content-without-comment-lines buf comment-prefix))
         (len (length str)))
    (while (and (< pos len)
                (string-match regex str pos))
      (push (match-string part str) result)
      (setq pos (match-end part)))
    (delete-dups (reverse result))))



;; Multi-Level choice list

(defun* cu-reshape-multi-level-choice-list* (li ndim)
  "Reshape a multi-level choice list, the input list is like this:
'((\"a\" \"b\" func-1)
  (\"a\" \"b\" func-2)
  (\"a\" \"c\" func-3)
  (\"b\" \"b\" func-4)
  (\"b\" \"b\" func-5)
  (\"b\" \"c\" func-6))
And the reshaped output multi-level list is as below:
'((\"a\" ((\"b\" func-1) (\"c\" func-3))) (\"b\" ((\"b\" func-4) (\"c\" func-6))))
"
  ;; parameter validation.
  (when (or (not li) (not (listp li))) (return-from cu-reshape-multi-level-choice-list* nil))
  ;; sort from dim (ndim-1 to 0)
  (let ((dim (1- ndim)))
    (while (>= dim 0)
      (setq li (sort li (lambda (e1 e2) (string-lessp (nth dim e1) (nth dim e2)))))
      (setq dim (1- dim))))
  ;; remove duplicate
  (let ((dim ndim)
        (index 1)
        (temp-li (list (car li))))
    (while (< index (length li))
      (when (not (cu-seq-pre-subseq-n-equal-p (nth index li) (nth (1- index) li) dim))
        (push (nth index li) temp-li))
      (setq index (1+ index)))
    (setq li (reverse temp-li)))
  ;; fold the list
  (let ((dim (1- ndim))
        (index nil)
        (temp-li nil)
        (length nil))
    ;; fold backword from dim -> 1
    (while (>= dim 1)
      (setq index 1
            temp-li (list (append (subseq (car li) 0 dim)
                                  (list (list (subseq (car li) dim)))))
            len (length li))
      ;; fold the item which previous <dim> subseq are equal.
      (while (< index len)
        (if (cu-seq-pre-subseq-n-equal-p (nth index li) (nth (1- index) li) dim)
            (progn
              (push (subseq (nth index li) dim) (nth dim (nth 0 temp-li)))
              (setf (nth dim (nth 0 temp-li)) (reverse (nth dim (nth 0 temp-li)))))
          (push (append (subseq (nth index li) 0 dim)
                        (list (list (subseq (nth index li) dim))))
                temp-li))
        (setq index (1+ index)))
      ;; As the items is added with `push', reverse is needed.
      (setq li (reverse temp-li))
      (setq dim (1- dim))))
  li)
;; (cu-reshape-multi-level-choice-list*-test)
(defun cu-reshape-multi-level-choice-list*-test ()
  (assert
   (equal
    (cu-reshape-multi-level-choice-list* '(("a" "b" func-1)
                                           ("a" "b" func-2)
                                           ("a" "c" func-3)
                                           ("b" "b" func-4)
                                           ("b" "b" func-5)
                                           ("b" "c" func-6)) 2)
    '(("a" (("b" func-1) ("c" func-3))) ("b" (("b" func-4) ("c" func-6)))))))

(defun cu-choose-from-reshaped-mlcl (li ndim &optional desc_list)
  "Choose from a reshaped multi-level choice list,
the input list should be as below:
'((\"a\" ((\"b\" func-1) (\"c\" func-3))) (\"b\" ((\"b\" func-4) (\"c\" func-6))))
read input choices with `ido-completing-read' and return the chosed item list.
NDIM is the dimentions of the choice items.
"
  (let ((level 0)
        (li-copy li)
        choice choosed-list ret)
    (while (< level ndim)
      (setq choice
            (if (> (length li-copy) 1)
                (ido-completing-read
                 (if desc_list
                     (format "Choose [%s] (%d level): " (nth level desc_list) level)
                   (format "Choose %d level key: " level))
                 (mapcar 'car li-copy))
              (caar li-copy))
            choosed-list (assoc choice li-copy)
            li-copy (cadr choosed-list)
            ret (cons choice ret))
      (setq level (1+ level)))

    (setq ret (cons (cadr choosed-list) ret))
    (reverse ret)))

;; (cu-choose-from-reshaped-mlcl-test)
(defun cu-choose-from-reshaped-mlcl-test ()
  (cu-choose-from-reshaped-mlcl
   '(("a" (("b" func-1) ("c" func-3)))
     ("b" (("b" func-4) ("c" func-6))))
   3 ;; dimen
   ;; Naming for diffent level key
   '("project"
     "home")))


(defun cu-read-ignore-list (dir ignore-file-name)
  (let ((ignore-file (cu-join-path dir ignore-file-name)))
    (if (file-exists-p ignore-file)
        (let ((content (with-current-buffer (find-file-noselect ignore-file)
                         (buffer-substring-no-properties (point-min) (point-max)))))
          (seq-filter (lambda (x)
                        (and (not (equal x ""))
                             (not (cu-seq-starts-with x "#"))))
                      (split-string content "\n"))))))


(defun cu-toggle-make-lyric ()
  (interactive)
  (if cursor-type
      (progn (setq cursor-type nil)
             (delete-other-windows)
             (maximize-frame)
             (split-window-horizontally-instead)
             (color-theme-sanityinc-tomorrow-blue)
             (dolist (fc '(("lyric" (han "STFangsong" 35) (ascii "Monaco" 32))))
               (set-font-for-current-frame fc))
             (maximize-frame))
    (setq cursor-type t)
    (color-theme-sanityinc-tomorrow-eighties)
    (next-font 0)
    (maximize-frame)))

(defun cu-dir-to-sha1 (dir)
  (let ((d (expand-file-name dir)))
    (when (cu-seq-ends-with d "/")
      (setq d (substring d 0 -1)))
    (sha1 d)))

(defun* __cu-find-nearest-ancestor-link-in (root dir)
  (when (or (not dir) (equal dir "/"))
    (return-from __cu-find-nearest-ancestor-link-in nil))
  (let ((link (cu-join-path root (cu-dir-to-sha1 dir))))
    (when (file-exists-p link)
      (return-from __cu-find-nearest-ancestor-link-in link)))
  (__cu-find-nearest-ancestor-link-in
   root (file-name-directory (directory-file-name dir))))

(defun cu-find-nearest-ancestor-link-in (root path)
  "Find the nearest ancestor directory who has `cu-dir-to-sha1(dir)` located under root"
  ;; append "/" for directory.
  (when (and (cu-is-dir-or-dirlink-p path)
             (not (equal (substring path (1- (length path))) "/")))
    (setq path (concat path "/")))
  ;; extract the dir part.
  (setq path (file-name-directory (expand-file-name path)))
  (__cu-find-nearest-ancestor-link-in root path))

(defun cu-is-alphabet (c)
  (and (characterp c)
       (or (and (>= c ?a) (<= c ?z))
           (and (>= c ?A) (<= c ?Z)))))

(defun kill-processes-in-the-same-group ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (ignore-errors
      (when proc
        (kill-process proc t)))))

(defun cu-start-process (name buffer program &rest program-args)
  (apply 'start-process name buffer program program-args)
  (with-current-buffer buffer
    (let ((kill-buffer-hook-copy
           (buffer-local-value 'kill-buffer-hook (get-buffer buffer))))
      (add-to-list 'kill-buffer-hook-copy 'kill-processes-in-the-same-group)
      (setq-local kill-buffer-hook kill-buffer-hook-copy))))

(defun cu-count-lines (string)
  (let ((count 1))
    (mapc (lambda (c)
            (when (equal c ?\n)
              (setq count (1+ count))))
     string)
    count))

(defun cu-read-word-or-region ()
  (interactive)
  (let ((bounds nil))
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (or (and (setq bounds (bounds-of-thing-at-point 'word))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
          (error "No word at point.")))))

(defun* cu-newline-and-indent ()
  (interactive "*")
  (when (and (boundp 'magit-blame-mode) magit-blame-mode)
    (call-interactively 'magit-show-commit)
    (return-from cu-newline-and-indent t))
  (let ((cur (char-after (point)))
        (prev (char-before (point))))
    (if (or (and  (equal prev ?\() (equal cur ?\)))
            (and  (equal prev ?\{) (equal cur ?\}))
            (and  (equal prev ?\[) (equal cur ?\])))
        (progn
          (newline)
          (save-excursion
            (newline)
            (indent-according-to-mode))
          (indent-according-to-mode))
      (newline-and-indent))))

(defun* cu-insert-semicolon ()
  (interactive "*")
  (let ((is-last-char (equal (line-end-position) (point))))
    (if is-last-char
        (progn
          (self-insert-command 1)
          (newline)
          (indent-according-to-mode))
      (self-insert-command 1))))

(defun cu-set-skeleton-pair-indent ()
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key [(return)] 'cu-newline-and-indent))

(defun cu-set-gopath (path)
  (message "set GOPATH to %s" path)
  (setenv "GOPATH" path))

(defun cu-get-gopath ()
  (interactive)
  (message "GOPATH: %s" (getenv "GOPATH")))

(defun cu-cycle-flycheck-error (&optional arg)
  (interactive "P")
  (let ((pos (flycheck-next-error-pos (if arg -1 1))))
    (if pos
        (goto-char pos)
      (when (not arg)
        (flycheck-first-error)))))

(with-eval-after-load "org"
  (defun cu-offline-org-to-latex (file output-file)
    (with-current-buffer (find-file-noselect file)
      (org-export-to-file 'latex output-file nil nil nil nil nil)
      (kill-buffer))))

(with-eval-after-load "lsp-mode"
  (defun cu-toggle-lsp ()
    (interactive)
    (if lsp-mode
        (progn (lsp-mode -1)
               (lsp-ui-mode -1)
               (fci-mode 1))
      (lsp)
      (fci-mode -1))))

(defun cu-set-python-virtualenv (&optional python-home)
  (interactive)
  (unless python-home
    (setq python-home
          (ido-read-file-name "Choose PYTHONHOME: "
                              (cu-join-path (getenv "HOME") ".py-vir-env"))))
  (unless (file-exists-p python-home)
    (error "%s not exist" python-home))
  ;; (setenv "PYTHONHOME" python-home)
  ;; (setenv "VIRTUAL_ENV" python-home)
  (add-to-path (cu-join-path python-home "bin") t))

(defun cu-make-mode-specific-custom-compile-rule-map
    (mode with-cur-buffer binding-lists &optional mode-list show-message)
  (when (equal major-mode mode)
    `((,default-directory
        ((unit ,(concat "mode-specific" (symbol-name major-mode))
               (elisp (when ,with-cur-buffer
                        (with-current-buffer cp-custom-compile-current-buffer
                          (call-interactively
                           ,(cu-make-commands-map-with-help-msg
                             binding-lists mode-list show-message)))))))))))

(defun cu-toggle-debug-on-error ()
  (interactive)
  (and (y-or-n-p (message "%s debug-on-error"
                          (if debug-on-error "Turn off" "Turn on")))
       (setq debug-on-error (not debug-on-error))))

(defun cu-get-or-select-buffer-local-terminal (&optional reselect-terminal)
  "Get project local terminal or select one from the existed terminal if no
terminal was selected before or RESELECT-TERMINAL is not nil"
  (interactive "P")
  (when (or (not (boundp 'buffer-local-terminal))
            (not (buffer-live-p buffer-local-terminal))
            reselect-terminal)
    (set (if (not (boundp 'buffer-local-terminal))
             (make-local-variable 'buffer-local-terminal)
           'buffer-local-terminal)
         (let ((buffer-list '()))
           (dolist (term multi-term-buffer-list)
             (add-to-list 'buffer-list (buffer-name term)))
           (get-buffer
            (ido-completing-read "Select a terminal: " buffer-list)))))
  buffer-local-terminal)

(defun cu-send-command-to-terminal (term command)
  "Send the COMMAND to the TERMINAL"
  (with-current-buffer term
    (term-send-raw-string (concat command "\n"))
    (when (or (not (get-buffer-window term))
              (not (eq (window-frame (get-buffer-window term)) (selected-frame))))
      (switch-to-buffer-other-window (current-buffer))
      (end-of-buffer))))

(defun cu-send-command-to-buffer-local-terminal
    (command &optional reselect-terminal)
  (cu-send-command-to-terminal
   (cu-get-or-select-buffer-local-terminal reselect-terminal)
   command))

(with-eval-after-load "lsp-mode"
  (defun cu-lsp-execute-command ()
    (interactive)
    (let* ((commands
            (seq-filter
             (lambda (title-action-cons) (car title-action-cons))
             (mapcar
              (lambda (action)
                (cons (ht-get action "title" nil) action))
              (lsp-get-or-calculate-code-actions))))
           (choices
            (mapcar
             (lambda (command) (car command))
             commands)))
      (lsp-execute-code-action
       (cdr (assoc (ido-completing-read "Choose your action: " choices) commands))))))

(defun cu-open-current-file-with-external-app (&optional choose-app)
  (interactive)
  (let ((app (if choose-app
                 (read-string "Input command: ")
               "open")))
    (shell-command (format "%s %s" app (buffer-file-name)))))

(defun cu-open-with-idea ()
  "Open current buffer at current line in IntelliJ"
  (interactive)
  (let ((idea-executable
         (cond
          ((equal (shell-command "which idea 2>/dev/null") 0) "idea")
          ((file-executable-p "/Applications/IntelliJ IDEA CE.app/Contents/MacOS/idea")
           "/Applications/IntelliJ IDEA CE.app/Contents/MacOS/idea")
          (t (error "idea not exist")))))
    (shell-command (format "%s -l %d %s &>/dev/null" (shell-quote-argument idea-executable) (line-number-at-pos) (buffer-file-name)))))

(provide 'init-common-utils)
