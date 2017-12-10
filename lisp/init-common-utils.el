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

(defun _make-commands-map-with-help-msg (binding-lists)
  (let ((converted-list nil)
        (to-test (caar binding-lists))
        (list-copy nil))
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
       (let ((msg "Key bindings are as below:\n\n"))
         (dolist (key (reverse ',list-copy))
           (setq msg (concat msg (format "{ [%s] => %-70s }\n" (car key) (cdr key)))))
         (message (concat msg "\nPlease input: ")))
       ;; read key and get it run;
       (let* ((key (read-key))
	          (func (cdr (assoc (format "%c" key) ',list-copy))))
         (if func
	         (call-interactively func)
           (error "key <%s> was not binded\n" key))))))

(defun cu-set-key-bindings (keymap prefix binding-lists)
  "Binding multiple binding lists to PREFIX and binding PREFIX + ? to print the
help message.
There must not be the same key exist in two different list in BINDING-LISTS.
The BINDING-TYPE should be either global or local, which causing this function
to call global-set-key or local-set-key to bind the key.

Example:
(defconst map-1 '((?a . a-func) (?b . b-func)))
(defconst map-2 '((?c . c-func) (?c . c-func)))
(cu-set-key-bindings global-map \"\C-c\C-s\" '(map-1 map-2))"
  (define-key keymap prefix (_make-commands-map-with-help-msg binding-lists)))
  
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
(with-eval-after-load "ido"

  (defun _cu-insert-path(replace-home)
    (let ((path (ido-read-file-name "Insert a Abstract: ")))
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
                 (getenv "HOME") "~" (buffer-file-name))))
      (kill-new (format "[[%s][%s]]" name (file-name-nondirectory name)))))

  (defun cu-open-link ()
    (interactive)

    (defun is-separator (str i)
      "To check if the char at I of STR is a separator"
      (let ((ret nil)
            (ch (elt str i)))
        (dolist (c (append "\n,\"; :" nil) ret)
          (when (equal ch c) (setq ret t)))))
    
    (let* ((start (max (- (point) 256) 1))
           (end (min (+ (point) 256) (buffer-size)))
           (cur (- (point) start))
           (str (buffer-substring-no-properties start end))
           (len (length str))
           (start (do ((i cur))
                      ((or (< i 0) (is-separator str i)) (1+ i))
                    (decf i)))
           (end (do ((i cur))
                    ((or (>= i len) (is-separator str i)) i)
                  (incf i)))
           (maybe-filename (substring str start end)))
      (when (file-exists-p maybe-filename)
        (find-file-other-window maybe-filename))))
  
  (defconst cu-path-util-map
    '((?i . cu-insert-path-replace-home)
      (?I . cu-insert-path-absolute-home)
      (?s . cu-save-current-file-path)
      (?o . cu-save-current-file-path-org-style)
      (?j . cu-open-link))
    "Util key map for path saving to ring / paste, etc")

  (with-eval-after-load "cc-mode" (define-key c-mode-base-map "\C-c\C-l" nil))
  (with-eval-after-load "java-mode" (define-key java-mode-map "\C-c\C-l" nil))
  (cu-set-key-bindings global-map "\C-c\C-l" `(,cu-path-util-map))
  (with-eval-after-load "org"
    (cu-set-key-bindings org-mode-map "\C-c\C-l"
                           `(,cu-path-util-map ((?l . org-insert-link))))))

(defun cu-eval-file (file)
  "Return the eval result of filename as expression"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (buffer-end 1))
    (eval-last-sexp t)))

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
  (let ((pos 1)
        (result '())
        (str (cu-buffer-content-without-comment-lines buf comment-prefix)))
    (while (and (< pos (point-max))
                (string-match regex str pos))
      (add-to-list 'result (match-string part str))
      (setq pos (match-end part)))))



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


(provide 'init-common-utils)

