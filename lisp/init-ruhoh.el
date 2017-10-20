(require 'name-variables)

(setq ruhoh-blog-root-directory (expand-file-name "~/blog/chophi"))

(defconst ruhoh-blog-posts-directory
  (expand-file-name (concat ruhoh-blog-root-directory "/posts"))
  "the directory of the posts")
(defconst ruhoh-blog-media-directory
  (expand-file-name (concat ruhoh-blog-root-directory "/media"))
  "the directory of the media")
(defconst ruhoh-blog-rackup-command
  "bundle exec ruhoh server"
  "rackup command")
(defconst ruhoh-blog-port
  "9293"
  "the port of ruhoh")
(defconst ruhoh-shell-process-buffer
  "*RUHOH RACKUP*"
  "the buffer to save the output of rackup")
(defconst ruhoh-rackup-command
  (format "cd %s && %s %s" 
	  ruhoh-blog-root-directory
	  ruhoh-blog-rackup-command
	  ruhoh-blog-port)
  "command to rackup rahoh")
  
(defun ruhoh-rackup()
  "rackup ruhoh at the port `ruhoh-blog-port'"
  (interactive)
  (shell-command (concat ruhoh-rackup-command "&")
		 ruhoh-shell-process-buffer))
(defun ruhoh-rackdown()
  "kill the rackup process"
  (interactive)
  (let ((ruhoh-rack-process (get-buffer-process ruhoh-shell-process-buffer)))
    (when ruhoh-rack-process
	(kill-process ruhoh-rack-process)
      )))

(defun ruhoh-rack-restart()
  "restart ruhoh, but to do this, have to wait the activated process quit"
  (interactive)
  (let ((process (get-buffer-process ruhoh-shell-process-buffer)))
    (if process
	;; if there is a process racked up, wait until it killed, then rackup
      (progn	
	(set-process-sentinel 
	 process
	 (lambda (process event) (when (equal event "killed\n") (ruhoh-rackup))))

	 (ruhoh-rackdown))
      ;; if there is no process, just rackup
      (ruhoh-rackup))))

(defconst ruhoh-post-tag-type-list
  '(("date" . string) 
    ("categories" . raw-list)
    ("tags" . raw-list)
    ("layout" . string)
    ("title" . string)
    ("description" . string)
    ("postid" . string)
    ))

(defconst ruhoh-post-tag-list
       nil
       "the tags of ruhoh to use")
(dolist (tag ruhoh-post-tag-type-list)
  (push (car tag) ruhoh-post-tag-list))

(defun ruhoh-erase-end-spaces(str)
  (let ((len (1- (length str))))
    (while (and (>= len 0) (equal (elt str len) ?\s))
      (setq len (1- len)))
    (if (>= len 0)
      (substring str 0 (1+ len))
      nil)))
(defun ruhoh-erase-begin-spaces(str)
  (let ((len (length str)) (idx 0))
    (while (and (< idx len) (equal (elt str idx) ?\s))
      (setq idx (1+ idx)))
    (if (< idx len)
	(substring str idx len)
      nil)))
(defun ruhoh-erase-begin-and-end-spaces (str)
  (ruhoh-erase-end-spaces (ruhoh-erase-begin-spaces str))
  )

(defun ruhoh-post-get-tag-assoc-list()
  (interactive)
  (goto-char (point-min))
  (let (re tag-list-with-upcase tag-alist)
    (dolist (tag ruhoh-post-tag-list)
      (push tag tag-list-with-upcase)
      (push (upcase tag) tag-list-with-upcase))
    (setq re (regexp-opt tag-list-with-upcase))
    (while (re-search-forward (concat "#\\+\\(" re "\\):\s*\\(.+\\)$") nil t)
      (push `(,(downcase (match-string-no-properties 1)) ,(ruhoh-erase-end-spaces (match-string-no-properties 2))) tag-alist))
    ;; check date message
    (if (not (assoc "date" tag-alist))
	(error "Error: No Date message !!!!"))
     tag-alist)
)

(defun ruhoh-post-print-list (lst)
  (catch 'return
    (when (or (not lst) (equal "" lst))
      (throw 'return nil))
    (let ((alist (split-string lst ",")) ret)
      (dolist (str alist)
	(when ret (setq ret (concat ret ",")))
	(when (and str (not (equal "" str)))
	  (setq ret (concat ret (format "'%s'" (ruhoh-erase-begin-and-end-spaces str))))))
      ret)))
;; (ruhoh-post-print-list ",")

(defun ruhoh-post-meta-infomations()
  (interactive)
  (let (str prop val)
    (setq str "---\n")
    (dolist (alist (ruhoh-post-get-tag-assoc-list))
      (setq prop (cdr (assoc (car alist) ruhoh-post-tag-type-list)))
      (when (equal prop 'string)
	(setq val (cadr alist))
	(when val
	  (setq str (concat str (format "%s: '%s'\n" (car alist) val)))))
      
      (when (equal prop 'raw-list)
	(setq val (cadr alist))
	(when val
	  (setq str (concat str (format "%s: %s\n" (car alist) val)))))

      (when (equal prop 'list) 
	(setq val (ruhoh-post-print-list (cadr alist)))
	(when val
	  (setq str (concat str (format "%s: [%s]\n" (car alist) val)))))
    )
    (setq str (concat str "---\n"))))
    ;; (message str)))

(defconst ruhoh-temp-buffer-for-post "*Org HTML Export*")
(defconst ruhoh-org-level 3)
(defun org/ruhoh-parse-post-to-buffer(&optional buf)
  (when (not buf)
      (setq buf (get-buffer-create ruhoh-temp-buffer-for-post)))
  (let ((output (ruhoh-post-meta-infomations)))
    (save-current-buffer
      (org-html-export-as-html nil nil nil t))
    (message "after parse current-buffer is %s" (buffer-name))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert output))))

(defun org/ruhoh-parse-post-to-temp-buffer()
  (interactive)
  (org/ruhoh-parse-post-to-buffer ruhoh-temp-buffer-for-post))

(defun org/ruhoh-publish-post-internal(path)
  (org/ruhoh-parse-post-to-buffer)
  (let ((str (with-current-buffer ruhoh-temp-buffer-for-post
	       (buffer-string)))
	processed-str)
    ;; (message "current-buffer is %s" (buffer-name))
    (setq processed-str (ruhoh/wp-upload-files-replace-urls str))
    (with-current-buffer ruhoh-temp-buffer-for-post
      (erase-buffer)
      (insert processed-str)
      (write-file path)
      (kill-buffer (current-buffer)))))

(defconst org/ruhoh-file-extend 
  "html")

;; these two are two strong, so I change it!
;; (defun org-get-post-id()
;;   (interactive)
;;   (goto-char (point-min))
;;   (re-search-forward "#\\+\\([Pp][oO][sS][tT][iI][dD]\\):[^0-9]*\\([0-9]+\\)" nil t)
;;   (message (ruhoh-erase-end-spaces (match-string-no-properties 2))))
;; (defun ruhoh-get-post-id()
;;   (interactive)
;;   (goto-char (point-min))
;;   (re-search-forward "\\([Pp][oO][sS][tT][iI][dD]\\):[^0-9]*\\([0-9]+\\)" nil t)	
;;   (message (ruhoh-erase-end-spaces (match-string-no-properties 2))))
(defun org-get-post-id()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "#\\+\\([Pp][oO][sS][tT][iI][dD]\\):\s*\\(.+\\)$" nil t)
  (ruhoh-erase-end-spaces (match-string-no-properties 2)))
(defun ruhoh-get-post-id()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "\\([Pp][oO][sS][tT][iI][dD]\\):\s*'\\(.+\\)'" nil t)	
  (ruhoh-erase-end-spaces (match-string-no-properties 2)))
  
(defun org/ruhoh-publish-post()
  (interactive)
  (let ((path (format "%s/%s.%s" 
		     ruhoh-blog-posts-directory 
		     ts-file-basename
		     org/ruhoh-file-extend)))
    (if (not (file-exists-p path))
	(org/ruhoh-publish-post-internal path)
      (progn
	(let ((org-id (org-get-post-id)) ruhoh-id)
	  (setq ruhoh-id (with-temp-buffer (insert-file path)
					   (ruhoh-get-post-id)))
	  (message "exist same files, their <org-id, ruhoh-id> is <%s,%s>"
		   org-id ruhoh-id)
	  (if (equal org-id ruhoh-id)
	      (progn (message "update %s" path)
		     (org/ruhoh-publish-post-internal path))
	    (progn (setq path (format "%s/%s.%s.%s" 
				      ruhoh-blog-posts-directory 
				      ts-file-basename
				      (ruhoh-unique-guid)
				      org/ruhoh-file-extend))
		   (org/ruhoh-publish-post-internal path))))))
    (find-file-other-window  path)))

(defun find-chrome-program-name ()
  (if (not (equal  (shell-command-to-string "which chrome") ""))
      "chrome"
    (if (not (equal  (shell-command-to-string "which google-chrome") ""))
        "google-chrome"
      (message "chrome program not found"))))
(defconst ruhoh-prefer-browser
  (find-chrome-program-name)
  "default browser for viewing ruhoh")

(defun org/ruhoh-publish-post-and-view()
  (interactive)
  (let* ((path (org/ruhoh-publish-post))
	 (basename (file-name-nondirectory (file-name-sans-extension path))))
    (ruhoh-rack-restart)
    (shell-command (format "%s http://localhost:%s"
			   ruhoh-prefer-browser
			   ruhoh-blog-port
			   ;; basename
			   ))))

(setq ts-cache-directory "~/.emacs.d/cache")
(defconst ruhoh-cache-file
  (concat ts-cache-directory "ruhoh.cache")
  "the cache file for ruhoh")

(defconst current-post-id nil
  "current post id, just for reading a post id from the cache file")

(defun ts-read-lisp-expr-from-file(file)
  (with-temp-buffer
    (if (file-exists-p file)
      (progn (insert-file file)
	     (read (current-buffer)))
      nil
    )))


(defconst org/ruhoh-prefix
  '(format 
   "\
#+TITLE: %s
#+DATE: %s\n
# ruhoh blog metadata
# catetories and tags should be wrapped with \"\" and seperate with comma
# ----
#+OPTIONS: ^:{}
#+CATEGORIES: []
#+TAGS: []
#+LAYOUT:
#+DESCRIPTION:
# ----\n"
   (read-string "File Name: ")
   (format-time-string "%Y-%m-%d")))

(defun org/ruhoh-insert-metadata ()
  (interactive)
  (goto-char 1)
  (insert (eval org/ruhoh-prefix)))

(defun org/ruhoh-new-post()
  (interactive)
  (with-current-buffer (ido-find-file)
    (insert (eval org/ruhoh-prefix))
    (let* ((args (ts-read-lisp-expr-from-file ruhoh-cache-file))
	  (current-post-id (cdr (assoc 'current-post-id args))))
      (insert (format "#+POSTID: %d"
		      current-post-id))
      (setf (cdr (assoc 'current-post-id args)) (1+ current-post-id))
      (with-temp-buffer
	(print args (current-buffer))
	(write-file ruhoh-cache-file nil))
      )))

(defun org/ruhoh-view-post ()
  (interactive)
  (shell-command (format "%s http://localhost:%s"
                         ruhoh-prefer-browser
                         ruhoh-blog-port
                         ;; basename
                         )))

(defun org/ruhoh-publish-to-github()
  (interactive)
  (shell-command (format "cd %s && git add -A && git commit -m \"%s\" && bundle exec ruhoh publish github &"                         
                         ruhoh-blog-root-directory
                         (format-time-string "%Y-%m-%d"))))

(defconst org/ruhoh-keys
  (if (company-computer-p)
      '()
    '(("u" . ruhoh-rackup)
      ("d" . ruhoh-rackdown)
      ("r" . ruhoh-rack-restart)
      ("n" . org/ruhoh-new-post)
      ("p" . org/ruhoh-publish-post)
      ("v" . org/ruhoh-view-post)
      ("g" . org/ruhoh-publish-to-github))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; duplicated, can make it shared
(defun ts-help-with-key-binds(keys)
  "get the help message from the key binds variables and read key, then exec"
  ;; get help message
  (let (msg)
    (dolist (key keys)
      (setq msg (concat msg (format "{[%s]: %s} \n" (car key) (cdr key)))))
    (message msg))

  ;; read key and get it run;
  (let* ((key (read-key))
	 (func (cdr (assoc (format "%c" key) keys))))
    (if func
	(funcall func)
      (error "key <%s> was not binded\n" key))
    ))

;; (assoc (format "%c" (read-key)) android-mode-keys)


(define-key global-map (kbd "\C-c\C-p")
  (lambda() (interactive)
    (ts-help-with-key-binds org/ruhoh-keys)))

(define-key org-mode-map (kbd "\C-c\C-p")
  (lambda() (interactive)
    (ts-help-with-key-binds org/ruhoh-keys)))

;; (defun msg-me (process event)
;;   (princ
;;    (format "Process: %s had the event `%s'" process event)))
;; (progn
;;   (set-process-sentinel (get-buffer-process ruhoh-shell-process-buffer)
;; 			'msg-me)
;;   (ruhoh-rack-restart))
;; (process-sentinel (get-buffer-process ruhoh-shell-process-buffer))
;; (shell-command-sentinel)
(defun ruhoh-unique-guid()
  (let* ((uuid (shell-command-to-string "uuidgen"))
	 (uuid-nonewline (substring uuid 0 (1- (length uuid)))))
    uuid-nonewline
    ))
;; (ruhoh-unique-guid) test
(defun ruhoh-unique-file-name (file-name)
  "use uuid to gen a guid"
  (let* ((uuid (shell-command-to-string "uuidgen"))
	 (uuid-nonewline (substring uuid 0 (1- (length uuid))))
	 (ext (file-name-extension file-name))
	 (basename (file-name-sans-extension (file-name-nondirectory file-name))))
    (format "%s-%s.%s" basename uuid-nonewline ext)
    )
  )
(defun ruhoh-unique-media-file-copy (file-name)
  (let* ((uuid-name (ruhoh-unique-file-name file-name))
	(to (format "%s/%s" ruhoh-blog-media-directory uuid-name)))
    (when (not (file-exists-p to))
	(if (not (file-exists-p file-name))
	    (message (format "Error when try to copy %s to %s: %s not exist\n" file-name to file-name))
	  (copy-file file-name to))
	  )
    (concat "{{urls.media}}/" uuid-name)
    ))

(defun ruhoh/wp-upload-files-replace-urls (text)
  "Uploads files, if any in the html, and changes their links"
  (let ((file-all-urls nil)
        file-name file-web-url beg
        (file-regexp "<a href=\"\\(.?*\\)\"\\|<img src=\"\\(.*?\\)\""))
    (save-excursion
      (while (string-match file-regexp text beg)
        (setq file-name
              (if (match-beginning 1)
                  (substring text (match-beginning 1) (match-end 1))
                (substring text (match-beginning 2) (match-end 2))))
        (setq file-name (save-match-data (if (string-match "^file:" file-name)
                                             (substring file-name 7)
                                           file-name)))
        (setq beg (match-end 0))
        (if (save-match-data (not (or
                                   (string-match org-plain-link-re file-name)
                                   (string-match "^#" file-name)
                                   (string-equal (file-name-nondirectory file-name) ""))))

            (progn
              (goto-char (point-min))
              (if (re-search-forward (concat "^#\\+"
                                             (regexp-quote file-name)
                                             " ") nil t 1)
                  (setq file-web-url (buffer-substring-no-properties
                                      (point)
                                      (or (end-of-line) (point))))
                (setq file-web-url (ruhoh-unique-media-file-copy file-name))
                (goto-char (point-max))
                (newline)
                (insert (concat "#+" file-name " " file-web-url)))
              (setq file-all-urls
                    (append file-all-urls (list (cons
                                                 file-name file-web-url)))))))

      (dolist (file file-all-urls)
        (setq text (replace-regexp-in-string
                    (concat "\\(<a href=\"\\|<img src=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                    (concat "\\1" (cdr file)) text))))
    text))
(provide 'init-ruhoh)
