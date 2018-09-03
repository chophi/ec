;;; eopengrok.el --- opengrok interface for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.5.0
;; Package-Version: 20160903.2359
;; Keywords: tools
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (magit "2.1.0") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; opengrok interface for Emacs
;;
;; See documentation on https://github.com/youngker/eopengrok.el

;;; Code:

(require 's)
(require 'dash)
(require 'etags)
(require 'magit)
(require 'cl-lib)

(defvar eopengrok-pending-output nil)
(defvar eopengrok-last-filename nil)
(defvar eopengrok-page nil)
(defvar eopengrok-mode-line-status 'not-running)

(defun eopengrok-get-workspace-name (dir type)
  (let ((workspace (cu-dir-to-sha1 (file-truename dir))))
    (case type
      (:index-process (format "opengrok-indexer[%s]" workspace))
      (:index-buffer (format "*opengrok-indexer[%s]*" workspace))
      (:search-process (format "opengrok-searcher[%s]" workspace))
      (:search-buffer (format "*opengrok-searcher[%s]*" workspace))
      (t "invalid"))))

(defun eopengrok-workspace-name-p (name type)
  (case type
    (:index-process (cu-seq-starts-with name "opengrok-indexer"))
    (:index-buffer (cu-seq-starts-with name "*opengrok-indexer"))
    (:search-process (cu-seq-starts-with name "opengrok-searcher"))
    (:search-buffer (cu-seq-starts-with name "*opengrok-searcher"))
    (t nil)))

(defun eopengrok-current-searcher ()
  (unless (boundp 'eopengrok-cwd)
    (error "eopengrok-cwd is not bound"))
  (get-buffer-process (eopengrok-get-workspace-name eopengrok-cwd :search-buffer)))

(defun eopengrok-current-search-buffer ()
  (unless (boundp 'eopengrok-cwd)
    (error "eopengrok-cwd is not bound"))
  (eopengrok-get-workspace-name eopengrok-cwd :search-buffer))

(defconst eopengrok-script-name "opengrok.sh")
(defconst eopengrok-history-regexp
  "^\\([[:lower:][:upper:]]?:?.*?\\)::[ \t]+\\(\\w+\\)\\(.*\\)")

(defconst eopengrok-source-regexp
  "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\) \\[\\(.*\\)\\]")

(defconst eopengrok-file-regexp
  "^\\([[:lower:][:upper:]]?:?.*?\\):\\(.*\\)")

(defconst eopengrok-collect-the-rest
  "^Collect the rest (y/n).*")

(defgroup eopengrok nil
  "Opengrok interface for emacs."
  :prefix "eopengrok-"
  :group 'applications)

(defcustom eopengrok-configuration
  ".opengrok/configuration.xml"
  "Configuration file."
  :type 'string
  :group 'eopengrok)

(defcustom eopengrok-abbreviate-filename 80
  "Abbreviate filename length."
  :type 'number
  :group 'eopengrok)

(defcustom eopengrok-mode-line '(:eval (eopengrok--mode-line-page))
  "Mode line lighter for eopengrok."
  :group 'eopengrok
  :type 'sexp
  :risky t)

(defcustom eopengrok-mode-line-prefix "EOG"
  "Mode line prefix."
  :group 'eopengrok
  :type 'string)

(defcustom eopengrok-ignore-file-or-directory
  ".opengrok:out:*.so:*.a:*.o:*.gz:*.bz2:*.jar:*.zip:*.class:*.elc"
  "Ignore files or directories."
  :group 'eopngrok
  :type 'string)

(defcustom eopengrok-ignore-list
  '("d:.opengrok"
    "d:out"
    "d:.out"
    "d:output"
    "d:.scripts"
    "d:.repo"
    "d:.log"
    "d:.git"
    "f:*.so"
    "f:*.a"
    "f:*.o"
    "f:*.gz"
    "f:*.bz2"
    "f:*.tgz"
    "f:*.tar.gz"
    "f:*.zip"
    "f:*.jar"
    "f:*.class"
    "f:*.elc"
    "f:*.bin"
    "f:*.elf")
  "Ignore file or directory list."
  :group 'eopngrok
  :type 'string)

(defface eopengrok-file-face
  '((t :inherit font-lock-function-name-face))
  "Face for files."
  :group 'eopengrok)

(defface eopengrok-info-face
  '((t :inherit font-lock-constant-face))
  "Face for info."
  :group 'eopengrok)

(defface eopengrok-source-face
  '((t :inherit font-lock-doc-face))
  "Face for source."
  :group 'eopengrok)

(defface eopengrok-highlight-face
  '((t :inherit highlight))
  "Face for highlight item."
  :group 'eopengrok)

(defun eopengrok-resume ()
  "Resume *eopengrok* buffer."
  (interactive)
  (when (get-buffer (eopengrok-current-search-buffer))
    (pop-to-buffer (eopengrok-current-search-buffer))))

(defun eopengrok-quit ()
  "Quit eopengrok-mode."
  (interactive)
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (when (process-live-p proc)
      (kill-process proc)
      (sleep-for 0.1))
    (kill-buffer buf)))

(defun eopengrok-visit-nearest-ancestor-link ()
  (interactive)
  (let ((possible-dir (cu-find-nearest-ancestor-link-in
            eopengrok-database-root-dir default-directory)))
    (if possible-dir
        (find-file possible-dir))))

(defun eopengrok-get-source-config-alist ()
  (interactive)
  (mapcar
   (lambda (dir)
     (cons (file-chase-links
            (cu-join-path eopengrok-database-root-dir dir "source"))
           (cu-join-path eopengrok-database-root-dir dir)))
   (cddr (directory-files eopengrok-database-root-dir))))

(defun eopengrok-choose-projects-from-database ()
  (interactive)
  (let ((source-config-alist
         (eopengrok-get-source-config-alist)))
    (setq eopengrok-default-project-alist-from-database
          (assoc (ido-completing-read
                  "Choose a project: "
                  (mapcar 'car source-config-alist))
                 source-config-alist))))

(defconst eopengrok-file-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cu-open-link)
    (define-key map (kbd "RET") #'cu-open-link)
    (define-key map "\C-j" #'cu-open-link)
    map))

(defun eopengrok-list-projects ()
  (interactive)
  (let ((buf (get-buffer-create "*eopengrok-project-list*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (dolist (lst (eopengrok-get-source-config-alist))
        (insert (propertize (concat (car lst) ":\n")
                            'face 'eopengrok-source-face
                            'keymap eopengrok-file-link-map)
                (propertize (concat  "\t" (cdr lst) "\n")
                            'face 'eopengrok-source-face
                            'keymap eopengrok-file-link-map))))
    (switch-to-buffer buf)
    (read-only-mode)))

(defvar eopengrok-default-project-alist-from-database nil
  "default project alist from database")

(defun* eopengrok-has-database-source-of-dir (dir)
  (let ((ret))
    (dolist (lst (eopengrok-get-source-config-alist))
      (when (string-match-p (file-truename (car lst)) (expand-file-name dir))
        (return-from eopengrok-has-database-source-of-dir (cdr lst))))
    ret))

(defun* eopengrok-was-symbol-linked-under-a-project (dir)
  (let ((ret))
    (dolist (lst (eopengrok-get-source-config-alist))
      (when (cu-search-child-symlink-recursively-in
             (file-truename (cu-join-path (cdr lst) "source")) dir 1)
        (return-from eopengrok-was-symbol-linked-under-a-project (cdr lst))))
    ret))

(defun* eopengrok--get-configuration ()
  "Search for Project configuration.xml.
Return CONS of paths: (ROOT . CONFIGURATION)"
  (interactive)
  (let ((exist-and-configuration-exist-p
         (lambda (dir)
           (when (and dir (file-exists-p (cu-join-path dir eopengrok-configuration)))
             (cons
              (file-truename (cu-join-path dir "source"))
              (cu-join-path dir eopengrok-configuration))))))
    (or (funcall exist-and-configuration-exist-p
                 (cu-find-nearest-ancestor-link-in
                  eopengrok-database-root-dir default-directory))
        ;; The source code project itself is a symbol link.
        (funcall exist-and-configuration-exist-p
                 (eopengrok-has-database-source-of-dir default-directory))
        ;; The project has no opengrok indexed database, however it was
        ;; symbolinked to an already indexed project.
        ;; So we can use that project to search the files.
        (funcall exist-and-configuration-exist-p
                 (eopengrok-was-symbol-linked-under-a-project default-directory))
        (user-error "Can't find configuration.xml"))))

(defun eopengrok--search-option (conf text option symbol)
  "Opengrok search option list with CONF TEXT OPTION SYMBOL."
  (if (eq symbol 'custom)
      (-flatten (list "search" "-R" conf (split-string text " " t)))
    (if (equal "-f" option)
        (list "search_full_text" "-R" conf option (format "\"%s\"" text))
      (list "search" "-R" conf option text))))

(defmacro eopengrok--properties-region (props &rest body)
  "Add PROPS and Execute BODY to all the text it insert."
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun eopengrok--get-properties (pos)
  "Get properties at POS."
  (list (get-text-property pos :name)
        (get-text-property pos :info)))

(defun eopengrok--show-source ()
  "Display original source."
  (with-current-buffer (eopengrok-current-search-buffer)
    (-when-let* (((file number) (eopengrok--get-properties (point))))
      (let* ((buffer (find-file-noselect file))
             (window (display-buffer buffer)))
        (set-buffer buffer)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- number)))
        (set-window-point window (point))
        window))))

(defun eopengrok--show-commit (noselect)
  "Display magit-show-commit with NOSELECT."
  (-when-let* (((file commit-id) (eopengrok--get-properties (point))))
    (setq default-directory (file-name-directory file))
    (let ((magit-display-buffer-noselect noselect))
      (magit-git-string "rev-parse" "--show-toplevel")
      (magit-show-commit commit-id))))

(defun eopengrok-jump-to-source ()
  "Jump point to the other window."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (-when-let (info (get-text-property (point) :info))
      (if (numberp info)
          (-when-let (window (eopengrok--show-source))
            (select-window window)
            (ring-insert find-tag-marker-ring (point-marker)))
        (eopengrok--show-commit nil)))))

(defun eopengrok-next-line ()
  "Move point to the next search result, if one exists."
  (interactive)
  (with-current-buffer (eopengrok-current-search-buffer)
    (-when-let (pos (next-single-property-change
                     (save-excursion (end-of-line) (point)) :info))
      (goto-char pos)
      (if (numberp (get-text-property pos :info))
          (eopengrok--show-source)
        (eopengrok--show-commit t)))))

(defun eopengrok-previous-line ()
  "Move point to the previous search result, if one exists."
  (interactive)
  (with-current-buffer (eopengrok-current-search-buffer)
    (-when-let (pos (previous-single-property-change
                     (save-excursion (beginning-of-line) (point)) :info))
      (goto-char pos)
      (beginning-of-line)
      (if (numberp (get-text-property (point) :info))
          (eopengrok--show-source)
        (eopengrok--show-commit t)))))

(defun eopengrok--abbreviate-file (file)
  "Abbreviate FILE name."
  (let* ((start (- (point) (length file)))
         (end (point))
         (amount (if (numberp eopengrok-abbreviate-filename)
                     (- (- end start) eopengrok-abbreviate-filename)
                   999))
         (advance-word (lambda ()
                         "Return the length of the text made invisible."
                         (let ((wend (min end (progn (forward-word 1) (point))))
                               (wbeg (max start (progn (backward-word 1) (point)))))
                           (goto-char wend)
                           (if (<= (- wend wbeg) 1)
                               0
                             (put-text-property (1+ wbeg) wend 'invisible t)
                             (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (cl-decf amount (funcall advance-word)))
    (goto-char end)))

(defun eopengrok--remove-html-tags (text)
  "Remove html tag from TEXT."
  (->> text
       (replace-regexp-in-string "<[^>]*>" "")
       (s-replace-all '(("&lt;" . "<") ("&gt;" . ">")
                        ("&amp;" . "&") ("\r" . "")))))

(defun eopengrok--text-highlight (text line)
  "Highlighting TEXT from LINE."
  (let (beg end)
    (with-temp-buffer
      (insert (propertize line 'read-only nil))
      (goto-char (point-min))
      (while (and (re-search-forward
                   (s-replace "\"" "" text) nil t)
                  (> (- (setq end (match-end 0))
                        (setq beg (match-beginning 0))) 0))
        (add-text-properties beg end '(face eopengrok-highlight-face)))
      (buffer-string))))

(defun eopengrok--handle-mouse (_event)
  "Handle mouse click EVENT."
  (interactive "e")
  (eopengrok-jump-to-source))

(defvar eopengrok-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'eopengrok--handle-mouse)
    map))

(defun eopengrok--file-line-properties (file rest)
  "Decorate File link."
  (let* ((file (propertize file 'face 'eopengrok-file-face))
         (info (propertize " "
                           'face 'eopengrok-info-face
                           'mouse-face 'highlight
                           'keymap eopengrok-mouse-map))
         (proc (eopengrok-current-searcher)))
    (eopengrok--properties-region
     (list :page eopengrok-page)
     (eopengrok--properties-region
      (list :name (expand-file-name file)
            :info (string-to-number info))
      (insert info))
     (progn
       (unless (string= file eopengrok-last-filename)
         (insert file)
         (eopengrok--abbreviate-file file))
       (insert (format "%s\n" rest))
       (setq eopengrok-last-filename file)))))

(defun eopengrok--line-properties (line-list &optional history)
  "Decorate LINE-LIST with HISTORY."
  (-when-let* (((file info src) line-list)
               (file (propertize file 'face 'eopengrok-file-face))
               (info (propertize info
                                 'face 'eopengrok-info-face
                                 'mouse-face 'highlight
                                 'keymap eopengrok-mouse-map))
               (src (propertize (eopengrok--remove-html-tags src)
                                'face 'eopengrok-source-face))
               (proc (eopengrok-current-searcher)))
    (eopengrok--properties-region
     (list :page eopengrok-page)
     (progn
       (unless (string= file eopengrok-last-filename)
         (insert (format "\n%s\n" file))
         (eopengrok--abbreviate-file file))
       (eopengrok--properties-region
        (list :name (expand-file-name file)
              :info (if history info (string-to-number info)))
        (insert (concat (format "%08s" info) " "
                        (eopengrok--text-highlight
                         (process-get proc :text)
                         src))))
       (insert "\n")
       (setq eopengrok-last-filename file)))))

(defun eopengrok--insert-line (line process)
  "Insert matching any regex in LINE."
  (cond
   ((string-match eopengrok-history-regexp line)
    (eopengrok--line-properties
     (mapcar (lambda (n) (match-string n line)) '(1 2 3)) t))
   ((string-match eopengrok-source-regexp line)
    (eopengrok--line-properties
     (mapcar (lambda (n) (match-string n line)) '(1 2 3))))
   ((and (string-match eopengrok-file-regexp line) (file-exists-p (match-string 1 line)))
    (eopengrok--file-line-properties
     (match-string 1 line) (match-string 2 line)))
   ((string-match eopengrok-collect-the-rest line)
    (process-send-string process "y\n"))
   (t (insert line "\n"))))

(defun eopengrok--process-filter (process output)
  "Process eopengrok output from PROCESS containted in OUTPUT."
  (with-current-buffer (process-buffer process)
    (let ((buffer-read-only nil)
          (pos 0)
          (output (concat eopengrok-pending-output output)))
      (save-excursion
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (goto-char (point-max))
            ;;(insert line "\n")
            (eopengrok--insert-line line process))))
      (setq eopengrok-pending-output (substring output pos)))))

(defun eopengrok--process-sentinel (process event)
  "Handle eopengrok PROCESS EVENT."
  (let ((buf (process-buffer process)))
    (with-current-buffer buf
      (cond ((string= "killed\n" event)
             (kill-buffer buf))
            ((string= "finished\n" event)
             (setq eopengrok-mode-line-status 'finished)
             (unless (eopengrok-workspace-name-p (buffer-name buf) :search-buffer)
               (kill-buffer buf)))
            (t nil)))))

(defun eopengrok--current-info (process dir &optional search text ep inhabit-pop-buffer)
  "Display current information (PROCESS DIR SEARCH TEXT EP)."
  (let ((buf (process-buffer process)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (erase-buffer)
        (if search
            (insert text)
          (insert (concat "Creating the index" (and ep " (enable projects)"))))
        (insert (format "\nDirectory: %s\n" dir))
        (forward-line -2))
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (unless inhabit-pop-buffer
        (pop-to-buffer buf)))))

(defun eopengrok--init ()
  "Initialize variable."
  (setq eopengrok-last-filename nil
        eopengrok-pending-output nil
        eopengrok-page nil
        eopengrok-mode-line-status 'not-running))

(defmacro eopengrok-define-find (sym option)
  "Make function with SYM and OPTION."
  (let ((fun (intern (format "eopengrok-find-%s" sym)))
        (doc (format "Find option %s" option))
        (str (format "Find %s: " sym)))
    `(defun ,fun () ,doc
            (interactive)
            (let* ((source-conf-cons (eopengrok--get-configuration))
                   (dir (car source-conf-cons))
                   (conf (cdr source-conf-cons))
                   (proc (eopengrok-get-workspace-name dir :search-process))
                   (text (read-string ,str (thing-at-point 'symbol))))
              (when (process-live-p (get-process proc))
                (kill-process (get-process proc))
                (sleep-for 0.1))
              (let* ((proc (apply 'start-process
                                  (eopengrok-get-workspace-name dir :search-process)
                                  (eopengrok-get-workspace-name dir :search-buffer)
                                  eopengrok-script-name
                                  (eopengrok--search-option conf text
                                                            ,option ',sym))))
                (set-process-query-on-exit-flag proc nil)
                (set-process-filter proc 'eopengrok--process-filter)
                (set-process-sentinel proc 'eopengrok--process-sentinel)
                (process-put proc :text text)
                (with-current-buffer (eopengrok-get-workspace-name dir :search-buffer)
                  (eopengrok-mode t)
                  (eopengrok--init)
                  (setq-local eopengrok-cwd dir)
                  (eopengrok--current-info
                   proc (s-chop-suffix eopengrok-configuration conf)
                   t (concat ,str text))))))))

(eopengrok-define-find definition "-d")
(eopengrok-define-find file "-p")
(eopengrok-define-find reference "-r")
(eopengrok-define-find text "-f")
(eopengrok-define-find history "-h")
(eopengrok-define-find custom "")

(defconst eopengrok-database-root-dir (expand-file-name "~/.opengrok-data-base")
  "The default directory for the opengrok database")

(defun create-database-dir-if-not-exist (dir)
  (when (not (file-directory-p dir))
    (error "%s is not a directory" dir))
  (let* ((sha1-dir (cu-dir-to-sha1 dir))
         (absolute-path-sha1-dir
          (cu-join-path eopengrok-database-root-dir sha1-dir))
         (source-dir (cu-join-path absolute-path-sha1-dir "source")))
    (when (not (file-exists-p absolute-path-sha1-dir))
      (make-directory absolute-path-sha1-dir))
    (make-symbolic-link dir source-dir t)
    (list (file-chase-links source-dir) absolute-path-sha1-dir)))

(defvar eopengrok-create-index-quite-mode t
  "crate index as quiet as possible")

(defconst eopengrok-enable-projects-p nil
  "If enabled, every project in the root directory will be considered as a separate project")

(defun eopengrok-create-index (dir &optional sentinel inhabit-pop-buffer)
  "Create an Index file in DIR, optionally the caller can pass in a customized SENTINEL"
  (interactive "DRoot directory: ")
  (let* ((dir (file-truename dir))
         (index-buffer-name (eopengrok-get-workspace-name dir :index-buffer))
         (proc (apply 'start-process
                      (eopengrok-get-workspace-name dir :index-process)
                      index-buffer-name
                      eopengrok-script-name
                      (append (list "index")
                              (when eopengrok-create-index-quite-mode
                                (list "-q"))
                              (let ((source-dir-pair (create-database-dir-if-not-exist dir)))
                                (list "-s" (car source-dir-pair)
                                      "-d" (cu-join-path (cadr source-dir-pair) ".opengrok")
                                      "-W" (cu-join-path (cadr source-dir-pair) eopengrok-configuration)))
                              (seq-reduce
                               (lambda (init ele)
                                 (if (and (stringp ele) (not (equal ele "")))
                                     (append (list "-i" ele) init)
                                   init))
                               eopengrok-ignore-list
                               nil)
                              (when eopengrok-enable-projects-p '("-e"))))))
    (set-process-filter proc 'eopengrok--process-filter)
    (set-process-sentinel proc (or sentinel 'eopengrok--process-sentinel))
    (with-current-buffer index-buffer-name
      (eopengrok-mode t)
      (eopengrok--init)
      (setq-local eopengrok-cwd dir)
      (eopengrok--current-info proc (expand-file-name dir)
                               nil nil eopengrok-enable-projects-p inhabit-pop-buffer)
      (setq eopengrok-mode-line-status 'running))))

(defvar eopengrok-mode-map nil
  "Keymap for eopengrok minor mode.")

(unless eopengrok-mode-map
  (setq eopengrok-mode-map (make-sparse-keymap)))

(--each '(("n"        . eopengrok-next-line)
          ("p"        . eopengrok-previous-line)
          ("q"        . eopengrok-quit)
          ("<return>" . eopengrok-jump-to-source))
  (define-key eopengrok-mode-map (read-kbd-macro (car it)) (cdr it)))

(defun eopengrok--mode-line-page ()
  "Return the string of page number."
  (let ((page (or (get-text-property (point) :page)
                  eopengrok-page))
        (status (pcase eopengrok-mode-line-status
                  (`running "*:") (`finished ":") (`not-running ""))))
    (concat " " eopengrok-mode-line-prefix status page)))

(define-minor-mode eopengrok-mode
  "Minor mode for opengrok."
  :keymap eopengrok-mode-map
  :lighter eopengrok-mode-line)

(provide 'eopengrok)

;;; eopengrok.el ends here
