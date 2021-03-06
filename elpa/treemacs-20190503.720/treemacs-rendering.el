;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; Code in this file is considered performance critical.
;;; The usual restrictions w.r.t quality, readability and maintainability are
;;; lifted here.

;;; Code:

(require 's)
(require 'ht)
(require 'cl-lib)
(require 'treemacs-core-utils)
(require 'treemacs-icons)
(require 'treemacs-async)
(require 'treemacs-customization)
(require 'treemacs-dom)
(require 'treemacs-workspaces)
(eval-and-compile
  (require 'treemacs-macros)
  (require 'inline))

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--start-watching
  treemacs--stop-watching)

(treemacs-import-functions-from "treemacs-tags"
  treemacs--goto-tag-button-at
  treemacs--tags-path-of)

(treemacs-import-functions-from "treemacs-interface"
  treemacs-TAB-action)

(treemacs-import-functions-from "treemacs-extensions"
  treemacs--apply-root-top-extensions
  treemacs--apply-root-bottom-extensions
  treemacs--apply-project-top-extensions
  treemacs--apply-project-bottom-extensions
  treemacs--apply-directory-top-extensions
  treemacs--apply-directory-bottom-extensions)

(defvar-local treemacs--projects-end nil
  "Marker pointing to position at the end of the last project.

If there are no projects, points to the position at the end of any top-level
extensions positioned to `TOP'. This can always be used as the insertion point
for new projects.")

(define-inline treemacs--projects-end ()
  "Importable getter for `treemacs--projects-end'."
  (declare (side-effect-free t))
  (inline-quote treemacs--projects-end))

(define-inline treemacs--button-at (pos)
  "Return the button at position POS in the current buffer, or nil.
If the button at POS is a text property button, the return value
is a marker pointing to POS."
  (declare (side-effect-free t))
  (inline-letevals (pos)
    (inline-quote (copy-marker ,pos t))))

(define-inline treemacs--current-screen-line ()
  "Get the current screen line in the selected window."
  (declare (side-effect-free t))
  (inline-quote
   (max 1 (count-screen-lines (window-start) (point-at-eol)))))

(define-inline treemacs--lines-in-window ()
  "Determine the number of lines visible in the current (treemacs) window.
A simple call to something like `window-screen-lines' is insufficient becase
the height of treemacs' icons must be taken into account."
  (declare (side-effect-free t))
  (inline-quote
   (/ (- (window-pixel-height) (window-mode-line-height))
      (max treemacs--icon-size (frame-char-height)))))

(define-inline treemacs--sort-alphabetic-asc (f1 f2)
  "Sort F1 and F2 alphabetically asc."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp ,f2 ,f1))))

(define-inline treemacs--sort-alphabetic-desc (f1 f2)
  "Sort F1 and F2 alphabetically desc."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp ,f1 ,f2))))

(define-inline treemacs--sort-alphabetic-case-insensitive-asc (f1 f2)
  "Sort F1 and F2 case insensitive alphabetically asc."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp (downcase ,f2) (downcase ,f1)))))

(define-inline treemacs--sort-alphabetic-case-insensitive-desc (f1 f2)
  "Sort F1 and F2 case insensitive alphabetically desc."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp (downcase ,f1) (downcase ,f2)))))

(define-inline treemacs--sort-size-asc (f1 f2)
  "Sort F1 and F2 by size asc."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote
     (>= (nth 7 (file-attributes ,f1))
         (nth 7 (file-attributes ,f2))))))

(define-inline treemacs--sort-size-desc (f1 f2)
  "Sort F1 and F2 by size desc."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote
     (< (nth 7 (file-attributes ,f1))
        (nth 7 (file-attributes ,f2))))))

(define-inline treemacs--sort-mod-time-asc (f1 f2)
  "Sort F1 and F2 by modification time asc."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (file-newer-than-file-p ,f1 ,f2))))

(define-inline treemacs--sort-mod-time-desc (f1 f2)
  "Sort F1 and F2 by modification time desc."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (file-newer-than-file-p ,f2 ,f1))))

(define-inline treemacs--insert-root-separator ()
  "Insert a root-level separator at point, moving point after the separator."
  (inline-quote
   (insert (if treemacs-space-between-root-nodes "\n\n" "\n"))))

(define-inline treemacs--get-dir-content (dir)
  "Get the content of DIR, separated into sublists of first dirs, then files."
  (inline-letevals (dir)
    (inline-quote
     ;; `directory-files' is much faster in a temp buffer for whatever reason
     (with-temp-buffer
       (let* ((file-name-handler-alist '(("\\`/[^/|:]+:" . tramp-autoload-file-name-handler)))
              (sort-func
               (pcase treemacs-sorting
                 ('alphabetic-asc #'treemacs--sort-alphabetic-asc)
                 ('alphabetic-desc #'treemacs--sort-alphabetic-desc)
                 ('alphabetic-case-insensitive-asc  #'treemacs--sort-alphabetic-case-insensitive-asc)
                 ('alphabetic-case-insensitive-desc #'treemacs--sort-alphabetic-case-insensitive-desc)
                 ('size-asc #'treemacs--sort-size-asc)
                 ('size-desc #'treemacs--sort-size-desc)
                 ('mod-time-asc #'treemacs--sort-mod-time-asc)
                 ('mod-time-desc #'treemacs--sort-mod-time-desc)
                 (other other)))
              (entries (-> ,dir (directory-files :absolute-names nil :no-sort) (treemacs--filter-files-to-be-shown)))
              (dirs-files (-separate #'file-directory-p entries)))
         (list (sort (cl-first dirs-files) sort-func)
               (sort (cl-second dirs-files) sort-func)))))))

(define-inline treemacs--create-dir-button-strings (path prefix parent depth)
  "Return the text to insert for a directory button for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is."
  ;; for directories the icon is included in the prefix since it's always known
  (inline-letevals (path prefix parent depth)
    (inline-quote
     (list
      ,prefix
      (propertize (file-name-nondirectory ,path)
                  'button '(t)
                  'category 'default-button
                  'help-echo nil
                  'keymap nil
                  :default-face 'treemacs-directory-face
                  :state 'dir-node-closed
                  :path ,path
                  :key ,path
                  :symlink (file-symlink-p ,path)
                  :parent ,parent
                  :depth ,depth)))))

(define-inline treemacs--create-file-button-strings (path prefix parent depth)
  "Return the text to insert for a file button for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is."
  (inline-letevals (path prefix parent depth)
    (inline-quote
     (list
      ,prefix
      (treemacs-icon-for-file ,path)
      (propertize (file-name-nondirectory ,path)
                  'button '(t)
                  'category 'default-button
                  'help-echo nil
                  'keymap nil
                  :default-face 'treemacs-git-unmodified-face
                  :state 'file-node-closed
                  :path ,path
                  :key ,path
                  :parent ,parent
                  :depth ,depth)))))

(cl-defmacro treemacs--button-open (&key button new-state new-icon open-action post-open-action immediate-insert)
  "Building block macro to open a BUTTON.
Gives the button a NEW-STATE, and, optionally, a NEW-ICON. Performs OPEN-ACTION
and, optionally, POST-OPEN-ACTION. If IMMEDIATE-INSERT is non-nil it will concat
and apply `insert' on the items returned from OPEN-ACTION. If it is nil either
OPEN-ACTION or POST-OPEN-ACTION are expected to take over insertion."
  `(save-excursion
     (-let [p (point)]
       (treemacs-with-writable-buffer
        (treemacs-button-put ,button :state ,new-state)
        ,@(when new-icon
            `((beginning-of-line)
              (treemacs--button-symbol-switch ,new-icon)))
        (goto-char (treemacs-button-end ,button))
        ,@(if immediate-insert
              `((progn
                  (insert (apply #'concat ,open-action))))
            `(,open-action))
        ,post-open-action)
       (count-lines p (point)))))

(cl-defmacro treemacs--create-buttons (&key nodes depth extra-vars node-action node-name)
  "Building block macro for creating buttons from a list of items.
Will not making any insertions, but instead return a list of strings returned by
NODE-ACTION, so that the list can be further manipulated and efficiently
inserted in one go.
NODES is the list to create buttons from.
DEPTH is the indentation level buttons will be created on.
EXTRA-VARS are additional var bindings inserted into the initial let block.
NODE-ACTION is the button creating form inserted for every NODE.
NODE-NAME is the variable individual nodes are bound to in NODE-ACTION."
  `(let* ((depth ,depth)
          (prefix (concat "\n" (s-repeat (* depth treemacs-indentation) treemacs-indentation-string)))
          (,node-name (cl-first ,nodes))
          (strings)
          ,@extra-vars)
     (when ,node-name
       (dolist (,node-name ,nodes)
         (--each ,node-action
           (push it strings))))
     (nreverse strings)))

(defun treemacs--collapse-dirs (dirs)
  "Display DIRS as collpased.
Go to each dir button, expand its label with the collapsed dirs, set its new
path and give it a special parent-patX property so opening it will add the
correct cache entries.

DIRS: List of Collapse Paths. Each Collapse Path is a list of
 1) The original,full and uncollapsed path,
 2) the extra text that must be appended in the view,
 3) a series of intermediate steps which are the result of appending the
    collapsed path elements onto the original, ending in
 4) the full path to the
    directory that the collapsing leads to. For Example:
\(\"/home/a/Documents/git/treemacs/.cask\"
 \"/26.0/elpa\"
 \"/home/a/Documents/git/treemacs/.cask/26.0\"
 \"/home/a/Documents/git/treemacs/.cask/26.0/elpa\"\)"
  (when dirs
    (-let [project (-> dirs (car) (car) (treemacs--find-project-for-path))]
      (dolist (it dirs)
        ;; use when-let because the operation may fail when we try to move to a node
        ;; that us not visible because treemacs ignores it
        (-when-let (b (treemacs-goto-file-node (car it) project))
          ;; no warning since filewatch mode is known to be defined
          (when (with-no-warnings treemacs-filewatch-mode)
            (treemacs--start-watching (car it))
            (dolist (step (nthcdr 2 it))
              (treemacs--start-watching step t)))
          (let ((props (text-properties-at (treemacs-button-start b)))
                (new-path (nth (- (length it) 1) it)))
            (treemacs-button-put b :path new-path)
            ;; if the collapsed path leads to a symlinked directory the button needs to be marked as a symlink
            ;; so `treemacs--expand-dir-node' will know to start a new git future under its true-name
            (treemacs-button-put b :symlink (or (treemacs-button-get b :symlink)
                                                (--first (file-symlink-p it)
                                                         (cdr it))))
            ;; number of directories that have been appended to the original path
            ;; value is used in `treemacs--follow-each-dir'
            (treemacs-button-put b :collapsed (- (length it) 2))
            (end-of-line)
            (let* ((beg (point))
                   (dir (cadr it))
                   (parent (file-name-directory dir)))
              (insert dir)
              (add-text-properties beg (point) props)
              (add-text-properties
               (treemacs-button-start b) (+ beg (length parent))
               '(face treemacs-directory-collapsed-face)))))))))

(defmacro treemacs--map-when-unrolled (items interval &rest mapper)
  "Unrolled variant of dash.el's `--map-when'.
Specialized towards applying MAPPER to ITEMS on a given INTERVAL."
  (declare (indent 2))
  `(let* ((ret nil)
          (--items-- ,items)
          (reps (/ (length --items--) ,interval))
          (--loop-- 0))
     (while (< --loop-- reps)
       ,@(-repeat
          (1- interval)
          '(setq ret (cons (pop --items--) ret)))
       (setq ret
             (-let [it (pop --items--)]
               (cons ,@mapper ret)))
       (cl-incf --loop--))
     (nreverse (nconc --items-- ret))))

(defmacro treemacs--inplace-map-when-unrolled (items interval &rest map-body)
  "Unrolled in-place mappig operation.
Applies MAP-BODY to every element in ITEMS at the given INTERVAL."
  (declare (indent 2))
  (let ((l (make-symbol "list"))
        (tail-op (cl-case interval
                   (2 'cdr)
                   (3 'cddr)
                   (4 'cdddr)
                   (_ (error "Interval %s is not handled yet" interval)))))
    `(let ((,l ,items))
       (while ,l
         (setq ,l (,tail-op ,l))
         (let ((it (pop ,l)))
           ,@map-body)))))

(define-inline treemacs--create-branch (root depth git-future collapse-process &optional parent)
  "Create a new treemacs branch under ROOT.
The branch is indented at DEPTH and uses the eventual outputs of
GIT-FUTURE to decide on file buttons' faces and COLLAPSE-PROCESS to determine
which directories should be displayed as one. The buttons' parent property is
set to PARENT."
  (inline-letevals (root depth git-future collapse-process parent)
    (inline-quote
     (save-excursion
       (let* ((dirs-and-files (treemacs--get-dir-content ,root))
              (dirs (cl-first dirs-and-files))
              (files (cl-second dirs-and-files))
              (git-info)
              (file-strings)
              (dir-strings))
         (setq dir-strings
               (treemacs--create-buttons
                :nodes dirs
                :extra-vars ((dir-prefix (concat prefix treemacs-icon-closed)))
                :depth ,depth
                :node-name node
                :node-action (treemacs--create-dir-button-strings node dir-prefix ,parent ,depth)))
         (setq file-strings
               (treemacs--create-buttons
                :nodes files
                :depth ,depth
                :node-name node
                :node-action (treemacs--create-file-button-strings node prefix ,parent ,depth)))

         (--when-let (file-truename (treemacs-button-get ,parent :path))
           (setq ,root it))

         (end-of-line)

         ;; the files list contains 3 item tuples: the prefix the icon and the filename
         ;; direcories are different, since dirs do not  have different icons the icon is part if the prefix
         ;; therefore when filtering or propertizing the files and dirs only every 3rd or 2nd item must be looked at

         ;; as reopening is done recursively the parsed git status is passed down to subsequent calls
         ;; so there are two possibilities: either the future given to this function is a pfuture object
         ;; that needs to complete and be parsed or it's an already finished git status hash table
         ;; additionally when git mode is deferred we don't parse the git output right here, it is instead done later
         ;; by means of an idle timer. The git info used is instead fetched from `treemacs--git-cache', which is
         ;; based on previous invocations
         ;; if git-mode is disabled there is nothing to do - in this case the git status parse function will always
         ;; produce an empty hash table
         (pcase treemacs-git-mode
           ((or 'simple 'extended)
            (setq git-info (treemacs--get-or-parse-git-result ,git-future)))
           ('deferred
             (setq git-info (or (ht-get treemacs--git-cache ,root) (ht)))
             (run-with-timer 0.5 nil #'treemacs--apply-deferred-git-state ,parent ,git-future (current-buffer)))
           (_
            (setq git-info (ht))))

         (when treemacs-pre-file-insert-predicates
           (-let [result nil]
             (while file-strings
               (let* ((prefix (car file-strings))
                      (icon (cadr file-strings))
                      (filename (cl-third file-strings))
                      (filepath (concat ,root "/" filename)))
                 (unless (--any? (funcall it filepath git-info) treemacs-pre-file-insert-predicates)
                   (setq result (cons filename (cons icon (cons prefix result))))))
               (setq file-strings (cl-cdddr file-strings)))
             (setq file-strings (nreverse result)))
           (-let [result nil]
             (while dir-strings
               (let* ((prefix (car dir-strings))
                      (dirname (cadr dir-strings))
                      (dirpath (concat ,root "/" dirname)))
                 (unless (--any? (funcall it dirpath git-info) treemacs-pre-file-insert-predicates)
                   (setq result (cons dirname (cons prefix result)))))
               (setq dir-strings (cddr dir-strings)))
             (setq dir-strings (nreverse result))))

         (treemacs--inplace-map-when-unrolled dir-strings 2
           (put-text-property
            0
            (length it)
            'face
            (treemacs--get-node-face (concat ,root "/" it) git-info 'treemacs-directory-face)
            it))
         (insert (apply #'concat dir-strings))

         (end-of-line)
         (treemacs--inplace-map-when-unrolled file-strings 3
           (put-text-property
            0
            (length it)
            'face
            (treemacs--get-node-face (concat ,root "/" it) git-info 'treemacs-git-unmodified-face)
            it))
         (insert (apply #'concat file-strings))

         (save-excursion
           (treemacs--collapse-dirs (treemacs--parse-collapsed-dirs ,collapse-process))
           (treemacs--reopen-at ,root ,git-future))
         (point-at-eol))))))

(cl-defmacro treemacs--button-close (&key button new-state new-icon post-close-action)
  "Close node given by BTN, use NEW-ICON and set state of BTN to NEW-STATE."
  `(save-excursion
     (treemacs-with-writable-buffer
      ,@(when new-icon
          `((treemacs--button-symbol-switch ,new-icon)))
      (treemacs-button-put ,button :state ,new-state)
      (-let [next (next-button (point-at-eol))]
        (if (or (null next)
                (/= (1+ (treemacs-button-get btn :depth))
                    (treemacs-button-get (copy-marker next t) :depth)))
            (delete-trailing-whitespace)
          ;; Delete from end of the current button to end of the last sub-button.
          ;; This will make the EOL of the last button become the EOL of the
          ;; current button, making the treemacs--projects-end marker track
          ;; properly when collapsing the last project or a last directory of the
          ;; last project.
          (let* ((pos-start (treemacs-button-end ,button))
                 (next (treemacs--next-non-child-button ,button))
                 (pos-end (if next
                              (-> next (treemacs-button-start) (previous-button) (treemacs-button-end))
                            (point-max))))
            (delete-region pos-start pos-end))))
      ,post-close-action)))

(defun treemacs--expand-root-node (btn)
  "Expand the given root BTN."
  (let ((project (treemacs-button-get btn :project)))
    (treemacs-with-writable-buffer
     (treemacs-project->refresh-path-status! project))
    (if (treemacs-project->is-unreadable? project)
        (treemacs-pulse-on-failure
            (format "%s is not readable."
                    (propertize (treemacs-project->path project) 'face 'font-lock-string-face)))
      (let* ((path (treemacs-button-get btn :path))
             (git-path (if (treemacs-button-get btn :symlink) (file-truename path) path))
             (git-future (treemacs--git-status-process git-path project))
             (collapse-future (treemacs--collapsed-dirs-process path project)))
        (treemacs--maybe-recenter treemacs-recenter-after-project-expand
          (treemacs--button-open
           :immediate-insert nil
           :button btn
           :new-state 'root-node-open
           :open-action
           (progn
             (treemacs--apply-project-top-extensions btn project)
             (goto-char (treemacs--create-branch path (1+ (treemacs-button-get btn :depth)) git-future collapse-future btn))
             (treemacs--apply-project-bottom-extensions btn project))
           :post-open-action
           (progn
             (treemacs-on-expand path btn nil)
             (treemacs--start-watching path)
             ;; Performing FS ops on a disconnected Tramp project
             ;; might have changed the state to connected.
             (treemacs-with-writable-buffer
              (treemacs-project->refresh-path-status! project)))))))))

(defun treemacs--collapse-root-node (btn &optional recursive)
  "Collapse the given root BTN.
Remove all open entries below BTN when RECURSIVE is non-nil."
  (treemacs--button-close
   :button btn
   :new-state 'root-node-closed
   :post-close-action
   (-let [path (treemacs-button-get btn :path)]
     (treemacs--stop-watching path)
     (treemacs-on-collapse path recursive))))

(cl-defun treemacs--expand-dir-node (btn &key git-future recursive)
  "Open the node given by BTN.

BTN: Button
GIT-FUTURE: Pfuture|Hashtable
RECURSIVE: Bool"
  (if (not (f-readable? (treemacs-button-get btn :path)))
      (treemacs-pulse-on-failure
       "Directory %s is not readable." (propertize (treemacs-button-get btn :path) 'face 'font-lock-string-face))
    (let* ((project (treemacs-project-of-node btn))
           (path (treemacs-button-get btn :path))
           (git-future (if (treemacs-button-get btn :symlink)
                           (treemacs--git-status-process (file-truename path) project)
                         (or git-future (treemacs--git-status-process path project))))
           (collapse-future (treemacs--collapsed-dirs-process path project)))
      (treemacs--button-open
       :immediate-insert nil
       :button btn
       :new-state 'dir-node-open
       :new-icon treemacs-icon-open
       :open-action
       (progn
         ;; do on-expand first so buttons that need collapsing can quickly find their parent
         (treemacs-on-expand path btn (treemacs-parent-of btn))
         (treemacs--apply-directory-top-extensions btn path)
         (goto-char (treemacs--create-branch path (1+ (treemacs-button-get btn :depth)) git-future collapse-future btn))
         (treemacs--apply-directory-bottom-extensions btn path))
       :post-open-action
       (progn
         (treemacs--start-watching path)
         (when recursive
           (--each (treemacs--get-children-of btn)
             (when (eq 'dir-node-closed (treemacs-button-get it :state))
               (goto-char (treemacs-button-start it))
               (treemacs--expand-dir-node it :git-future git-future :recursive t)))))))))

(defun treemacs--collapse-dir-node (btn &optional recursive)
  "Close node given by BTN.
Remove all open dir and tag entries under BTN when RECURSIVE."
  (treemacs--button-close
   :button btn
   :new-state 'dir-node-closed
   :new-icon treemacs-icon-closed
   :post-close-action
   (-let [path (treemacs-button-get btn :path)]
     (treemacs--stop-watching path)
     (treemacs-on-collapse path recursive))))

(defun treemacs--root-face (project)
  "Get the face to be used for PROJECT."
  (cl-case (treemacs-project->path-status project)
    (local-unreadable 'treemacs-root-unreadable-face)
    (remote-readable 'treemacs-root-remote-face)
    (remote-disconnected 'treemacs-root-remote-disconnected-face)
    (remote-unreadable 'treemacs-root-remote-unreadable-face)
    (otherwise 'treemacs-root-face)))

(defun treemacs--add-root-element (project)
  "Insert a new root node for the given PROJECT node.

PROJECT: Project Struct"
  (insert treemacs-icon-root)
  (treemacs--set-project-position project (point-marker))
  (insert
   (propertize (treemacs-project->name project)
               'button '(t)
               'category 'default-button
               'face (treemacs--root-face project)
               :project project
               :symlink (when (treemacs-project->is-readable? project)
                          (file-symlink-p (treemacs-project->path project)))
               :state 'root-node-closed
               :path (treemacs-project->path project)
               :depth 0)))

(defun treemacs--render-projects (projects)
  "Actually render the given PROJECTS in the current buffer."
  (treemacs-with-writable-buffer
   (unless treemacs--projects-end
     (setq treemacs--projects-end (make-marker)))
   (let* ((current-workspace (treemacs-current-workspace))
          (has-previous (treemacs--apply-root-top-extensions current-workspace)))

     (--each projects
       (when has-previous (treemacs--insert-root-separator))
       (setq has-previous t)
       (treemacs--add-root-element it))

     ;; Set the end marker after inserting the extensions. Otherwise, the
     ;; extensions would move the marker.
     (let ((projects-end-point (point)))
       (treemacs--apply-root-bottom-extensions current-workspace has-previous)
       ;; If the marker lies at the start of the buffer, expanding extensions would
       ;; move the marker. Make sure that the marker does not move when doing so.
       (set-marker-insertion-type treemacs--projects-end has-previous)
       (set-marker treemacs--projects-end projects-end-point)))))

(define-inline treemacs-do-update-node (path)
  "Update the node identified by its PATH.
Throws an error when the node cannot be found. Does nothing if the node is
not expanded.
Same as `treemacs-update-node', but does not take care to either save
position or assure hl-line highlighting, so it should be used when making
multiple updates.

PATH: Node Path"
  (inline-letevals (path)
    (inline-quote
     (treemacs-unless-let (btn (treemacs-goto-node ,path))
         (error "Node at path %s cannot be found" ,path)
       (when (treemacs-is-node-expanded? btn)
         (-let [close-func (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config)]
           (funcall close-func)
           ;; close node again if no new lines were rendered
           (when (= 1 (funcall (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config)))
             (funcall close-func))))))))

(defun treemacs-update-node (path)
  "Update the node identified by its PATH.
Same as `treemacs-do-update-node', but wraps the call in
`treemacs-save-position'.

PATH: Node Path"
  (treemacs-save-position
   (treemacs-do-update-node path)))

(defun treemacs-delete-single-node (path &optional project)
  "Delete single node at given PATH and PROJECT.
Does nothing when the given node is not visible. Must be run in a treemacs
buffer.

This will also take care of all the necessary house-keeping like making sure
child nodes are deleted as well and everything is removed from the dom.

If multiple nodes are to be deleted it is more efficient to make multiple calls
to `treemacs-do-delete-single-node' wrapped in `treemacs-save-position' instead.

PATH: Node Path
Project: Project Struct"
  (treemacs-save-position
    (treemacs-do-delete-single-node path project)))

(define-inline treemacs-do-delete-single-node (path &optional project)
  "Actual implementation of single node deletion.
Will delete node at given PATH and PROJECT. See also
`treemacs-delete-single-node'.

PATH: Node Path
Project: Project Struct"
  (inline-letevals (path project)
    (inline-quote
     (when (treemacs-is-path-visible? ,path)
       (-let [pos nil]
         (--when-let (treemacs-find-in-dom ,path)
           (setf pos (treemacs-dom-node->position it))
           (when (treemacs-is-node-expanded? pos)
             (goto-char pos)
             (treemacs-TAB-action :purge))
           (treemacs-dom-node->remove-from-dom! it))
         (unless pos (treemacs-goto-node ,path ,project))
         (treemacs-with-writable-buffer
          (treemacs--delete-line)))
       (hl-line-highlight)))))

(defun treemacs--maybe-recenter (when &optional new-lines)
  "Potentially recenter based on value of WHEN.

WHEN can take the following values:

 * always: Recenter indiscriminately,
 * on-distance: Recentering depends on the distance between `point' and the
   window top/bottom being smaller than `treemacs-recenter-distance'.
 * on-visibility: Special case for projects: recentering depends on whether the
   newly rendered number of NEW-LINES fits the view."
  (declare (indent 1))
  (when (treemacs-is-treemacs-window? (selected-window))
    (let* ((current-line (float (treemacs--current-screen-line)))
           (all-lines (float (treemacs--lines-in-window))))
      (pcase when
        ('always (recenter))
        ('on-visibility
         (-let [lines-left (- all-lines current-line)]
           (when (> new-lines lines-left)
             ;; if possible recenter only as much as is needed to bring all new lines
             ;; into view
             (recenter (max 0 (round (- current-line (- new-lines lines-left))))))))
        ((guard (memq when '(t on-distance))) ;; TODO(2019/02/20): t for backward compatibility, remove eventually
         (let* ((distance-from-top (/ current-line all-lines))
                (distance-from-bottom (- 1.0 distance-from-top)))
           (when (or (> treemacs-recenter-distance distance-from-top)
                     (> treemacs-recenter-distance distance-from-bottom))
             (recenter))))))))

(provide 'treemacs-rendering)

;;; treemacs-rendering.el ends here
