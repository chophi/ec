;;; ox-confluence --- Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2012, 2014 SÃ©bastien Delafond

;; Author: SÃ©bastien Delafond <sdelafond@gmail.com>
;; Keywords: outlines, confluence, wiki

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-confluence.el lets you convert Org files to confluence files
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-confluence)
;;
;; Export Org files to confluence:
;; M-x org-confluence-export-as-confluence RET
;;
;;; Code:

(require 'ox)
(require 'ox-ascii)
(require 'org-table)

;; Define the backend itself
(org-export-define-derived-backend 'confluence 'ascii
  :translate-alist '((bold . org-confluence-bold)
                     (code . org-confluence-code)
                     (example-block . org-confluence-example-block)
                     (fixed-width . org-confluence-fixed-width)
                     (footnote-definition . org-confluence-footnote-definition)
                     (footnote-reference . org-confluence-footnote-reference)
                     (headline . org-confluence-headline)
                     (italic . org-confluence-italic)
                     (item . org-confluence-item)
                     (link . org-confluence-link)
                     (paragraph . org-confluence-paragraph)
                     (property-drawer . org-confluence-property-drawer)
                     (quote-block . org-confluence-quote-block)
                     (section . org-confluence-section)
                     (src-block . org-confluence-src-block)
                     (strike-through . org-confluence-strike-through)
                     (table . org-confluence-table)
                     (table-cell . org-confluence-table-cell)
                     (table-row . org-confluence-table-row)
                     (template . org-confluence-template)
                     (inner-template . org-confluence-inner-template)
                     (timestamp . org-confluence-timestamp)
                     (underline . org-confluence-underline)
                     (verbatim . org-confluence-verbatim)))

(defconst org-confluence-lang-alist
  '(("sh" . "bash")
    ("makefile" . "text"))
  "Map from org-babel language name to confluence wiki language name")

;; All the functions we use
(defun org-confluence-bold (bold contents info)
  (format "*%s*" contents))

(defun org-confluence-footnote-reference (footnote-reference _contents info)
  (let ((number (org-export-get-footnote-number footnote-reference info)))
    (format "[^\\[%s\\]^|#_fn_-%s]" number number)))

(defun org-confluence-footnote-definition (footnote-reference _contents info)
  ""
  ;; (let ((number (org-export-get-footnote-number footnote-reference info)))
  ;;   (format "{anchor:foot-note-%s}" number))
  )

(defun org-confluence-inner-template (contents info)
  "Return complete document string after ASCII conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-element-normalize-string
   (let ((global-margin (plist-get info :ascii-global-margin)))
     (org-ascii--indent-string
      (concat
       ;; 1. Document's body.
       contents
       ;; 2. Footnote definitions.
       (let ((definitions (org-export-collect-footnote-definitions info))
	     ;; Insert full links right inside the footnote definition
	     ;; as they have no chance to be inserted later.
	     (info (org-combine-plists info '(:ascii-links-to-notes nil))))
	 (when definitions
	   (concat
	    "\n"
	    ;; (let ((title (org-ascii--translate "Footnotes" info)))
	    ;;   (concat
	    ;;    title "\n"
	    ;;    "----"))
	    ;; "\n\n"
	    (let ((text-width (- (plist-get info :ascii-text-width)
				 global-margin)))
	      (mapconcat
	       (lambda (ref)
		 (let ((id (format "{anchor:_fn_-%s} %s. " (car ref) (car ref))))
		   ;; Distinguish between inline definitions and
		   ;; full-fledged definitions.
		   (org-trim
		    (let ((def (nth 2 ref)))
		      (if (org-element-map def org-element-all-elements
			    #'identity info 'first-match)
			  ;; Full-fledged definition: footnote ID is
			  ;; inserted inside the first parsed
			  ;; paragraph (FIRST), if any, to be sure
			  ;; filling will take it into consideration.
			  (let ((first (car (org-element-contents def))))
			    (if (not (eq (org-element-type first) 'paragraph))
				(concat id "\n" (org-export-data def info))
			      (push id (nthcdr 2 first))
			      (concat id (org-export-data def info))))
			;; Fill paragraph once footnote ID is inserted
			;; in order to have a correct length for first
			;; line.
			(org-ascii--fill-string
			 (concat id (org-export-data def info))
			 text-width info))))))
	       definitions "\n\n"))))))
      global-margin))))

(defun org-confluence-example-block (example-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let ((content (org-export-format-code-default example-block info)))
    (org-confluence--block "none" "Confluence" content)))

(defun org-confluence-italic (italic contents info)
  (format "_%s_" contents))

(defun org-confluence-item (item contents info)
  (let ((list-type (org-element-property :type (org-export-get-parent item))))
    (concat
     (make-string (1+ (org-confluence--li-depth item))
                  (if (eq list-type 'ordered) ?\# ?\-))
     " "
     (pcase (org-element-property :checkbox item)
       (`on "*{{(X)}}* ")
       (`off "*{{( )}}* ")
       (`trans "*{{(\\-)}}* "))
     (when (eq list-type 'descriptive)
       (concat "*"
               (org-export-data (org-element-property :tag item) info)
               "* - "))
     (org-trim contents))))

(defun org-confluence-fixed-width (fixed-width contents info)
  (org-confluence--block
   "none"
   "Confluence"
   (org-trim (org-element-property :value fixed-width))))

(defun org-confluence-verbatim (verbatim contents info)
  (format "\{\{%s\}\}" (org-element-property :value verbatim)))

(defun org-confluence-code (code contents info)
  (format "\{\{%s\}\}" (org-element-property :value code)))

(defun org-confluence-headline (headline contents info)
  (let* ((low-level-rank (org-export-low-level-p headline info))
         (text (org-export-data (org-element-property :title headline)
                                info))
         (todo (org-export-data (org-element-property :todo-keyword headline)
                                info))
         (level (org-export-get-relative-level headline info))
         (todo-text (if (or (not (plist-get info :with-todo-keywords))
                            (string= todo ""))
                        ""
                      (format "*{{%s}}* " todo))))
    (format "h%s. %s%s\n%s%s" level todo-text text
            (if (equal level 1) "----\n" "")
            (if (org-string-nw-p contents) contents ""))))

(defvar private-local-server-image-map-alist nil)

(defun* is-graphviz-link (raw-link)
  (when private-local-server-image-map-alist
    (dolist (m private-local-server-image-map-alist)
      (when (string-match (car m) raw-link)
        (return-from is-graphviz-link (replace-regexp-in-string (car m) (cdr m) raw-link))))))

(defun org-confluence-link (link desc info)
  (let* ((raw-link (org-element-property :raw-link link))
         (graphviz-link (is-graphviz-link raw-link))
         (open-brace (if graphviz-link "!" "["))
         (close-brace (if graphviz-link "!" "]")))
    (when graphviz-link (setq raw-link graphviz-link))
    (concat open-brace
            (when (and (not graphviz-link) (org-string-nw-p desc))
              (format "%s|" desc))
            (cond
             ((string-match "^confluence:" raw-link)
              (replace-regexp-in-string "^confluence:" "" raw-link))
             (t
              raw-link))
            (when (and graphviz-link (org-string-nw-p desc))
              (format "|title=%s" desc))
            close-brace)))

(defun org-confluence-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element for Confluence.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  contents)

(defun org-confluence-property-drawer (property-drawer contents info)
  (and (org-string-nw-p contents)
       (format "\{\{%s\}\}" contents)))

(defun org-confluence-quote-block (quote-block contents info)
  (format "{quote}\n%s{quote}" contents))

(defun org-confluence-section (section contents info)
  contents)

(defun org-confluence-attr (src-block key-str)
  (let* ((attrs (car (org-element-property :attr_confluence src-block)))
         (attr-list (when (stringp attrs)
                      (mapcar (lambda (attr)
                                (mapcar 'string-trim (split-string attr ":")))
                              (split-string attrs ",")))))
    (cadr (assoc key-str attr-list))))

(defun org-attribute-to-bool (src-block key-str)
  (let ((result (org-confluence-attr src-block key-str)))
    (and (stringp result)
         (not (or (equal (downcase result) "nil")
                  (equal (downcase result) "false")
                  (equal (downcase result) "no"))))))

(defun org-confluence-src-block (src-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let* ((lang (org-element-property :language src-block))
         (linenumbers (org-attribute-to-bool src-block "linenumbers"))
         (no-collapse (org-attribute-to-bool src-block "no-collapse"))
         (language (or (cdr (assoc lang org-confluence-lang-alist)) lang))
         (content (org-export-format-code-default src-block info)))
    (org-confluence--block language "Confluence"
                           content no-collapse linenumbers)))

(defun org-confluence-strike-through (strike-through contents info)
  (format "-%s-" contents))

(require 'ox-html)
(require 'table)
(defun org-confluence-table (table contents info)
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-html-table--table.el-table table info)
  contents))

(defun org-confluence-table-row  (table-row contents info)
  (when (not (eq (org-element-property :type table-row) 'table.el))
    (concat
     (if (org-string-nw-p contents) (format "|%s" contents)
       "")
     (when (org-export-table-row-ends-header-p table-row info)
       "|"))))

(defun org-confluence-table-cell  (table-cell contents info)
  (when (not (eq (org-element-property :type table-cell) 'table.el)) 
    (let ((table-row (org-export-get-parent table-cell)))
      (concat (and (org-export-table-row-starts-header-p table-row info) "|")
              (if (= (length contents) 0) " " contents)
              "|"))))

(defun org-confluence-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "\{toc\}\n\n") contents)))

(defun org-confluence-timestamp (timestamp _contents _info)
  "Transcode a TIMESTAMP object from Org to Confluence.
CONTENTS and INFO are ignored."
  (let ((translated (org-trim (org-timestamp-translate timestamp))))
    (if (string-prefix-p "[" translated)
        (concat "(" (substring translated 1 -1) ")")
      translated)))

(defun org-confluence-underline (underline contents info)
  (format "+%s+" contents))

(defun org-confluence-translate-language (lang)
  (let ((lan-alist '(("c" . "cpp")
                     ("cc" . "cpp")
                     ("c++" . "cpp")
                     ("python" . "py")
                     ("sh" . "bash")
                     ("m4" . "text")))
        (dc-lang (downcase lang)))
    (if (assoc dc-lang lan-alist)
        (cdr (assoc dc-lang lan-alist))
      dc-lang)))

(defun org-confluence--block (language theme contents &optional no-collapse linenumber)
  (let ((numlines (1- (cu-count-lines contents))))
    (concat "\{code:theme=" theme
            (when language (format "|language=%s" (org-confluence-translate-language language)))
            (when (and (>= numlines 15) (not no-collapse)) "|collapse=true")
            (when linenumbers "|linenumbers=true")
            "}\n"
            contents
            "\{code\}\n")))

(defun org-confluence--li-depth (item)
  "Return depth of a list item; -1 means not a list item"
  ;; FIXME check whether it's worth it to cache depth
  ;;       (it gets recalculated quite a few times while
  ;;       traversing a list)
  (let ((depth -1)
        (tag))
    (while (and item
                (setq tag (car item))
                (or (eq tag 'item) ; list items interleave with plain-list
                    (eq tag 'plain-list)))
      (when (eq tag 'item)
        (cl-incf depth))
      (setq item (org-export-get-parent item)))
    depth))

;; main interactive entrypoint
(defun org-confluence-export-as-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONFLUENCE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'confluence "*org CONFLUENCE Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'custom-ox-confluence)
