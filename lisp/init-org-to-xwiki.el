(require 'ox-md)
(require 'ox-pandoc)

(defun org-get-xwiki-path()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#\\+\\([xX][wW][iI][kK][iI]_[pP][aA][tT][hH]\\):\s*\\([-_0-9a-zA-Z/]+\\)\s*$" nil t)
    (let ((path (match-string-no-properties 2)))
      (if (and path (cu-seq-ends-with path "/"))
          (substring path 0 -1)
        path))))

(defun org-get-export-format()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#\\+\\([Xx][Ww][Ii][Kk][Ii]_[Ff][Oo][Rr][Mm][Aa][Tt]\\):\s*\\([a-zA-Z]+\\)\s*$" nil t)
    (let ((format (match-string-no-properties 2)))
      format)))

(defun org-open-xwiki ()
  (interactive)
  (unless (equal major-mode 'org-mode)
    (error "Only use this in org-mode"))
  (unless (equal system-type 'darwin)
    (error "Only use this in darwin"))
  (let* ((filename (buffer-file-name))
         (name (file-name-nondirectory filename))
         (wiki-path-from-property (org-get-xwiki-path))
         (wiki-url (if wiki-path-from-property
                       (format "https://w.amazon.com/bin/view/%s" wiki-path-from-property)
                     (format "https://w.amazon.com/bin/view/ApiTesting/%s/%s"
                             (cu-strip-string (shell-command-to-string "whoami") t t)
                             (file-name-sans-extension name)))))
    (shell-command (format "open %s" wiki-url))))

(defun org-to-xwiki ()
  (interactive)
  (unless (equal major-mode 'org-mode)
    (error "Only use this in org-mode"))
  (let* ((filename (buffer-file-name))
         (name (file-name-nondirectory filename))
         (wiki-path-from-property (org-get-xwiki-path))
         (wiki-name (or wiki-path-from-property (file-name-sans-extension name)))
         (export-format (or (org-get-export-format) "md"))
         (tmpfile (cu-join-path "/tmp" (concat (file-name-nondirectory (file-name-sans-extension filename)) (concat "." export-format))))
         (command (format "amzn-wiki-wrapper.sh %s %s %s" (if wiki-path-from-property "--no-api-testing" "")  tmpfile wiki-name)))
    (message "Export format is %s" export-format)
    (cond ((equal export-format "md")
           (unless (equal (shell-command
                           (format "pandoc --mathjax -f org -t markdown < %s > %s"
                                   filename
                                   tmpfile)) 0)
             (error "Fail to convert to markdown!")))
          ((equal export-format "xwiki")
           (unless (require 'ox-xwiki nil t)
             (error "Need ox-xwiki to export org file to xwiki!"))
           (org-export-to-file 'xw tmpfile nil nil nil)))
    (cu-send-command-to-buffer-local-terminal command)))
(provide 'init-org-to-xwiki)
