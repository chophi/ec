(require 'init-private-custom)
(require 'ox-confluence)

(defconst org-confluence-export-use-html-engine nil)

(defun org-confluence-export-buffer-name ()
  (if org-confluence-export-use-html-engine
      "*Org HTML Export*"
    "*org CONFLUENCE Export*"))

(defun org-export-buffer-to-wiki-and-view ()
  (interactive)
  (org-export-buffer-to-wiki (current-buffer) t))

(defun org-export-buffer-to-wiki(buffer-name &optional switch)
  (with-current-buffer (get-buffer buffer-name)
    (save-current-buffer
      (if org-confluence-export-use-html-engine
          (let ((org-html-format-table-no-css t))
            (org-html-export-as-html nil nil nil t))
          (org-confluence-export-as-confluence nil nil nil nil))
      ))
  (when switch
    (switch-to-buffer-other-window (org-confluence-export-buffer-name)))
  )

(defun org-publish-buffer-to-wiki(buffer-name wiki-page-id &optional)
  (org-export-buffer-to-wiki buffer-name nil)
  (let ((str
         (with-current-buffer (get-buffer (org-confluence-export-buffer-name))
           (buffer-string))))
    (message
     (shell-command-to-string
      (format "python %s -u %s -p %s -P %s -c %s -C %s -a store-wiki-content -m %s"
              *custom-write-wiki-script-path*
              *custom-confluence-username*
              (custom-input-confluence-password)
              wiki-page-id
              (shell-quote-argument str)
              *custom-confluence-root-url*
              (if org-confluence-export-use-html-engine "true" "false"))
      ))))

(defun org-get-wiki-page-id()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#\\+\\([wW][iI][kK][iI]_[iI][dD]\\):\s*\\([0-9]+\\)\s*$" nil t)
    (match-string-no-properties 2)))

(defun get-wiki-desc (wiki-page-id)
  (shell-command-to-string
   (format "python %s -u %s -p %s -P %s -c %s -C %s -a print-wiki-title"
           *custom-write-wiki-script-path*
           *custom-confluence-username*
           (custom-input-confluence-password)
           wiki-page-id
           "none"
           *custom-confluence-root-url*)))

(defun get-wiki-content (wiki-page-id)
  (shell-command-to-string
   (format "python %s -u %s -p %s -P %s -c %s -C %s -a print-wiki-content"
           *custom-write-wiki-script-path*
           *custom-confluence-username*
           (custom-input-confluence-password)
           wiki-page-id
           "none"
           *custom-confluence-root-url*)))

(defun* org-update-related-wiki-page ()
  (interactive)
  (let ((wiki-page-id (org-get-wiki-page-id)))
    (when (not wiki-page-id)
      (return-from 'org-update-related-wiki-page "wiki page id error"))
    (when (y-or-n-p (format "Update the buffer to following wiki:\n%s"
                            (get-wiki-desc wiki-page-id)))
      (org-publish-buffer-to-wiki (current-buffer) wiki-page-id))))

(defun* org-read-related-wiki-page ()
  (interactive)
  (let ((wiki-page-id (org-get-wiki-page-id)))
    (when (not wiki-page-id)
      (return-from 'org-update-related-wiki-page "wiki page id error"))
    (let ((wiki-buffer (get-buffer-create
                        (format "*confluence-wiki[%s]*" wiki-page-id))))
      (with-current-buffer wiki-buffer
        (insert (get-wiki-content wiki-page-id)))
      (switch-to-buffer-other-window wiki-buffer)
      )))

(when *amazon-machine?*
  (add-to-list 'org/ruhoh-keys '("w" . org-update-related-wiki-page))
  (add-to-list 'org/ruhoh-keys '("r" . org-read-related-wiki-page))
  (add-to-list 'org/ruhoh-keys '("v" . org-export-buffer-to-wiki-and-view))
  )

(provide 'init-confluence)
