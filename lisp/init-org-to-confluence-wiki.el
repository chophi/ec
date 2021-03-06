(require 'custom-ox-confluence)

(defvar org-confluence-write-wiki-out-buffer "*Org Write Wiki[OUTPUT]*")
(defvar org-confluence-write-wiki-err-buffer "*Org Write Wiki[ERROR]*")
(defconst org-confluence-export-use-html-engine nil
  (concat "Whether to use html engine to export the confluence org files\n"
          "You can also force using html-engine with #+USE_HTML: true")
  )

(defun org-export-use-html-engine()
  (interactive)
  (let ((use-html
         (save-excursion
           (goto-char (point-min))
           (re-search-forward
            "#\\+\\([uU][sS][eE]_[hH][tT][mM][lL]\\):\s*\\([a-z]+\\)\s*$"
            nil t)
           (match-string-no-properties 2))))
    (if (equal use-html "true")
        t
      (if (equal use-html "false") nil org-confluence-export-use-html-engine))))

(defun org-export-buffer-to-wiki-and-view ()
  (interactive)
  (org-export-buffer-to-wiki (current-buffer) t))

(defun org-confluence-export-buffer-name ()
  (if org-use-html-engine
      "*Org HTML Export*"
    "*org CONFLUENCE Export*"))

(defun org-export-buffer-to-wiki(buffer-name &optional switch)
  (with-current-buffer (get-buffer buffer-name)
    (let ((org-use-html-engine (org-export-use-html-engine)))
      (save-current-buffer
        (let ((org-export-show-temporary-export-buffer nil))
          (if org-use-html-engine
              (let ((org-html-format-table-no-css t))
                (org-html-export-as-html nil nil nil t))
            (org-confluence-export-as-confluence nil nil nil nil))))
      (when switch
        (switch-to-buffer-other-window (org-confluence-export-buffer-name))))))

(defun run-write-wiki-script (action content &optional extra-arg get-output)
  (let*  ((command (format "python %s -u %s -p %s -P %s -c %s -C %s -a %s"
                          *custom-write-wiki-script-path*
                          *custom-confluence-username*
                          (custom-input-confluence-password)
                          wiki-page-id
                          (shell-quote-argument content)
                          *custom-confluence-root-url*
                          action)))
    (when extra-arg (setq command (concat command " " extra-arg)))
    (if get-output
        (shell-command-to-string command)
      (save-window-excursion
        (let ((command-success (eq 0 (shell-command
                                      command
                                      org-confluence-write-wiki-out-buffer
                                      org-confluence-write-wiki-err-buffer))))
          command-success)))))

(defun org-publish-buffer-to-wiki(buffer-name wiki-page-id)
  (org-export-buffer-to-wiki buffer-name nil)
  (with-current-buffer (get-buffer buffer-name)
    (let* ((org-use-html-engine (org-export-use-html-engine))
           (str (with-current-buffer
                    (get-buffer (org-confluence-export-buffer-name))
                  (buffer-string))))
      (message "Use %s engine" (if org-use-html-engine "HTML" "Confluence"))
      (run-write-wiki-script
       "store-wiki-content"
       str
       (format "-m %s" (if org-use-html-engine "false" "true"))))))

(defun org-get-wiki-page-id()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#\\+\\([wW][iI][kK][iI]_[iI][dD]\\):\s*\\([0-9]+\\)\s*$" nil t)
    (match-string-no-properties 2)))

(defun get-wiki-desc (wiki-page-id)
  (run-write-wiki-script
   "print-wiki-title"
   "none"
   nil ;; no-extra
   t ;; the purpose is to get output
   ))

(defun* org-update-related-wiki-page (&optional draft-wiki)
  (interactive "P")
  (let ((curbuf (current-buffer))
        (wiki-page-id
         (if draft-wiki
             "269528852"
             (org-get-wiki-page-id))))
    (when (not wiki-page-id)
      (return-from 'org-update-related-wiki-page "wiki page id error"))
    (when (y-or-n-p (format "Update the buffer to following wiki%s:\n%s"
                            (if draft-wiki "[DRAFT]" "")
                            (get-wiki-desc wiki-page-id)))
      (org-publish-buffer-to-wiki curbuf wiki-page-id))))

(defun* org-read-related-wiki-page ()
  (interactive)
  (let ((wiki-page-id (org-get-wiki-page-id)))
    (when (not wiki-page-id)
      (return-from 'org-update-related-wiki-page "wiki page id error"))
    (let ((wiki-buffer (get-buffer-create
                        (format "*confluence-wiki[%s]*" wiki-page-id))))
      (if (run-write-wiki-script "print-wiki-content" "none")
          (display-buffer org-confluence-write-wiki-out-buffer)
        (display-buffer org-confluence-write-wiki-err-buffer)))))

(defun org-update-to-draft-page ()
  (interactive)
  (org-update-related-wiki-page t))

(provide 'init-org-to-confluence-wiki)
