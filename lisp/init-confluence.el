(require 'init-private-custom)
(require 'ox-confluence)

(defconst org-confluence-export-use-html-engine nil
  (concat "Whether to use html engine to export the confluence org files\n"
          "You can also force using html-engine with #+USE_HTML: true")
  )

(defun org-export-use-html-engine()
  (interactive)
  (let ((use-html (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "#\\+\\([uU][sS][eE]_[hH][tT][mM][lL]\\):\s*\\([a-z]+\\)\s*$" nil t)
                    (match-string-no-properties 2))))
    (if (equal use-html "true")
        t
      (if (equal use-html "false") nil org-confluence-export-use-html-engine))))

(defun org-export-buffer-to-wiki-and-view ()
  (interactive)
  (org-export-buffer-to-wiki (current-buffer) t))

(defun org-export-buffer-to-wiki(buffer-name &optional switch)
  (with-current-buffer (get-buffer buffer-name)
    (let ((org-use-html-engine (org-export-use-html-engine)))
      (save-current-buffer
        (if org-use-html-engine
            (let ((org-html-format-table-no-css t))
              (org-html-export-as-html nil nil nil t))
            (org-confluence-export-as-confluence nil nil nil nil)))
      (when switch
        (switch-to-buffer-other-window
         (if org-use-html-engine
             "*Org HTML Export*"
           "*org CONFLUENCE Export*"))))))

(defun org-publish-buffer-to-wiki(buffer-name wiki-page-id)
  (org-export-buffer-to-wiki buffer-name nil)
  (with-current-buffer (get-buffer buffer-name)
    (let* ((org-use-html-engine (org-export-use-html-engine))
           (str (with-current-buffer
                    (get-buffer (if org-use-html-engine
                                    "*Org HTML Export*"
                                  "*org CONFLUENCE Export*"))
                  (buffer-string))))
      (print "org-use-html-engine is:")
      (print org-use-html-engine)
      (shell-command
       (format "python %s -u %s -p %s -P %s -c %s -C %s -a store-wiki-content -m %s"
               *custom-write-wiki-script-path*
               *custom-confluence-username*
               (custom-input-confluence-password)
               wiki-page-id
               (shell-quote-argument str)
               *custom-confluence-root-url*
               (if org-use-html-engine "false" "true"))
       "*Org Write Wiki[OUTPUT]*" "*Org Write Wiki[ERROR]*"))))

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
  (shell-command
   (format "python %s -u %s -p %s -P %s -c %s -C %s -a print-wiki-content"
           *custom-write-wiki-script-path*
           *custom-confluence-username*
           (custom-input-confluence-password)
           wiki-page-id
           "none"
           *custom-confluence-root-url*)
   "*Org Write Wiki[OUTPUT]*" "*Org Write Wiki[ERROR]*"))

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
      (with-current-buffer wiki-buffer
        (insert (get-wiki-content wiki-page-id)))
      (switch-to-buffer-other-window wiki-buffer)
      )))

(when (company-computer-p)
  (defconst org/ruhoh-keys
    '(("d" . (lambda () (interactive) (org-update-related-wiki-page t)))
      ("w" . org-update-related-wiki-page)
      ("r" . org-read-related-wiki-page)
      ("v" . org-export-buffer-to-wiki-and-view)))
  (cu-set-key-bindings global-map "\C-c\C-p" org/ruhoh-keys)
  (cu-set-key-bindings org-mode-map "\C-c\C-p" org/ruhoh-keys))

(provide 'init-confluence)
