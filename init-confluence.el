(require-package 'confluence)
(require 'confluence)

(setq confluence-xml-convert-to-wiki-on-load t)
(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook (lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))


(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))


     (add-hook 'ediff-cleanup-hook
               (lambda ()
                 (dolist (tmp-buf (list ediff-buffer-A
                                        ediff-buffer-B
                                        ediff-buffer-C))
                   (if (buffer-live-p tmp-buf)
                       (with-current-buffer tmp-buf
                         (longlines-restore))))))))

(add-hook 'confluence-edit-mode-hook
  (local-set-key "\C-xw" confluence-prefix-map)
  (local-set-key "\M-j" 'confluence-newline-and-indent)
  (local-set-key "\M-;" 'confluence-list-indent-dwim))

(require 'init-private-custom)
(require 'ox-confluence)
(defun org-export-buffer-to-wiki(buffer-name wiki-page-id)
  (with-current-buffer (get-buffer buffer-name)
    (save-current-buffer
      (org-confluence-export-as-confluence nil nil nil nil)))
  (let ((str
         (with-current-buffer (get-buffer "*org CONFLUENCE Export*")
           (buffer-string))))
    (message
     (shell-command-to-string
      (format "python %s -u %s -p %s -P %s -c %s -C %s -a store-wiki-content"
              *custom-write-wiki-script-path*
              *custom-confluence-username*
              (custom-input-confluence-password)
              wiki-page-id
              (shell-quote-argument str)
              *custom-confluence-root-url*)))))

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
      (org-export-buffer-to-wiki (current-buffer) wiki-page-id))))

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
  )

(provide 'init-confluence)
