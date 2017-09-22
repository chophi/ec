;; config for publish site from org files
(require 'ox-publish)

(when (file-exists-p "init-private-custom.el") (require 'init-private-custom))
(when (not (boundp '*org-preview-root*)) (setq *org-preview-root* "~/org-preview/"))
(when (not (boundp '*org-blog-root*)) (setq *org-blog-root* "~/blog/"))

(defun make-org-project-list (project-root)
  `(
    ;; These are the main web files
    ("org-notes"
     :base-directory ,(concat project-root "publish/") ;; Change this to your local dir
     :base-extension "org"
     :publishing-directory ,(concat project-root "www")
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4             ; Just the default for this project.
     :auto-preamble nil
     :auto-sitemap t
     :sitemap-filename "sitemap.org"
     :sitemap-title "Sitemap"
     :section-numbers nil
     :table-of-contents t
     :html-head ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\"/>\n"
                         "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/stylesheet.css\"/>\n"
                         "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../css/stylesheet.css\"/>") 
     :style-include-default nil
     )

    ;; These are static files (images, pdf, etc)
    ("org-static"
     :base-directory ,(concat project-root "material/") ;; Change this to your local dir
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
     :publishing-directory ,(concat project-root "www/")
     :recursive t
     :publishing-function org-publish-attachment
     )

    ("org" :components ("org-notes" "org-static"))
    ))

(defun publish-org-files-in-dir (dir)
  (let ((make-dir-if-not-exist
         (lambda (relative-path)
           (when (not (file-directory-p (concat dir relative-path)))
             (make-directory (concat dir relative-path) t)
             ))))
    (funcall make-dir-if-not-exist "publish")
    (funcall make-dir-if-not-exist "material/css")
    (funcall make-dir-if-not-exist "www")
    (when (not (file-exists-p (concat dir "material/css/stylesheet.css")))
      (copy-file "~/.emacs.d/css/stylesheet.css"
                 (concat dir "material/css/stylesheet.css")))
    (let ((org-publish-project-alist (make-org-project-list dir)))
      (org-publish-all)
      )
    ))

(defun my-preview-org-file (with-file)
  (interactive "P")
  (when with-file
    (when (or (not (eq major-mode 'org-mode))
              (not (equal (file-name-extension (buffer-name)) "org"))) 
      (error "This command only works in org-mode and on org file"))
    (write-file (format (org-blog-preview "publish/%s") (buffer-name))))

  (publish-org-files-in-dir *org-preview-root*)

  (when *mac?*
    (shell-command
     (format "open %s/www/%s.%s" *org-preview-root*
             (if with-file
               (file-name-sans-extension (buffer-name))
               "sitemap") "html"))))

(defun my-open-blog ()
  (interactive)
  (publish-org-files-in-dir *org-blog-root*))

(define-key org-mode-map [f9] 'my-preview-org-file)

(provide 'init-preview-org)
