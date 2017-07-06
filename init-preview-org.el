;; config for publish site from org files
(require 'ox-publish)

(when (file-exists-p "init-private-custom.el") (require 'init-private-custom))
(when (not (boundp '*org-blog-root*)) (setq *org-blog-root* "~/blog/"))
(defun org-blog-path (relpath)
  (concat *org-blog-root* relpath))

(setq org-publish-project-alist
      `(
        ;; These are the main web files
        ("org-notes"
         :base-directory ,(org-blog-path "org-export/") ;; Change this to your local dir
         :base-extension "org"
         :publishing-directory ,(org-blog-path "www")
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble nil
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         :section-numbers nil
         :table-of-contents t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\"/>"
         :style-include-default nil
         )

        ;; These are static files (images, pdf, etc)
        ("org-static"
         :base-directory ,(org-blog-path "org-export/") ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
         :publishing-directory ,(org-blog-path "www/")
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        )
      )

(defun my-preview-org-file (with-file)
  (interactive "P")
  (when (not (file-directory-p (org-blog-path "org-export/css")))
    (make-directory (org-blog-path "org-export/css") t))
  (when (not (file-directory-p (org-blog-path "www")))
    (make-directory (org-blog-path "www")))
  (when (not (file-exists-p (org-blog-path "org-export/css/stylesheet.css")))
    (copy-file "~/.emacs.d/css/stylesheet.css"
               (org-blog-path "org-export/css/stylesheet.css")))

  (when with-file
    (when (or (not (eq major-mode 'org-mode))
              (not (equal (file-name-extension (buffer-name)) "org"))) 
      (error "This command only works in org-mode and on org file"))
    (write-file (format (org-blog-path "org-export/%s") (buffer-name))))

  (org-publish-all)

  (when *mac?*
    (shell-command
     (format "open %s/www/%s.%s" *org-blog-root*
             (if with-file
               (file-name-sans-extension (buffer-name))
               "sitemap") "html"))))

(define-key org-mode-map [f9] 'my-preview-org-file)

(provide 'init-preview-org)
