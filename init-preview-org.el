;; config for publish site from org files
(require 'ox-publish)

(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("org-notes"
         :base-directory "~/temp/org-export/" ;; Change this to your local dir
         :base-extension "org"
         :publishing-directory "~/temp/www"
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
         :base-directory "~/temp/org-export/" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
         :publishing-directory "~/temp/www/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        )
      )

(defun my-preview-org-file ()
  (interactive)

  (when (not (file-directory-p "~/temp/org-export/css"))  (make-directory "~/temp/org-export/css" t))
  (when (not (file-directory-p "~/temp/www"))  (make-directory "~/temp/www" t))
  (when (not (file-exists-p "~/temp/org-export/css/stylesheet.css"))
    (copy-file "~/.emacs.d/css/stylesheet.css" "~/temp/org-export/css/stylesheet.css"))
  
  (when (or (not (eq major-mode 'org-mode))
            (not (equal (file-name-extension (buffer-name)) "org"))) 
    (error "This command only works in org-mode and on org file"))  
  (write-file (format "~/temp/org-export/%s" (buffer-name)))
  (org-publish-all)

  (when *is-mac-machine*
    (shell-command (format "open ~/temp/www/%s.%s" (file-name-sans-extension (buffer-name)) "html"))))

(define-key org-mode-map [f9] 'my-preview-org-file)

(provide 'init-preview-org)
