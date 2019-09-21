;; config for publish site from org files
(require 'ox-publish)

(defvar org-preview-root "~/blog/preview/"
  "The root directory to publish org files as html for previewing")
(defvar org-blog-root "~/blog/public"
  "The root directory for bloging locally")

(defun make-org-project-list (root)
  `(;; These are the writings with suffix .org
    ("org-writings"
     :base-directory ,(cu-join-path root "writings")
     :base-extension "org"
     :publishing-directory ,(cu-join-path root "www")
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4 
     :auto-preamble nil
     :auto-sitemap t
     :sitemap-filename "index.org"
     :sitemap-title "Site Map"
     :section-numbers nil
     :table-of-contents t
     :html-head
     ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\"/>\n"
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/stylesheet.css\"/>\n"
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../css/stylesheet.css\"/>") 
     :style-include-default nil)

    ;; These are static resource files (images, pdf, etc)
    ("org-resources"
     :base-directory ,(cu-join-path root "resources")
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
     :publishing-directory ,(cu-join-path root "www")
     :recursive t
     :publishing-function org-publish-attachment)

    ("org" :components ("org-writings" "org-resources"))))

(defvar org-publish-default-css-file "~/.emacs.d/css/stylesheet.css"
  "The default style sheet for org publishing")

(defun publish-org-files-in-dir (dir)
  "Publish the org files from DIR to DIR/www"
  (let ((make-dir-if-not-exist
         (lambda (relative-path)
           (when (not (file-directory-p (cu-join-path dir relative-path)))
             (make-directory (cu-join-path dir relative-path) t))))
        (css-file (cu-join-path dir "resources/css/stylesheet.css"))
        (sitemap-file (cu-join-path dir "writings/index.org")))
    ;; make directories if not exist
    (mapc make-dir-if-not-exist '("writings" "resources/css" "www"))
    ;; copy the default css files to the blog resource folder
    (when (not (file-exists-p css-file))
      (copy-file org-publish-default-css-file css-file))
    ;; create an empty sitemap file if there's no one existed.
    (when (not (file-exists-p sitemap-file))
      (shell-command (format "touch %s" sitemap-file)))
    ;; publish all the blog files
    (let ((org-publish-project-alist (make-org-project-list dir)))
      (org-publish-all))))

(defun org-preview-file-as-html (file)
  (interactive "P")
  ;; Copy the file to the preview folder
  (when file
    (when (or (not (eq major-mode 'org-mode))
              (not (equal (file-name-extension (buffer-name)) "org"))) 
      (error "This command only works in org-mode and on org file"))
    (let ((writings-dir (cu-join-path org-preview-root "writings")))
      (when (not (file-directory-p writings-dir))
        (make-directory writings-dir t))
      (write-file (cu-join-path writings-dir (buffer-name)))))
  ;; Publish all the org files in the preview root.
  (publish-org-files-in-dir org-preview-root)
  ;; Open the html file in browser
  (when (eq os 'macos)
    (shell-command
     (format "open %s/www/%s.%s" org-preview-root
             (if file
                 (file-name-sans-extension (buffer-name))
               "index")
             "html"))))

(defun org-update-blog ()
  (interactive)
  (publish-org-files-in-dir org-blog-root)
  (when (eq os 'macos)
    (shell-command (format "open %s/www/index.html" org-blog-root))))

(with-eval-after-load "org-mode"
  (define-key org-mode-map [f9] 'org-preview-file-as-html)
  (define-key org-mode-map [f10] 'org-update-blog))

(provide 'init-org-publish-settings)
 
