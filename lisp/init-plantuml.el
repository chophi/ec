(require-package 'plantuml-mode)

;; graphviz-output-file-name require this.
(require 'graphviz-dot-mode)

(require-package 'flycheck-plantuml)
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/jar-package/plantuml.jar")
      plantuml-jar-path org-plantuml-jar-path)

(defun org-mode-init ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(plantuml . t))))
(add-hook 'org-mode-hook 'org-mode-init)

(defvar plantuml-style-folder (expand-file-name "~/.emacs.d/plantuml-style"))

(defun get-style-name ()
  (interactive)
  (let* ((str (buffer-substring-no-properties (point-min) (point-max)))
         (style "default"))
    (when (string-match "^\\s-*/'\\s-*STYLE:\\s-*\\(\\S-+\\)\\s-*'/\\s-*$" str)
      (setq style (match-string 1 str)))
    style))

(defun* include-style-string ()
  (interactive)
  (let* ((style-name (get-style-name))
         (default-style-name (cu-join-path plantuml-style-folder "default.plu"))
         (possible-style-name
          (cu-join-path plantuml-style-folder (concat style-name ".plu"))))
    (when (or (equal style-name "none") (equal style-name "nil"))
      (return-from include-style-string ""))
    (if (file-exists-p possible-style-name)
        (format "-I%s" possible-style-name)
      (if (file-exists-p default-style-name)
          (format "-I%s" default-style-name)
        ""))))

(setq graphviz-dot-preview-extension "svg")
(setq plantuml-convert-to-latex nil)

(defun plantuml-execute ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (if (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
        (setq out-file (match-string 1 code))
      (setq out-file
            (let ((graphviz-dot-preview-extension
                   (if plantuml-convert-to-latex
                       "latex"
                     graphviz-dot-preview-extension)))
              (graphviz-output-file-name (buffer-file-name) plantuml-convert-to-latex))))
    (when plantuml-convert-to-latex
      (setq out-file (cu-join-path "/tmp" (file-name-nondirectory out-file)))
      (when (file-exists-p out-file) (delete-file out-file)))
    (setq cmd (concat
               "java -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (or (and out-file (concat "-t" (file-name-extension out-file)))
                   "-tpng")
               " -p "
               " " (include-style-string) " "
               plantuml-options " < "
               (buffer-file-name)
               " > " out-file))
    (message cmd)
    (shell-command cmd)
    (message "done")
    (if plantuml-convert-to-latex
        (funcall #'compile-tikz-to-svg out-file)
      (find-file-other-window out-file))))

(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8")

(with-eval-after-load "auto-complete" 
  (add-to-list 'ac-modes 'plantuml-mode))

(defun plantuml-style-list ()
  (delete "common"
          (mapcar 'file-name-sans-extension
                  (mapcar 'file-name-nondirectory
                          (cu-list-files-recursively plantuml-style-folder ".plu" 1)))))

(provide 'init-plantuml)
