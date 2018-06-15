(require-package 'plantuml-mode)

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
      (setq out-file (graphviz-output-file-name (buffer-file-name))))
    (setq cmd (concat
               "java -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (or (and out-file (concat "-t" (file-name-extension out-file)))
                   "-tpng") " -p "
               plantuml-options " < "
               (buffer-file-name)
               " > " out-file))
    (message cmd)
    (shell-command cmd)
    (message "done")
    (find-file-other-window out-file)))

(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8")
(provide 'init-plantuml)
