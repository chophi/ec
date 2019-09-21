(require 'org-export-header)

;;-----------------------------------------------------------------------------
;; cn-article class settings.
;;-----------------------------------------------------------------------------
(add-to-list 'org-latex-classes
             `("cn-article"
               ,(concat cn-article-header
                        "\n[NO-DEFAULT-PACKAGES]"
                        "\n[EXTRA]"
                        "\n[PACKAGES]")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;-----------------------------------------------------------------------------
;; slides class settings.
;;-----------------------------------------------------------------------------
(add-to-list 'org-latex-classes
             `("slides"
               ,(concat org-slides-header
                        "\n[NO-DEFAULT-PACKAGES]"
                        "\n[EXTRA]"
                        "\n[PACKAGES]")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))


;;-----------------------------------------------------------------------------
;; babel class settings.
;;-----------------------------------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 `((R . t)
   (emacs-lisp . t)
   (matlab . t)
   (C . t)
   (perl . t)
   (ditaa . t)
   (python . t)
   (haskell . t)
   (dot . t)
   (latex . t)
   (js . t)))

;; load ditaa 
;; (setq org-ditaa-jar-path "/usr/bin/ditaa")

;;-----------------------------------------------------------------------------
;; org latex export listings setting
;;-----------------------------------------------------------------------------
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "orgminted") t)


;; Options for \lset command（reference to listing Manual)
;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-latex-minted-options
      '(("linenos" "true")
	    ;; ("bgcolor" "mint-background-0")
	    ;; ("samepage" "false")
	    ("fontfamily" "tt")
	    ("fontsize" "\\footnotesize")
	    ("mathescape" "true")
	    ("frame" "single")))

(setq org-latex-minted-langs
      '((emacs-lisp "common-lisp")
        (cc "c++")
        (cperl "perl")
        (java "java")
        (shell-script "bash")
        (lisp "common-lisp")
        (js "javascript")
        (ruby "ruby")
        (php "php")
        (scala "scala")
        (clojure "clojure")
        (caml "ocaml")
        (asm "Assembler")))

;;-----------------------------------------------------------------------------
;; export command setting
;;-----------------------------------------------------------------------------
;; Originally taken from Bruno Tavernier:
;; http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd (exporter)
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  ;; (print exporter)
  (when (eq exporter 'latex)
    (let ((texcmd-latex "latexmk -pdf -f -shell-escape %f")
          (texcmd-xelatex "latexmk -pdf -f -xelatex -shell-escape %f")
          (bufstr (buffer-string))
          (texcmd nil))
      ;; pdflatex -> .pdf
      (cond ((or (string-match "LATEX_CMD: xelatex" bufstr)
                 (string-match "LATEX_CLASS: cn-article" bufstr))
             (setq texcmd texcmd-xelatex))
            (t (setq texcmd texcmd-latex)))
      (setq org-latex-pdf-process (list texcmd)))))

(add-hook 'org-export-before-processing-hook 'my-auto-tex-cmd)

(defconst xelatex-temp-ext-list
  '(".pyg" ".fls" ".fdb_latexmk" ".aux" ".log" ".out" ".toc" ".bbl" ".xdv" ".tex")
  "xelatex temp file extention list")

(defun my-trash-xelatex-temp-files ()
  (interactive)
  (let ((base-name
         (file-name-sans-extension
          (file-name-nondirectory
           (buffer-file-name)))))
    (dolist (ext xelatex-temp-ext-list)
      (when (file-exists-p (concat base-name ext))
        (delete-file (concat base-name ext) t)))))

;;-----------------------------------------------------------------------------
;; export html css setting
;;-----------------------------------------------------------------------------
(setq org-html-head-include-default-style nil)

(defun org-export-use-style-file ()
  (interactive)
  (insert
   (format
    "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />\n"
    (file-relative-name
     (concat
      "~/css/"
      (ido-completing-read "Select CSS file: " (cddr (directory-files "~/css"))))))))

;; prevent show the char of seperate line of fci-mode in the html
(add-hook 'htmlize-before-hook
          (lambda () (when (fboundp 'fci-mode) (fci-mode -1))))
(setq htmlize-html-charset "utf-8")
(setq org-html-postamble nil)
(setq org-hide-emphasis-markers t)

(provide 'init-org-export-settings)

