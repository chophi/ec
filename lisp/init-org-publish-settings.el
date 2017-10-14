;;-----------------------------------------------------------------------------
;; cn-article class settings.
;;-----------------------------------------------------------------------------
(require 'org-cn-article-prefix)
(defvar cn-article-headlines nil
  "the headline for cn-article latex export")

(let ((cn-article-headlines
       (concat cn-article-prefix
               "\n[NO-DEFAULT-PACKAGES]"
               "\n[EXTRA]"
               "\n[PACKAGES]")))
  (add-to-list 'org-latex-classes
               `("cn-article" ,cn-article-headlines
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;;-----------------------------------------------------------------------------
;; beamer class settings.
;;-----------------------------------------------------------------------------
(require 'org-beamer-prefix)
(defvar org-beamer-headlines nil
  "the headline for beamer latex export")
(let ((org-beamer-headlines
       (concat org-beamer-prefix
               "\n[NO-DEFAULT-PACKAGES]"
               "\n[EXTRA]"
               "\n[PACKAGES]")))
  (add-to-list 'org-latex-classes
               `("beamer" ,org-beamer-headlines
                 ("\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"
                  "\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"))))

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
   ,(when (not (equal (substring emacs-version 0 2) "27"))
        '(sh . t))
   (ditaa . t)
   (python . t)
   (haskell . t)
   (dot . t)
   (latex . t)
   (js . t)
   (ditaa . t)))

;; load ditaa 
;; (setq org-ditaa-jar-path "/usr/bin/ditaa")

;;-----------------------------------------------------------------------------
;; org latex export listings setting
;;-----------------------------------------------------------------------------
(defun org-listing-use-minted()
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "orgminted") t))
(org-listing-use-minted)

;; Options for \lset command（reference to listing Manual)
;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-latex-minted-options
      '(("linenos" "true")
	;; ("bgcolor" "mint-background-0")
	;; ("samepage" "false")
	("fontfamily" "tt")
	("fontsize" "\\footnotesize")
	("mathescape" "true")
	("frame" "single")
	))
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
;; it seems cann't work on windows.


;;-----------------------------------------------------------------------------
;; export command setting
;;-----------------------------------------------------------------------------
;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd (exporter)
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  ;; (print exporter)
  (when (eq exporter 'latex)
    (let ((texcmd))
      ;; default command: oldstyle latex via dvi
      (setq texcmd "latexmk -dvi -pdfps -shell-escape %f")
      ;; pdflatex -> .pdf
      (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
          (setq texcmd "latexmk -pdf %f"))
      (setq org-latex-pdf-process (list texcmd))
      
      ;; xelatex -> .pdf
      (if (or (string-match "LATEX_CMD: xelatex" (buffer-string))
              (string-match "LATEX_CLASS: cn-article" (buffer-string)))
          ;; "latexmk -pdflatex=xelatex -interaction=nonstopmode -shell-escape %f"
          (setq texcmd "xelatex -interaction=nonstopmode -shell-escape %f"
                org-latex-pdf-process (list texcmd texcmd texcmd))
        (setq texcmd "latexmk -pdf %f")
        ))))

(add-hook 'org-export-before-processing-hook 'my-auto-tex-cmd)

;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode -output-directory  %o -shell-escape %f"
;;         "xelatex -interaction nonstopmode -output-directory  %o -shell-escape %f"
;;         "xelatex -interaction nonstopmode -output-directory  %o -shell-escape %f"))
;; (setq org-export-publishing-directory nil)

(defconst xelatex-temp-ext-list
  '(".pyg" ".fls" ".fdb_latexmk" ".aux" ".log" ".out" ".toc" ".bbl")
  "xelatex temp file extention list")

(defun my-trash-xelatex-temp-files ()
  (interactive)
  (let ((base-name (file-name-sans-extension
                    (file-name-nondirectory
                     (buffer-file-name)))))
    (dolist (ext xelatex-temp-ext-list)
      (when (file-exists-p (concat base-name ext))
        (delete-file (concat base-name ext) t)))))

;;-----------------------------------------------------------------------------
;; export html css setting
;;-----------------------------------------------------------------------------
(setq org-html-style-include-default nil)
(defvar css-sanityinc-tomorrow-eighties ""
  "css simulate the color theme sanityinc-tommorow-eighties")

(defun org-export-use-style-file ()
  (interactive)
  (insert (format "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />\n"
                  (file-relative-name
                   (concat "~/css/"
                           (ido-completing-read "Select CSS file: " (cddr (directory-files "~/css"))))))))

(setq css-sanityinc-tomorrow-eighties
      "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: sans-serif; font-size: 12pt;}
  .title  { text-align: center; color: #f99157}
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  p, ul, ol {-webkit-margin-before: 0em; -webkit-margin-after: 0em;}
  div[id^=\"text-table-of-contents\"] {color: #99ffff}
  [href] {color: #ff7f00;}
  h3[id^=\"sec\"] {color: #9aff9a}
  h4[id^=\"sec\"] {color: #9aff9a}
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #a9a9a9;
	padding: 5pt;
	font-family: consolas, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>")

;; use the tomorrow-eighties style
(setq org-html-style css-sanityinc-tomorrow-eighties)

;; prevent show the char of seperate line of fci-mode in the html
(require-package 'htmlize)
(add-hook 'htmlize-before-hook (lambda () (when (fboundp 'fci-mode) (fci-mode -1))))
(setq htmlize-html-charset "utf-8")
(setq org-html-postamble nil)
(setq org-hide-emphasis-markers t)

(provide 'init-org-publish-settings)

