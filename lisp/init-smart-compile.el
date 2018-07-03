(require-package 'smart-compile)

(defvar smart-compile-state nil
  "Is it compiling, run, or doing something else?")

(defvar smart-compile-compile-alist
      '((emacs-lisp-mode emacs-lisp-byte-compile)
        (html-mode browse-url-of-buffer)
        (nxhtml-mode browse-url-of-buffer)
        (html-helper-mode browse-url-of-buffer)
        (octave-mode run-octave)
        (groovy-mode . "groovy %f")
        ("\\.c\\'" . "gcc -O2 -Wall -g %f -lm -o %n")
        ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 -Wall -g -std=gnu++0x %f -lm -o %n")
        ("\\.m\\'" . "gcc -O2 %f -lobjc -lpthread -o %n")
        ("\\.java\\'" . "javac %f")
        ("\\.php\\'" . "php -l %f")
        ("\\.f90\\'" . "gfortran %f -o %n")
        ("\\.[Ff]\\'" . "gfortran %f -o %n")
        ("\\.cron\\(tab\\)?\\'" . "crontab %f")
        ("\\.tex\\'" tex-file)
        ("\\.texi\\'" . "makeinfo %f")
        ("\\.mp\\'" . "mptopdf %f")
        ("\\.pl\\'" . "perl -cw %f")
        ("\\.rb\\'" . "ruby -cw %f")))

(defvar smart-compile-run-alist
  '(("\\.c\\'" . "./%n")
    ("\\.[Cc]+[Pp]*\\'" . "./%n")
    ("\\.m\\'" . "./%n")
    ("\\.java\\'" . "java %n")))

(defun smart-compile-compile ()
  (interactive)
  (setq smart-compile-state 'compile)
  (let ((smart-compile-alist smart-compile-compile-alist))
    (smart-compile 4)))

(defun smart-compile-run ()
  (interactive)
  (setq smart-compile-state 'run)
  (let ((smart-compile-alist smart-compile-run-alist))
    (smart-compile 4)))

(defun smart-compilation-complete-callback (buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (case smart-compile-state
    (compile (smart-compile-compile-callback buffer msg))
    (run (smart-compile-run-callback buffer msg))
    (t (smart-compile-common-callback buffer msg)))
  (setq smart-compile-state nil))

(defun smart-compile-compile-callback (buffer msg)
  (if (string-match "^finished" msg)
      (progn
        (with-current-buffer buffer
          (or (string-match "warning:" (buffer-string))
              (delete-windows-on buffer)))
        (tooltip-show "\n Process exit Successful :-) \n "))
    (tooltip-show "\n Process exit Failed :-( \n ")))

(defun smart-compile-run-callback (buffer msg)
  "do nothing")

(defun smart-compile-common-callback (buffer msg)
  "do nothing")

(add-to-list 'compilation-finish-functions
             'smart-compilation-complete-callback)


(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))

(require 'cc-mode)

(provide 'init-smart-compile)
