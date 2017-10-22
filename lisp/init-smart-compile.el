(require-package 'smart-compile)

(setq smart-compile-alist
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

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
        ;; (with-current-buffer buffer
        ;;   (or (string-match "warning:" (buffer-string))
        ;;       (delete-windows-on buffer)))
        (tooltip-show "\n Process exit Successful :-) \n "))
    (tooltip-show "\n Process exit Failed :-( \n ")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'compilation-finish-functions
                         'notify-compilation-result)))

(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))

(require 'cc-mode)
;; (define-key c-mode-base-map "\C-c\C-c" 'smart-compile)
(define-key c-mode-base-map "\C-c\S-c" 'recompile-quietly)

(provide 'init-smart-compile)
