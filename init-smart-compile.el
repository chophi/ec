(require-package 'smart-compile)
(require 'smart-compile)

(define-derived-mode cv-mode c++-mode "OpenCV" "mode for editing opencv files")
(setq smart-compile-alist
      '((emacs-lisp-mode emacs-lisp-byte-compile)
        (html-mode browse-url-of-buffer)
        (nxhtml-mode browse-url-of-buffer)
        (html-helper-mode browse-url-of-buffer)
        (octave-mode run-octave)
        (groovy-mode . "groovy %f")
        ;; (cv-mode . "g++ -O2 -Wall -g -std=gnu++0x  `pkg-config --cflags opencv` %f `pkg-config --libs opencv` -o %n")
        ("\\.c\\'" . "gcc -O2 -Wall -g %f -lm -o %n")
        ("\\.[Cc]+[Pp]*\\'" my-compile-cpp-file-function)
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

(define-minor-mode boost-minor-mode "Just for Compile boost")
(defvar boost-include-flags
  "-I/opt/boost/1.54.0/include/")
(defvar boost-lib-load-flags
  "-L/opt/boost/1.54.0/lib/ -lboost_system -lboost_filesystem \
-lboost_log -lboost_date_time -lboost_thread -lboost_log_setup")

;; (defconst opencv-include-flags
;;   (let ((result ""))
;;     (dolist (var (split-string (getenv "OPENCV_INCLUDE_DIR") ":") result)
;;       (setq result (concat result "-I" var " "))))
;;   )
;; (defconst opencv-libs-flags
;;   (format "-L%s %s" (getenv "OPENCV_LIB_DIR")
;;           (let ((result ""))
;;             (dolist (var '("highgui" "imgproc" "core" "features2d" "ml") result)
;;               (setq result (concat result (format " -lopencv_%s%s" var (getenv "OPENCV_VERSION_STR"))))))))
(defconst opencv-include-flags
  "`pkg-config --cflags opencv`")
(defconst opencv-libs-flags
  "`pkg-config --libs opencv`")
(defun mingw-directory-rep (str)
  (replace-regexp-in-string "/cygdrive/\\\([cdefghij]\\\)" "\\1:" str))

(defun my-compile-cpp-file-function ()
  (interactive)
  (let (include-flags lib-flags)
    (if (eq major-mode 'cv-mode)
        (setq include-flags opencv-include-flags
              lib-flags opencv-libs-flags)
      (setq include-flags ""
            lib-flags ""))
    (if boost-minor-mode
        (setq include-flags (concat include-flags " " boost-include-flags)
              lib-flags (concat lib-flags " " boost-lib-load-flags)))
    (compile (mingw-directory-rep (format "g++ -O2 -Wall -g -std=gnu++0x %s -I~/utils/src %s -o %s %s"
                                          include-flags
                                          (file-name-nondirectory (buffer-file-name))
                                          (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
                                          lib-flags)))))
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
(define-key c-mode-base-map "\C-c\C-c" 'smart-compile)
(define-key c-mode-base-map "\C-c\S-c" 'recompile-quietly)

(provide 'init-smart-compile)
