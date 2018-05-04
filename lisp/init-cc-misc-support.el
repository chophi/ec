(require 'cedet)
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
;; (require 'cedet-files)
;; (require 'ede)
;; (require 'eieio)
;; (require 'speedbar)

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-mru-bookmark-mode
        global-semantic-stickyfunc-mode
        global-semantic-idle-completions-mode
        global-semantic-highlight-func-mode
        global-semantic-decoration-mode))

(setq semanticdb-default-save-directory "~/.semanticdb/")
(semantic-mode t)

;;; set the default-directory to the Kbuild file to avoid pop selection prompt.
(setq project-linux-build-directory-default 'same)
(setq project-linux-architecture-default "arm64")

;;; add system include dirs for some modes.
;;; eg. (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
;;; boost support need to specify where to find constant's definitions.

;;; work optimation
;; - limit search by using an EDE project
;; - explicitly specify a list of root directories for your projects, so Semantic
;;   will use limited number of databases with syntactic information
;; - explicitly generate tags databases for often used directories (/usr/include,
;;   /usr/local/include, etc.). You can use commands semanticdb-create-ebrowse-database
;;   or semanticdb-create-cscope-database
;; - limit search by customization of the semanticdb-find-default-throttle
;;   variable for concrete modes — for example, don't use information from
;;   system include files, by removing system symbol from list of objects to search for c-mode:
;;   eg. (setq-mode-local c-mode semanticdb-find-default-throttle '(project unloaded system recursive))

;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(project unloaded recursive))
;; (setq-mode-local c++-mode semanticdb-find-default-throttle
;;                  '(project unloaded recursive))

;;; semantic-idle-scheduler-idle-time

;;; check and add gtags or exubertant ctags support, if found suitable gtags and ctags.
;; FIXME: it open too many buffers when enable global for c-mode
;; (require 'cedet-global)
;; ;; if you want to enable support for gnu global
;; (when (cedet-gnu-global-version-check t)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode)
;;   )

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; (when (cedet-ectag-version-check)
;;   (semantic-load-enable-primary-exuberent-ctags-support))

(defun c++-setup-boost (boost-root)
  (when (file-accessible-directory-p boost-root)
    (let ((cfiles
           (cu-list-files-recursively boost-root "\\(config\\|user\\)\\.hpp")))
      (dolist (file cfiles)
        (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))

;; (c++-setup-boost "/usr/local/include/boost")
;; setting opencv
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_PROP_RW" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS_W_SIMPLE" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS_W" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS_W_MAP" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_INLINE" . ""))

;;; preprocessing of source code
;; (setq qt4-base-dir "/usr/include/qt4")
;; (semantic-add-system-include qt4-base-dir 'c++-mode)
;; (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

;;; using ede for java projects
;; (require 'semantic/db-javap)
;; rt.jar may auto detected by (cedet-java-find-jdk-core-jar)
;; if not using maven, semanticdb-javap-classpath or java-root-project variable
;; setting is required to specify all used library:
;; (ede-java-root-project "TestProject"
;;          :file "~/work/TestProject/build.xml"
;;          :srcroot '("src" "test")
;;          :localclasspath '("/relative/path.jar")
;;          :classpath '("/absolute/path.jar"))

(with-eval-after-load "auto-complete"
  (defun ac-semantic-hook ()
    (add-to-list 'ac-sources 'ac-source-semantic))
  (add-hook 'c-mode-common-hook 'ac-semantic-hook))

(require 'eassist)

(add-to-list 'eassist-header-switches '("hh" "cpp" "cc"))
(add-to-list 'eassist-header-switches '("cc" "h" "hh"))

(defconst eassist-switch-h-cpp-post-pairs
  '(("/libs/" "/include/")
    ("/lib" "/include/")
    ("/media/libstagefright/" "/include/media/stagefright/")
    ("/base/native/android/" "/native/include/android/"))
  "The replace pairs for post processing eassist-switch-h-cpp")

(defun eassist-switch-h-cpp-try-replace (arg)
  "A wrapper for `eassist-switch-h-cpp'.
It try to do a string replace with `eassist-switch-h-cpp-post-pairs' and will
find the existed files with the replaced result."
  (interactive "P")
  ;; Can't find the pair
  (when (equal (eassist-switch-h-cpp)
               "There is no corresponding pair (header or body) file.")
    (catch 'file-found
      (dolist (pair (append eassist-switch-h-cpp-post-pairs
                            (mapcar 'reverse eassist-switch-h-cpp-post-pairs)))
        (let* ((get-a-replaced-name
                (lambda (filename switch-pair)
                  (let ((replaced-name
                         (replace-regexp-in-string
                          (car switch-pair) (cadr switch-pair) filename)))
                    (when (not (equal replaced-name filename)) replaced-name))))
               (filename (buffer-file-name))
               (replaced-name (funcall get-a-replaced-name filename pair)))
          (when replaced-name
            (let ((suffix-name (file-name-extension replaced-name)))
              (dolist (assoc-suffix
                       (cdr (assoc suffix-name eassist-header-switches)))
                (let ((target-name (format "%s.%s" (file-name-sans-extension replaced-name)
                                           assoc-suffix)))
                  (when (file-exists-p target-name)
                    (find-file target-name)
                    (throw 'file-found nil)))))))))))

(defadvice eassist-switch-h-cpp-try-replace (around eassist-switch-h-cpp-ad)
  "Rebind the find-file nad switch-to-buffer to open the found file in another
window side by side"
  (if (and (not arg) (fboundp 'cl-letf))
      (cl-letf
          (;; rebind find-file and switch-to-buffer to make the eassist
           ;; switch to file in another window
           ((symbol-function 'find-file) 'find-file-other-window)
           ((symbol-function 'switch-to-buffer) 'switch-to-buffer-other-window))
        (message "function find-file and switch-to-buffer was rebinded")
        ad-do-it)
    ad-do-it))

(ad-activate 'eassist-switch-h-cpp-try-replace)

(provide 'init-cc-misc-support)
