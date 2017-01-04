(defconst site-lisp-directory "~/.emacs.d/site-lisp")

(require 'cedet)
(require 'semantic)
(require 'cedet-files)
(require 'ede)
(require 'eieio)
(require 'speedbar)

;;; enable the features
(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-mru-bookmark-mode 1)

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

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded recursive))

;;; semantic-idle-scheduler-idle-time

;;; global configuration
(require-package 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))


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

;;; ede for c & c++ setting
(global-ede-mode t)

;; (defvar w32-include-path-list
;;   '("D:/ProgEnv/MinGW/lib/gcc/mingw32/4.8.1/include/c++"))

;; (defvar x-include-path-list
;;   '(""))

;; (defvar include-path-list (if *is-windows-system-p* w32-include-path-list x-include-path-list))
;; (defvar-mode-local c++-mode semantic-dependency-system-include-path
;;   `(,@include-path-list
;;     "include" "../include" "inc" "../inc" "~/utils/include" "~/utils/src/"))

(when (not (file-exists-p "~/.emacs.d/init-ede-projects.el"))
  (copy-file "~/.emacs.d/init-ede-projects.sample.el" "~/.emacs.d/init-ede-projects.el"))

(require 'init-ede-projects)

;; helper for boost setup...
(defun cedet-files-list-recursively (dir re)
  "Returns list of files in directory matching to given regex"
  (when (file-accessible-directory-p dir)
    (let ((files (directory-files dir t))
          matched)
      (dolist (file files matched)
        (let ((fname (file-name-nondirectory file)))
          (cond
           ((or (string= fname ".")
                (string= fname "..")) nil)
           ((and (file-regular-p file)
                 (string-match re fname))
            (setq matched (cons file matched)))
           ((file-directory-p file)
            (let ((tfiles (cedet-files-list-recursively file re)))
              (when tfiles (setq matched (append matched tfiles)))))))))))

(defun c++-setup-boost (boost-root)
  (when (file-accessible-directory-p boost-root)
    (let ((cfiles (cedet-files-list-recursively boost-root "\\(config\\|user\\)\\.hpp")))
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

(require 'auto-complete)
(defun my-ac/cedet-hook ()
  ;; (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-ac/cedet-hook)

;; (defun ac-semantic-complete-self-insert (arg)
;;   (interactive "p")
;;   (self-insert-command arg)
;;   (ac-complete-semantic))
;; (defun my-cedet-self-insert-hook ()
;;   (local-set-key "." 'ac-semantic-complete-self-insert)
;;   (local-set-key ">" 'ac-semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-cedet-self-insert-hook)
;; (remove-hook 'c-mode-common-hook 'my-cedet-self-insert-hook)
;;; keymap settings
(defun my-local-set-keys (prefix map-lists)
  (let ((help-message "") temp-key)
    (dolist (mlist map-lists)
      (dolist (m mlist)
        (setq temp-key
              (if (characterp (car m))
                  (char-to-string (car m))
                (car m))
              help-message
              (concat help-message
                      temp-key
                      " "
                      (symbol-name (cdr m))"\n"))
        (local-set-key (concat prefix temp-key) (cdr m))))
    (local-set-key (concat prefix "?") `(lambda () (interactive) (message ,help-message)))))

(defun my-set-global-keys (prefix map-lists)
  (let ((help-message "") temp-key)
    (dolist (mlist map-lists)
      (dolist (m mlist)
        (setq temp-key
              (if (characterp (car m))
                  (char-to-string (car m))
                (car m))
              help-message
              (concat help-message
                      temp-key
                      " "
                      (symbol-name (cdr m))"\n"))
        (global-set-key (concat prefix temp-key) (cdr m))))
    (global-set-key (concat prefix "?") `(lambda () (interactive) (message ,help-message)))))

(defconst my-semantic-map
  '((?i . semantic-ia-fast-jump)
    (?p . semantic-analyze-proto-impl-toggle)
    (?b . semantic-mrub-switch-tags)
    (?s . semantic-ia-show-summary)
    ;;(?d . semantic-ia-show-doc)
    ;; tag folding
    (?f . semantic-tag-folding-fold-block)
    (?o . semantic-tag-folding-show-block)
    (?- . semantic-tag-folding-fold-all)
    (?+ . semantic-tag-folding-show-all)
    ;; complete
    (?m . semantic-ia-complete-symbol-menu)
    (?c . semantic-ia-complete-symbol)
    (?t . semantic-ia-complete-tip))
  "the map of semantic")

(require 'eassist)


(add-to-list 'eassist-header-switches '("hh" "cpp" "cc"))
(add-to-list 'eassist-header-switches '("cc" "h" "hh"))

(setq lib-include-replace-list '(("/libs/" "/include/")
                                 ("/lib" "/include/")
                                 ("/media/libstagefright/" "/include/media/stagefright/")
                                 ("/base/native/android/" "/native/include/android/")))

(defun my-eassist-switch-h-cpp ()
  (interactive)
  (when (equal (eassist-switch-h-cpp) "There is no corresponding pair (header or body) file.")
    (let ((fname (buffer-file-name))
          sufname
          bname
          pname)
      (catch 'file-found
        (dolist (lib-include-pair lib-include-replace-list)
          (let ((first (car lib-include-pair))
                (second (cadr lib-include-pair)))
            (setq sufname (file-name-extension fname)
                  bname (file-name-sans-extension fname)
                  pname (replace-regexp-in-string first second bname))
            (when (equal pname bname)
              (setq pname (replace-regexp-in-string second first bname)))
            ;; (message "sufname %s\n bname %s\n pname %s\n" sufname bname pname)
            (dolist (suf (cdr (assoc sufname eassist-header-switches)))
              (let ((toname (concat pname "." suf)))
                ;; (message toname)
                (when (file-exists-p toname)
                  (find-file-other-window toname)
                  (throw 'file-found nil))))))))))

;;; DEBUG: the flet is obsolete, fix it when it is not bound.
(defun my-new-eassist-switch-h-cpp (arg)
  (interactive "P")
  (if arg
      (if (fboundp 'flet)
          (flet ((find-file-other-window (name &optional wildcard) (find-file name wildcard))
                 (switch-to-buffer-other-window (name &optional norecord) (switch-to-buffer name norecord)))
            (my-eassist-switch-h-cpp))
        (my-eassist-switch-h-cpp))
    (my-eassist-switch-h-cpp)))


(defconst my-eassist-map
  '(("g" . my-new-eassist-switch-h-cpp)
    ("l" . eassist-list-methods)
    ;;("r" . semantic-symref)
    ))

(defconst my-ggtags-map
  '(("j" . ggtags-find-tag-dwim)
    ("d" . ggtags-find-definition)
    ("r" . ggtags-find-reference)))

(require 'cc-mode)
(add-hook 'c-mode-common-hook (lambda () (my-local-set-keys "\C-c\C-s" `(,my-semantic-map ,my-eassist-map ,my-ggtags-map))))

(require 'init-cmake)
(defun my-semantic-include-dirs ()
  (interactive)
  (let (maybe-cmake file-dir dir-list)
    (setq maybe-cmake (get-maybe-cmake))
    (when maybe-cmake
      (setq file-dir (file-name-directory maybe-cmake))
      (setq dir-list (cddr (directory-files file-dir)))
      (setq dir-list (remove-if-not (lambda (x) (file-directory-p (concat file-dir x))) dir-list))
      (setq dir-list (mapcar (lambda (x) (concat file-dir x)) dir-list)))
     dir-list))

;; doesn't work
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (setq-local semantic-dependency-include-path (my-semantic-include-dirs))))

;; (require 'lisp-mode)
;; (add-hook 'emacs-lisp-mode-hook (lambda () (my-local-set-keys "\C-c\C-s" `(,my-semantic-map))))

;; (require 'js)
;; (add-hook 'js-mode-hook (lambda () (my-local-set-keys "\C-c\C-s" `(,my-semantic-map))))

(provide 'init-cedet)
