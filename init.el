;; Don't remove the below line as emacs want to see it as the first line even
;; it's commented out.
;; (package-initialize)

;;; Start to load the config files with debugger on
(setq debug-on-error t)

;;; Setup the load-path
(dolist (p `((,(concat user-emacs-directory "lisp") :required t)
             (,(concat user-emacs-directory "site-lisp") :required t)
             (,(concat user-emacs-directory "term") :required t)
             ("~/repo/amazon/tools/EmacsAmazonLibs/lisp")))
  (let ((dir (car p)))
    (when (and (plist-get p :required) (not (file-exists-p dir)))
      (error "Required dir {%s} doesn't exist" dir))
    (when (file-exists-p dir)
      (add-to-list 'load-path dir))))

;;; Set the caches/configurations' paths to the cache dir
(require '000.setup-cache-dirs)

;;; Require the custom settings
(setq custom-file "~/.emacs.d/lisp/000.custom.el")
(require '000.custom)
;;; Try without error if it's not exist
(require '000.private.custom nil t)

;;; Initialize the package
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;; The util functions should be implement without assuming any dependency loaded
;;; The dependencies should be loaded in utils by itself
(require '000.utils)

;;; *append* the directories in site-lisp to load-path
(dolist (dir (cu-list-files-recursively-general
              (cu-join-path user-emacs-directory "site-lisp")
              (lambda (p) (file-directory-p p))
              1 nil))
  (add-to-list 'load-path dir t))

;;; Load orders should be preload -> <user-emacs-directory>/lisp/init.*.el -> postload
(require '000.preload)

;; Skip some init file
(defun skip-init-file-function (filename)
  (or (string-match ".*/init-ivy.el" filename)
      (string-match ".*/init-emacs.el" filename)
      (string-match ".*/init-keybind.el" filename)
      (and (string-match ".*/init-notmuch.el" filename)
           (not (executable-find "notmuch")))))

;; load all init file <user-emacs-directory>/lisp/init.*.el except the skipped file
;; The init-* files should be loaded without assuming of the order of loading
(dolist (file (cu-list-files-recursively
               (cu-join-path user-emacs-directory "lisp") "init.*.el" nil))
  (let ((should-skip-file nil))
    (when (and (fboundp 'skip-init-file-function) (skip-init-file-function file))
      (setq should-skip-file t))
    (if should-skip-file
        (message "Skip init file: %s" file)
      (message "Loading init file: %s" file)
      (require (intern (file-name-nondirectory (file-name-sans-extension file)))))))

(require 'term-inside-ide-init)

(require '000.keybind)
(require '000.postload)

;;; Turn off the debugger
(setq debug-on-error nil)

