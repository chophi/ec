(require-package 'dired+)

;; tried but choose not to
;; (require-package 'dired-k)
;; (define-key dired-mode-map (kbd "K") 'dired-k)

;; tried but choose not to
;; (require-package 'dired-fdclone)
;; (dired-fdclone)

;; tried but choose not to
;; (require-package 'dired-filter)
;; (define-key dired-mode-map (kbd "F") dired-filter-map)

(require-package 'dired-narrow)
(require 'dired)
(define-key dired-mode-map (kbd "F") 'dired-narrow)

;;; be available
(if (eq os 'macos)
    ;; installed by brew install coreutils
    (let ((possible-ls-program "/usr/local/opt/coreutils/libexec/gnubin/ls"))
      (when (file-exists-p possible-ls-program)
        (setq insert-directory-program possible-ls-program))))

(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive
   (list
    (let ((print-level nil)
          (minibuffer-history-position 0)
          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
      (unwind-protect
          (read-from-minibuffer
           "Command: " (prin1-to-string (nth 0 command-history))
           read-expression-map t
           (cons 'command-history 0))
        
        ;; If command was added to command-history as a
        ;; string, get rid of that.  We want only
        ;; evaluable expressions there.
        (if (stringp (car command-history))
            (setq command-history (cdr command-history)))))))
  (dolist (filename (dired-get-marked-files))
    (with-current-buffer (find-file-noselect filename)
      (if (symbolp command)
          (call-interactively command)
        (eval command)))))

(define-key dired-mode-map (kbd "E") 'dired-do-command)

(require-package 'dired-narrow)
(define-key dired-mode-map (kbd "/")
  '(lambda ()
     (interactive)
     (let ((choice (read-char-choice
                    "Please choose the filter mode\nn: dired-narrow\nf: dired-narrow-fuzzy\nr: dired-narrow-regexp\nInput: "
                    '(?n ?f ?r))))
       (case choice
         (?n (dired-narrow))
         (?f (dired-narrow-fuzzy))
         (?r (dired-narrow-regexp))))))

(defun my-dired-sort-func ()
  (interactive)
  (let (choice choice-switcher-list switcher)
    (setq choice (read-char
                  "Sort Dir By: s[size] x[extension] c[ctime] u[utime] t[time] o[no extra]")
          choice-switcher-list '((?s "S")
                                 (?x "X")
                                 (?c "ct")
                                 (?u "ut")
                                 (?t "t")
                                 (?o ""))
          switcher (cadr (assoc choice choice-switcher-list)))
    (dired-sort-other (concat dired-listing-switches switcher))))

(setq dired-listing-switches
      (concat "--group-directories-first -h " dired-listing-switches))

(require 'dired)
(define-key dired-mode-map (kbd "s") 'my-dired-sort-func)

(provide 'init-dired)
