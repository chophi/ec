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
(if *mac?*
    ;; installed by brew install coreutils
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"))



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

(provide 'init-dired)
