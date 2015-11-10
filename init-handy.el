;;; sometimes need to insert a path, I'd like insert a link just like in
;;; org-mode
(require 'ido)

(defun insert-absolute-path(absolute)
  (interactive "P")
  (let ((path (ido-read-file-name "Insert a Abstract: ")))
    (if absolute
        (insert path)
      (insert (replace-regexp-in-string (getenv "HOME") "~" path )))))
(define-key global-map "\C-c\C-l" 'insert-absolute-path)


;;; auto indent when paste some code.
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;;; I'd like to hungry-delete everywhere.
(defmacro my-skip-ws-backward (&optional limit)
  "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace and line
continuations."
  (if limit
      `(let ((limit (or ,limit (point-min))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-backward " \t\n\r\f\v" limit)
		  (and (eolp)
		       (eq (char-before) ?\\)
		       (> (point) limit)))
	   (backward-char)))
    '(while (progn
	      (skip-chars-backward " \t\n\r\f\v")
	      (and (eolp)
		   (eq (char-before) ?\\)))
       (backward-char))))

(defun my-hungry-delete-backwards ()
  "Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also \\[c-hungry-delete-forward]."
  (interactive)
  (let ((here (point)))
    (my-skip-ws-backward)
    (if (/= (point) here)
	(delete-region (point) here)
      (backward-delete-char-untabify 1))))
(define-key global-map (kbd "<backspace>") 'my-hungry-delete-backwards)

;;; match paren with %
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doc-view-mode-maybe is boring, here i delete doc associatiation with it.
(delete (rassoc 'doc-view-mode-maybe auto-mode-alist) auto-mode-alist)

(setq explorer-command
      (if (eq system-type 'gnu/linux) "gnome-open"
        (if *is-mac-machine*
            "open"
          "explorer")))
(defun my-explore-curdir()
  (interactive)
  (shell-command (concat "cd \"" default-directory "\" && " explorer-command " .")))

;;----------------------------------------------------------------------------
;; if no region selected, comment/uncomment current line
;;----------------------------------------------------------------------------
(defun comment-line-dwim (&optional arg)
  "Replacement for the comment-dwim command.\
If no region is selected and current line is not blank and we are not at the \
end of the line, then comment current line.\
Replaces default behaviour of comment-dwim, when it inserts comment at the \
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-unset-key "\M-;")
(global-set-key "\M-;" 'comment-line-dwim)


(fset 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output 1)
;;; I need the echo area to see key bind message, sometimes it may be a long list.
(setq max-mini-window-height 0.5)


;; change backup and auto-save directory
(setq backup-directory-alist 
      '(("[:ascii:]*" . "~/.emacs.d/backup-file")))

(let ((auto-save-file-directory "~/.emacs.d/auto-save-file"))
  (when (not (file-exists-p auto-save-file-directory))
    (make-directory auto-save-file-directory))
  (setq auto-save-file-name-transforms 
        `((".*" ,(concat auto-save-file-directory "/\\2") t))))

(defun my-practise-dir ()
  (interactive)
  (dired "~/practise"))

(when (eq window-system 'w32)
  (setq visible-bell t)
  (setq desktop-dirname "d:/emacs-24.3/")
  (setq smart-compile-make-program "mingw32-make ")  
  )

(when *is-mac-machine*
  (defun my-ring-bell-function ()
    (message "try to ring bell"))
  (setq visible-bell t
        ring-bell-function 'my-ring-bell-function))

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '( ;; ("\\<\\(and\\|or\\|not\\)\\>" . 'font-lock-keyword-face)
                                      ("\\<\\(FIXME\\|MODIFIED\\|DEBUG\\|UPDATED\\|TODO\\):" 1 'font-lock-warning-face prepend)))))

(setq split-height-threshold nil
      split-width-threshold 100)

(global-set-key (kbd "C-!") 'shell-command)

(global-set-key (kbd "\C-xf") 'other-frame)

(defun handy-scratch()
  (interactive)
  (make-directory "~/scratch" t)
  (let ((pre-major-mode major-mode))
    (find-file-other-window (concat "~/scratch/scratch-" (symbol-name pre-major-mode)))
    (funcall pre-major-mode)
    ))


(provide 'init-handy)
