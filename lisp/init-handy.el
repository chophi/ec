;;; sometimes need to insert a path, I'd like insert a link just like in
;;; org-mode
(require 'ido)

(defconst often-used-mode-list
  '(emacs-lisp-mode
    lisp-mode
    clojure-mode
    scheme-mode
    haskell-mode
    ruby-mode
    rspec-mode
    c-mode
    c++-mode
    objc-mode
    latex-mode
    js-mode
    plain-tex-mode
    racket-mode)
  "Some often used mode which I'd like to enable some features
like: auto indent after pasting, fci mode, etc..")

;;; auto indent when paste some code.
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode often-used-mode-list)
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

(defvar file-explorer-command
  (case os
    ('macos "open")
    ('linux "gnome-open")
    ('windows "explorer")
    (t (error "unknown system, can't set file-explorer-command")))
  "Command to open file in file explorer")

(defun my-explore-curdir()
  (interactive)
  (shell-command
   (format "cd \"%s\" && %s ." default-directory file-explorer-command)))

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

;; change backup and auto-save directory
(setq backup-directory-alist '(("[:ascii:]*" . "~/.emacs.d/backup-file")))

(let ((auto-save-file-directory "~/.emacs.d/auto-save-file"))
  (when (not (file-exists-p auto-save-file-directory))
    (make-directory auto-save-file-directory))
  (setq auto-save-file-name-transforms 
        `((".*" ,(concat auto-save-file-directory "/\\2") t))))

(setq visible-bell t
      ring-bell-function (lambda () (message "Try to ring bell")))

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around set-null-device activate)
  "Solve two problems:
null-device: Use cygwin's /dev/null as the null-device
grep-use-null-filename-separator: the old zgrep doesn't support the --null option"
  (let ((null-device "/dev/null")
        (grep-use-null-filename-separator nil))
    ad-do-it))

(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIXME\\|MODIFIED\\|DEBUG\\|UPDATED\\|TODO\\):"
       1 'font-lock-warning-face prepend)))))

(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "\C-xf") 'other-frame)

(defvar scratch-dir "~/scratch" "The directory to store the scratch files")

(defun my-scratch()
  "Per mode scratch, open a file under `scratch-dir' with name scratch.`major-mode'"
  (interactive)
  (let ((dir scratch-dir)
        (mode major-mode))
    (make-directory dir t)
    (find-file-other-window
     (cu-join-path dir (format "scratch.%s" (symbol-name mode))))
    (funcall mode)))

(require-package 'whole-line-or-region)

(with-parameters-bounded
 '(out-clipboard-host-user)

 (defun kill-save-to-out-clipboard (start end)
   (let ((command
          (format
           "echo %s | ssh %s@%s pbcopy"
           (shell-quote-argument (buffer-substring-no-properties start end))
           out-clipboard-host-user (out-clipboard-hostname))))
     ;; (message "command is %s" command)
     (shell-command-to-string command)
     (message "The content in selection is sent to out host's clipboard")))

 (defun save-whole-line-or-region-to-out-clipboard (prefix)
   "Copy region or PREFIX whole lines to outer clipboard."
   (interactive "p")
   (whole-line-or-region-call-with-region 'kill-save-to-out-clipboard prefix t))

 (global-set-key "\C-xy" 'save-whole-line-or-region-to-out-clipboard))

(defun my-sudo-edit ()
  "Edit the file that is associated with current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(defun my-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(provide 'init-handy)
