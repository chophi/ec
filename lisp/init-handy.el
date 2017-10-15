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
        (if *mac?*
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


(defun my-ring-bell-function ()
  (message "try to ring bell"))
(setq visible-bell t
      ring-bell-function 'my-ring-bell-function)

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

(global-set-key (kbd "C-!") 'shell-command)

(global-set-key (kbd "\C-xf") 'other-frame)

(defun handy-scratch()
  (interactive)
  (make-directory "~/scratch" t)
  (let ((pre-major-mode major-mode))
    (find-file-other-window (concat "~/scratch/scratch-" (symbol-name pre-major-mode)))
    (funcall pre-major-mode)
    ))




(defun my-write-path ()
  (interactive)
  (let ((dir  default-directory))
    (shell-command (format "echo %s > ~/.emacs-default-directory" dir))))

(require-package 'whole-line-or-region)

;; Please define out-clip-host-user and out-clipboard-hostname
;; in lisp-snippets/az-common-env.el first
(when (file-exists-p "~/.emacs.d/lisp-snippets/az-common-env.el")
  (load-file "~/.emacs.d/lisp-snippets/az-common-env.el")
  (when (and (stringp out-clipboard-host-user) (stringp out-clipboard-hostname))
    (defun kill-save-to-out-clipboard (start end)
      (let ((command (format "echo %s | ssh %s@%s pbcopy" (shell-quote-argument (buffer-substring-no-properties start end)) out-clipboard-host-user out-clipboard-hostname)))
        ;; (message "command is %s" command)
        (shell-command-to-string command)
        (message "The content in selection is sent to out host's clipboard")))
    (defun save-whole-line-or-region-to-out-clipboard (prefix)
      "Copy region or PREFIX whole lines to outer clipboard."
      (interactive "p")
      (whole-line-or-region-call-with-region 'kill-save-to-out-clipboard prefix t))
    (global-set-key "\C-xy" 'save-whole-line-or-region-to-out-clipboard)))

(defun cp-under-dir-has-sub (regex &optional path)
  (when (not path) (setq path default-directory))
  (let ((parts (split-string path "/"))
        (ret nil))
    (dolist (part parts ret)
      (when (string-match regex part)
        (message "found matching %s" part)
        (setq ret t)))))


(defun cp-detect-context (&optional path)
  (when (and (cp-under-dir-has-sub "Chapter_[0-9]+$")) 'cp-opengles3-book))

(defun get-file-content-as-string (filename)
  "Return the content in filename as a string"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun eval-file-as-lisp-expression (filename)
  "Return the eval result of filename as expression"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (buffer-end 1))
    (eval-last-sexp t)
    ))

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
;; (unless window-system
(when (and  (getenv "DISPLAY")
            *linux?*
            (not (equal (shell-command-to-string "which xsel") "")))
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function ()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output)))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    )
;; )

(defun __is_base64_char (c)
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9))
      (eq c ?+)
      (eq c ?/)
      (eq c ?=)))

(defun __valid_base64_string? (string)
  (equal (shell-command-to-string (format "echo %s | base64 -d 2>&1 | grep \"invalid input\"" string))
         ""))

(defun __decode_base64_may_append_equal (string)
  (if (__valid_base64_string? string)
      (base64-decode-string string)
    (if (__valid_base64_string? (concat string "="))
        (base64-decode-string  (concat string "="))
      (if (__valid_base64_string? (concat string "=="))
          (base64-decode-string (concat string "=="))
        string))))

(defun __convert-all-to-base64 (string)
  (let ((len (length string))
        (beg 0)
        (end 0)
        (ret ""))
    (while (< end len)
      (when (not (__is_base64_char (elt string end)))
        (if (> end beg)
            (setq ret (format "%s%s%c" ret
                              (__decode_base64_may_append_equal (substring string beg end))
                              (elt string end)))
          (setq ret (format "%s%c" ret (elt string end))))
        (setq beg (1+ end)))
      (setq end (1+ end)))
    (when (__is_base64_char (elt string (1- len)))
      (setq ret (format "%s%s" ret
                        (__decode_base64_may_append_equal (substring string beg len)))))
    ret))

(defun my-region-base64-decode (start end)
  "Convert the select region from base64 to string"
  (interactive "*r")
  (let* ((content (buffer-substring-no-properties start end))
         (result (__convert-all-to-base64 content))
         (selected (y-or-n-p (format "The output is as following(y/n)? \n %s \n:" result))))
    (when selected
      (delete-region start end)
      (insert result))))

(defun my-func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun my-hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (my-func-region start end #'url-hexify-string))

(defun my-unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (my-func-region start end #'url-unhex-string))

(defun my-region-base64-decode-to-hex-string (start end)
  "Convert the select region from base64 to hex string"
  (interactive "*r")
  (let* ((content (buffer-substring-no-properties start end))
         (result (__convert-all-to-base64 content))
         (selected (y-or-n-p (format "The output is as following(y/n)? \n %s \n:" result))))
    (when selected
      (delete-region start end)
      (insert (url-hexify-string result)))))

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

(defun my-open-link ()
  (interactive)
  (defun is-separator (str i)
    (let ((ret nil)
          (ch (elt str i)))
      (dolist (c (append "\n,\"; :" nil) ret)
        (when (equal ch c) (setq ret t)))
      ))
  (let* ((start (max (- (point) 256) 1))
         (end (min (+ (point) 256) (buffer-size)))
         (cur (- (point) start))
         (str (buffer-substring-no-properties start end))
         (len (length str))
         (start (do ((i cur))
                    ((or (< i 0) (is-separator str i)) (1+ i))
                  (decf i)))
         (end (do ((i cur))
                  ((or (>= i len) (is-separator str i)) i)
                (incf i)))
         (maybe-filename (substring str start end)))
    (when (file-exists-p maybe-filename)
      (find-file-other-window maybe-filename))))

(provide 'init-handy)
