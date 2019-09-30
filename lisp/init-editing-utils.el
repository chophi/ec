;;; Adaptive wrap and visual mode
(with-eval-after-load 'adaptive-wrap
  (setq-default adaptive-wrap-extra-indent 2))

;;; Visual line mode
(add-hook 'visual-line-mode-hook
          (lambda ()
            (adaptive-wrap-prefix-mode +1)))
(diminish 'visual-line-mode)
(global-visual-line-mode +1)

;;; Electric pair mode
(when (fboundp 'electric-pair-mode)
  (setq-default electric-pair-mode 1))

;;; Auto complete
(global-auto-complete-mode)
(setq ac-max-width 0.4)
(require 'auto-complete-config)

;;; Make the emacs window tidy and remove some startup warning messages
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq warning-suppress-types '((initialization)))
(setq delete-by-moving-to-trash t)

;;; Some basic preferences
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position t
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t)

;;; Global auto revert mode
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      max-mini-window-height 0.8)

;;; Don't show the trailing whitespace in some modes
(dolist (hook '(term-mode-hook comint-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(transient-mark-mode t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;; Zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")

;;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;; Show matching parens by activating mic-paren
(paren-activate)

;;; Fix per-window memory of buffer point positions
(global-pointback-mode)
(eval-after-load 'skeleton
  '(defadvice skeleton-insert (before disable-pointback activate)
     "Disable pointback when using skeleton functions like `sgml-tag'."
     (when pointback-mode
       (message "Disabling pointback.")
       (pointback-mode -1))))


;;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Some editing functions will be bind in init-keybind.el
(defun duplicate-line ()
  "Insert a copy of the current line after the current line."
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;;; Page break lines
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

;;; Fill column indicator
(require 'fill-column-indicator)
(when (eval-when-compile (> emacs-major-version 23))
  (defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  ;; (add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

  (defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sanityinc/fci-enabled-p)
          (turn-on-fci-mode))))))

;; ?│ (\u2502) or ?┃ (\u2503), thin or thick version.
(defun my-change-window-divider ()
  (dolist (win (window-list nil 'no-minibuffer))
    (with-current-buffer (window-buffer win)
      (with-selected-window win
        (unless buffer-display-table
          (setq buffer-display-table (make-display-table)))
        (set-display-table-slot buffer-display-table 'vertical-border ?┃)))))

(defvar use-fancy-divider nil)
(defun toggle-use-fancy-divider ()
  (interactive)
  (setq use-fancy-divider (not use-fancy-divider))
  (message "%s fancy devider" (if use-fancy-divider "Enable" "Disable"))
  (if use-fancy-divider
      (progn (setq fci-rule-character ?│)
             (add-hook 'window-configuration-change-hook 'my-change-window-divider))
    (setq fci-rule-character ?|)
    (remove-hook 'window-configuration-change-hook 'my-change-window-divider)))
(toggle-use-fancy-divider)

;; ?│ (\u2502) or ?┃ (\u2503), thin or thick version.
(setq fci-rule-color "#8b2252") ;; maroon4
(setq-default fill-column 80)

;; toggle fci-mode on specified modes
(defvar enable-fci-mode-on-hooks nil
  "Whether to enable fci mode on some specified hooks")
(defun toggle-fci-on-specified-modes ()
  (interactive)
  (setq enable-fci-mode-on-hooks
        (not enable-fci-mode-on-hooks))
  (message "%s fci mode" (if enable-fci-mode-on-hooks "Enable" "Disable"))
  (dolist (hook
           '(c++-mode-hook
             c-mode-hook
             python-mode-hook
	         cv-mode-hook
             js-mode-hook
             lisp-mode-hook
	         html-mode-hook
             sawfish-mode-hook
             tex-mode-hook
	         emacs-lisp-mode-hook
	         latex-mode-hook))
    (if enable-fci-mode-on-hooks
        (add-hook hook 'turn-on-fci-mode)
      (remove-hook hook 'turn-on-fci-mode))))
(toggle-fci-on-specified-modes)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(move-text-default-bindings)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

;;; Cua Rectangle selections, and overwrite text when the selection is active
(cua-selection-mode t)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(when (executable-find "ag")
  (with-eval-after-load "ag"
    (when (equal
           (substring
            (shell-command-to-string
             "ag --version | head -1 | cut -d ' ' -f 3 | cut -d '.' -f 1")
            0 -1)
           "2")
      ;; limit the output to two lines.
      (add-to-list 'ag-arguments "-W 160"))))

(setq rainbow-html-colors t)

;;; Format buffer with astyle or clang format
(defun astyle-this-buffer(pmin pmax)
  (interactive "r")
  (until (executable-find "astyle")
         (error "astyle is not installed"))
  (shell-command-on-region
   pmin pmax
   "astyle --style=stroustrup"
   (current-buffer)
   t
   (get-buffer-create "*Astyle Errors*") t))

(setq-default clang-format-style
              "{BasedOnStyle: Google, IndentWidth: 4}")

;;; epa
(require 'epa-file)
;; To make it ask the passphase in the minibuffer
(custom-set-variables '(epa-pinentry-mode 'loopback))

;;; dash-at-point
(require 'dash-at-point)
(dolist (var '((kotlin-mode . "kotlin")
               (racket-mode . "racket")
               (cmake-mode . "cmake")
               (gradle-build-mode . "gradle")
               (go-mode . "go")))
  (add-to-list 'dash-at-point-mode-alist var))

;;; sudo edit current buffer
(defun my-sudo-edit ()
  "Edit the file that is associated with current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

;;; auto indent for some modes
(defconst auto-indent-modes-list
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
  "Modes that I'd like to enable auto-indent")

;;; auto indent when paste some code.
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode auto-indent-modes-list)
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;;; match paren with %
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

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

;;; doc-view-mode-maybe is boring, here i delete doc associatiation with it.
(delete (rassoc 'doc-view-mode-maybe auto-mode-alist) auto-mode-alist)

;;; if no region selected, comment/uncomment current line
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

;;; No ring bell
(setq visible-bell t
      ring-bell-function (lambda () (message "Try to ring bell")))

;;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around set-null-device activate)
  "Solve two problems:
null-device: Use cygwin's /dev/null as the null-device
grep-use-null-filename-separator: the old zgrep doesn't support the --null option"
  (let ((null-device "/dev/null")
        (grep-use-null-filename-separator nil))
    ad-do-it))

;;; Add some keyword to be highlight
(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIXME\\|MODIFIED\\|DEBUG\\|UPDATED\\|TODO\\):"
       1 'font-lock-warning-face prepend)))))

;;; Share clipboard to remote host
(cu-with-parameters-bounded
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

;;; Pretty print xml region
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

;;; wgrep setting
(setq wgrep-change-readonly-file t)

;;; rainbow-delimiters
(defun turn-on-rainbow-delimiters-mode ()
  (rainbow-delimiters-mode 1))

(dolist (hook
         '(emacs-lisp-mode
           racket-mode
           c-mode-common-hook))
  (add-hook hook 'turn-on-rainbow-delimiters-mode))

;;; Nicer naming of buffers for files with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; Spell check
;; 1. brew install hunspell
;; 2. Copy the en_US.aff and en_US.dic to ~/Library/Setting
(when (equal os 'darwin)
  (let ((possible-spell-program (cu-search-brew-executable "hunspell")))
    (when possible-spell-program
      (setenv "DICTIONARY" "en_US")
      (setq ispell-program-name possible-spell-program
            ispell-dictionary "en_US")
      (defconst my-ispell-is-enabled t "Whether ispell is enabled"))))

(when (and (equal os 'linux) (executable-find "hunspell"))
  (setenv "DICTIONARY" "en_US")
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US")
  (defconst my-ispell-is-enabled t "Whether ispell is enabled"))

;;; Show the kill ring
(require 'helm-ring)
(defadvice yank-pop (around show-key-ring () activate)
  (if (equal last-command 'yank)
      ad-do-it
    (helm-show-kill-ring)))

(provide 'init-editing-utils)
