;; org-mode
(require 'org)
(require 'org-install)

(require 'ox-latex)
(require 'org-indent)

;;; Get easy template back
(require 'org-tempo)

;; Various preferences
(setq org-log-done t
      org-src-fontify-natively t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-tags-column 80
      org-export-in-background t
      org-confirm-babel-evaluate nil
      org-startup-indented t)

(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode -1)
            (visual-line-mode 1))
          t)

(add-hook 'org-src-mode-hook
          (lambda ()
            (when (equal major-mode 'go-mode)
              (flycheck-mode -1))))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Refile targets include this file and any file contributing to the agenda -
;; up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5)))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path 'file)
;; Targets complete in steps so we start with filename,
;; TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(S)"
                  "PROJECT(P@)" "|" "CANCELLED(c@/!)")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs,
;; load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("⚇" "⚉"))

(define-derived-mode org-tbl-mode org-mode "ORG-TBL")
(add-to-list 'auto-mode-alist '("\\.tbl.org" . org-tbl-mode))
(add-hook 'org-tbl-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)) t)

;; Add gerrit map in org-mode
(setq org-mode-gerrit-map (make-keymap))
(define-key org-mode-map (kbd "C-c g") org-mode-gerrit-map)
(define-key org-mode-gerrit-map "v" 'cu-show-gerrit-at-point)
(with-eval-after-load "fci-mode"
  (add-hook org-mode-hook (lambda () (fci-mode -1))))
(provide 'init-org)
