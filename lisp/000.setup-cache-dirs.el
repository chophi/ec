(defconst user-emacs-cache-directory (expand-file-name "cache" user-emacs-directory)
  "cache directory to save kinds of caches")
;; create if not exist
(unless (file-exists-p user-emacs-cache-directory)
  (mkdir user-emacs-cache-directory))
;; set the directories to cache
(setq desktop-path (list user-emacs-cache-directory))
(setq session-save-file (expand-file-name "session" user-emacs-cache-directory))
(setq recentf-save-file (expand-file-name "recentf" user-emacs-cache-directory))
(setq lsp-session-file (expand-file-name "lsp-session-file" user-emacs-cache-directory))
;; transient
(setq transient-levels-file (expand-file-name "transient/levels.el" user-emacs-cache-directory))
(setq transient-values-file (expand-file-name "transient/values.el" user-emacs-cache-directory))
(setq transient-history-file (expand-file-name "transient/history.el" user-emacs-cache-directory))

;; auto complete data
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" user-emacs-cache-directory))
(setq ac-dictionary-files `(,(expand-file-name ".dict" user-emacs-cache-directory)))

;; ido
(setq ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-cache-directory))

;; projectile
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-cache-directory))
(setq projectile-cache-file (expand-file-name "projectile.cache" user-emacs-cache-directory))

;; smex
(setq smex-save-file (expand-file-name "smex-items" user-emacs-cache-directory))

;;; change backup and auto-save directory
(setq backup-directory-alist `(("[:ascii:]*" . ,(expand-file-name "backup" user-emacs-cache-directory))))
(let ((auto-save-file-directory (expand-file-name "auto-save-file" user-emacs-cache-directory)))
  (when (not (file-exists-p auto-save-file-directory))
    (make-directory auto-save-file-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(concat auto-save-file-directory "/\\2") t))))
;; auto-save-list
(setq auto-save-list-file-prefix (expand-file-name ".saves-" user-emacs-cache-directory))
(provide '000.setup-cache-dirs)
