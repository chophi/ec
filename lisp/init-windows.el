;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)


;; Make "C-x o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(require 'switch-window)
(setq switch-window-shortcut-style 'alphabet)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))


;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(setq split-height-threshold nil
      split-width-threshold 160)

(defun split-window-horizontally-instead ()
  (interactive)
  (if (not (>= (frame-width) split-width-threshold))
      (progn
        (message "Window is too narrow, split window vertical instead")
        (split-window-vertically-instead))
    (save-excursion
      (delete-other-windows)
      (funcall
       (split-window-func-with-other-buffer 'split-window-horizontally)))))

(defconst alpha-list '((85 50) (100 100)))
(defun window-cycle-alpha-parameter ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha (car alpha-list))
  (setq alpha-list (list (cadr alpha-list) (car alpha-list)))
  )

(global-set-key [(f12)] 'window-cycle-alpha-parameter)

(global-set-key "\C-x2" 'split-window-horizontally-instead)
(global-set-key "\C-x3" 'split-window-vertically-instead)

;; enable mouse reporting for terminal emulators
(defun enable-xterm-mouse-scroll ()
  (interactive)
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1))))
(unless window-system
  (enable-xterm-mouse-scroll))

(setq focus-follows-mouse t)

(defvar my-windows-mode nil)
(with-eval-after-load "treemacs"
  (require 'array)
  (defun my-delete-treemacs-window ()
    (interactive)
    (when (eq 'visible (treemacs-current-visibility))
      (delete-window (treemacs-get-local-window))))

  (defun my-only-one-window ()
    (interactive)
    (delete-other-windows)
    (my-delete-treemacs-window))

  (defun split-treemacs-with-other-window ()
    (interactive)
    (let ((curbuf (current-buffer)))
      (my-only-one-window)
      (treemacs)
      (setq my-windows-mode 'treemacs-with-other-one)
      (switch-to-buffer curbuf)))

  (defun split-treemacs-with-other-two-windows ()
    (interactive)
    (let ((curbuf (current-buffer)))
      (my-only-one-window)
      (treemacs)
      (setq my-windows-mode 'treemacs-with-other-two)
      (switch-to-buffer curbuf)
      (split-window-vertically-instead)
      (switch-to-buffer curbuf)))

  (defun select-treemacs-from-split-windows ()
    (interactive)
    (pcase my-windows-mode
      ((or 'treemacs-with-other-one 'treemacs-with-other-two)
       (select-window (treemacs-get-local-window)))))

  (defun select-first-window-with-pred (pred)
    (let ((wins (copy-list (window-list))))
      (select-window
       (car (sort wins pred)))))

  (defun select-right-up-window-from-split-windows ()
    (interactive)
    (select-first-window-with-pred
     (lambda (w0 w1)
       (let ((pos1 (window-edges w0))
             (pos2 (window-edges w1)))
         (or (> (nth 2 pos1) (nth 2 pos2))
             (< (nth 1 pos1) (nth 1 pos2)))))))

  (defun select-right-down-window-from-split-windows ()
    (interactive)
    (select-first-window-with-pred
     (lambda (w0 w1)
       (let ((pos1 (window-edges w0))
             (pos2 (window-edges w1)))
         (or (> (nth 2 pos1) (nth 2 pos2))
             (> (nth 1 pos1) (nth 1 pos2)))))))

  (add-hook 'treemacs-mode-hook
            (lambda ()
              (toggle-truncate-lines 1)
              ;; (with-current-buffer (treemacs-get-local-buffer)
              ;;   (face-remap-add-relative 'default '(:family "Consolas")))
              ) t))

(provide 'init-windows)
