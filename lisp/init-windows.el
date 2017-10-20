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

(provide 'init-windows)
