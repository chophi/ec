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

(defconst  split-at-n-column 83)

(when (or *is-amazon-linux-window* *is-mac-machine*)
  (setq split-at-n-column 87))

(defun split-right-at-n-column(arg)
  (interactive "P")
  (delete-other-windows)
  (let ((width split-at-n-column))
    (if arg
        (split-window-right (- width))
      (split-window-right width))))

(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall
     (split-window-func-with-other-buffer (lambda() (split-window-right split-at-n-column))))))

(defconst alpha-list '((85 50) (100 100)))
(defun window-cycle-alpha-parameter ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha (car alpha-list))
  (setq alpha-list (list (cadr alpha-list) (car alpha-list)))
  )

(global-set-key [(f12)] 'window-cycle-alpha-parameter)
(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer (lambda() (split-window-right split-at-n-column))))

(global-set-key "\C-x9" 'split-right-at-n-column)

;;; multi-window modes
(defun my-four-grid-windows ()
  (interactive)
  (execute-kbd-macro "\C-x1\C-x9\C-x2\C-xoc\C-x2\C-xoa"))
(defun my-three-left-windows ()
  (interactive)
  (execute-kbd-macro "\C-x1\C-x9\C-xo\C-x2\C-xoa"))
(defun my-three-right-windows ()
  (interactive)
  (execute-kbd-macro "\C-x1\C-x9\C-x2\C-xoa"))

(defun my-multi-windows ()
  (interactive)
  (let ((mode (ido-completing-read "W Mode: " '("four-grid" "three-left" "three-right"))))
    (when (> (length mode) 0)
      (let (command)
        (setq command (concat "my-" mode "-windows"))
        (when (functionp (intern command))
          (command-execute (intern command)))))))

(global-set-key "\C-x?" 'my-multi-windows)

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
(provide 'init-windows)
