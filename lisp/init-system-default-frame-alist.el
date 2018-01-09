(setq frame-default-name "Emacs::Console")
(defun my-set-frame-name ()
  (interactive)
  (let ((name (read-string (concat "Frame name[" frame-default-name "]: "))))
    (when (or (not name) (equal name ""))
      (setq name frame-default-name))
    (set-frame-parameter (selected-frame) 'title name)))

(setq default-frame-alist
      (append default-frame-alist
              `((width .  ,(frame-parameter nil 'width))
                (height . ,(- (frame-parameter nil 'height) 2))
                (font . ,(frame-parameter nil 'font))
                (top . ,(frame-parameter nil 'top))
                (left . ,(frame-parameter nil 'left))
                (background-color . ,(if global-background-color
				                         global-background-color
				                       (frame-parameter nil 'background-color)))
                (foreground-color . ,(if global-foreground-color
				                         global-foreground-color
				                       (frame-parameter nil 'foreground-color))))))

(add-to-list 'initial-frame-alist
	         `(background-color . ,(if global-background-color
				                       global-background-color
				                     (frame-parameter nil 'background-color))))

(provide 'init-system-default-frame-alist)

