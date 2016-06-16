(setq frame-default-name "Emacs::Console")
(defun my-set-frame-name ()
  (interactive)
  (let ((name (read-string (concat "Frame name[" frame-default-name "]: "))))
    (when (or (not name) (equal name ""))
      (setq name frame-default-name))
    (set-frame-parameter (selected-frame) 'title name)))

(defun re-evaluate-default-window-alist ()
  (setq window-system-default-frame-alist
        `((x
           (width .  ,(frame-parameter nil 'width))
           (height . ,(- (frame-parameter nil 'height) 2))
           (font . ,(frame-parameter nil 'font))
           (top . ,(frame-parameter nil 'top))
           (left . ,(frame-parameter nil 'left))
           (background-color . ,global-background-color)
           )
          (ns
           (width . 139)
           (height . 34)
           ;; (font . "-*-Monaco-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
           (top . 23)
           (left . 0)
           (background-color . ,global-background-color)))))

(re-evaluate-default-window-alist)

(add-to-list 'initial-frame-alist `(background-color . ,global-background-color))

(provide 'init-system-default-frame-alist)

