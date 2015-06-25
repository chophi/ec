(setq window-system-default-frame-alist
      `((x
         (width . ,(frame-parameter nil 'width))
         (height . ,(- (frame-parameter nil 'height) 2))
         (font . ,(frame-parameter nil 'font))
         (top . ,(frame-parameter nil 'top))
         (left . ,(frame-parameter nil 'left))
         ;; (background-color . "#2E3436")
         )))

(provide 'init-system-default-frame-alist)
