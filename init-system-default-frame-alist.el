(setq window-system-default-frame-alist
      '(`(x
         (width . ,(frame-parameter nil 'width))
         (height . ,(- (frame-parameter nil 'height) 2))
         (font . ,(frame-parameter nil 'font))
         (top . ,(frame-parameter nil 'top))
         (left . ,(frame-parameter nil 'left))
         (background-color . "#2E3436")
         )
        (ns
         (width . 138)
         (height . 38)
         (top . 23)
         (left . 0)
         (font . "-*-Menlo-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
         (background-color . "#2E3436"))))

(add-to-list 'initial-frame-alist '(background-color . "#2E3436"))

(provide 'init-system-default-frame-alist)
