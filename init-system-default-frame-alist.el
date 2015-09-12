(setq window-system-default-frame-alist
<<<<<<< HEAD
      `((x
          (width .  ,(frame-parameter nil 'width))
          (height . ,(- (frame-parameter nil 'height) 2))
=======
      '(`(x
          (width .  ,(frame-parameter nil 'width))
          (height . ,(- (frame-paramter nil 'height) 2))
>>>>>>> a33e64ec347f698129fe25c92eaf2349d15b33b8
          (font . ,(frame-parameter nil 'font))
          (top . ,(frame-parameter nil 'top))
          (left . ,(frame-parameter nil 'left))
          (background-color . "#2E3436")
          )
        (ns
         (width . 139)
         (height . 34)
         (font . "-*-Monaco-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
         (top . 23)
         (left . 0)
         (background-color . "#2E3436"))))

(add-to-list 'initial-frame-alist '(background-color . "#2E3436"))

(provide 'init-system-default-frame-alist)
