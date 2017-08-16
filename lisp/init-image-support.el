;; image-type-available-p check whether image type supported.
(defun check-image-support()
  (interactive)
  (dolist (image-type '(png jpeg tiff gif xpm svg))
    (message (concat (prin1-to-string image-type)
                     " "
                     (if (image-type-available-p image-type)
                         "supported"
                       "not supported")))))

;; image-library-alist tell which dlls need to support type


(provide 'init-image-support)
