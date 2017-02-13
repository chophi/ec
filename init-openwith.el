(require-package 'openwith)
(openwith-mode)

(when *is-linux-system-p*
  (setq openwith-associations
        '(("\\.pdf$" "evince" (file))
          ("\\.mp3$" "mplayer" (file) )
          ("\\.mov\\|\\.RM$\\|\\.RMVB$\\|\\.avi$\\|\\.AVI$\\|\\.flv$\\|\\.mp4\\|\\.mkv$\\|\\.rmvb$" "mplayer" (file) )
          ("\\.CHM$\\|\\.chm$" "chmsee"  (file) ))))

(when *is-windows-system-p*
  (let ((extra-openwith-associations
         '(("\\.docx$" "explorer" (file))
           ("\\.doc$" "explorer" (file))
           ("\\.vsd$" "explorer" (file))
           ("\\.vsdx$" "explorer" (file))
           ("\\.kdh$" "CAJVieweru" (file))
           ("\\.caj$" "CAJVieweru" (file))
           ))
        (extra-path-lists
         '("D:\\Program Files (x86)\\TTKN\\CAJViewer 7.2\\")))
    (dolist (p extra-path-lists)
      (add-to-list 'exec-path p))
    (dolist (p extra-openwith-associations)
      (add-to-list 'openwith-associations p))))

(when *is-mac-machine*
  (setq openwith-associations
        '(("\\.pdf$\\|\\.png$\\|\\.jpeg$\\|\\.jpg$" "open" (file)))))

(provide 'init-openwith)
