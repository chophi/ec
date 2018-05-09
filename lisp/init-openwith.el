(require-package 'openwith)
(openwith-mode)

(setq
 openwith-associations
 (case os
   ('linux
    '(("\\.pdf$" "evince" (file))
      ("\\.mp3$" "mplayer" (file) )
      ("\\.mov\\|\\.RM$\\|\\.RMVB$\\|\\.avi$\\|\\.AVI$\\|\\.flv$\\|\\.mp4\\|\\.mkv$\\|\\.rmvb$" "mplayer" (file) )
      ("\\.CHM$\\|\\.chm$" "chmsee"  (file))
      ("\\.\\(?:jp?g\\|png\\)\\'" "remote-show-image"
       (file))))
   ('macos
    '(("\\.pdf$" "open" (file))))))

(provide 'init-openwith)
