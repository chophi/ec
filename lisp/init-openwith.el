(require-package 'openwith)
(openwith-mode)

(setq
 openwith-associations
 (case os
   ('linux
    '(("\\.pdf$" "evince" (file))
      ("\\.mp3$" "mplayer" (file) )
      ("\\.mov\\|\\.RM$\\|\\.RMVB$\\|\\.avi$\\|\\.AVI$\\|\\.flv$\\|\\.mp4\\|\\.mkv$\\|\\.rmvb$" "mplayer" (file) )
      ("\\.CHM$\\|\\.chm$" "chmsee"  (file))))
   ('macos
    '(("\\.pdf$\\|\\.png$\\|\\.jpeg$\\|\\.jpg$" "open" (file))))))

(provide 'init-openwith)
