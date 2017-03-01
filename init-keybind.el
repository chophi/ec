(global-unset-key (kbd "\C-z"))
(global-unset-key (kbd "C-SPC"))

(global-set-key (kbd "C-x vq") 'vr/query-replace)
(global-set-key (kbd "C-x vr") 'vr/replace)
(global-set-key (kbd "\C-xo") 'switch-window)
(provide 'init-keybind)
