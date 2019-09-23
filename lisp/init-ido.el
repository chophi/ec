;; Use C-f during file selection to switch to regular find-file
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)

(require 'smex)
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(require 'ido-completing-read+)
(ido-ubiquitous-mode t)
(ido-mode t)
(ido-everywhere t)
(global-set-key (kbd "M-x") 'smex)
(counsel-mode -1)
(ivy-mode -1)

(provide 'init-ido)
