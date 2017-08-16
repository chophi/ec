(require-package 'maxframe)

(autoload 'mf-max-display-pixel-width "maxframe" "" nil)
(autoload 'mf-max-display-pixel-height "maxframe" "" nil)
(autoload 'maximize-frame "maxframe" "" t)
(autoload 'restore-frame "maxframe" "" t)


(when *linux?*
  (defvar *wmctrl-exist-p* (not (equal (shell-command-to-string "which wmctrl") "")))
  (defvar *xwininfo-exist-p* (not (equal (shell-command-to-string "which xwininfo") "")))

  (defun x-maximize-frame-maybe(window-name)
    (interactive)
    (when (and *wmctrl-exist-p* *xwininfo-exist-p*)
      (let ((winid (shell-command-to-string (format "xwininfo -name %s | grep 'Window id:' | cut -d\" \" -f4" window-name))))
	(when (and (stringp winid)
		   (> (length winid) 1))
	  (setq winid (substring winid 0 (1- (length winid))))
	  (message winid)
	  (shell-command (format "wmctrl -i -r \"%s\" -b add,maximized_vert,maximized_horz " winid))))))

  (eval-after-load 'maxframe
    '(progn
       (fset 'maximize-frame 'x-maximize-frame-maybe))))

(defvar sanityinc/prev-frame nil "The selected frame before invoking `make-frame-command'.")
(defadvice make-frame-command (before sanityinc/note-previous-frame activate)
  "Record the selected frame before creating a new one interactively."
  (setq sanityinc/prev-frame (selected-frame)))

(defun sanityinc/maybe-maximize-frame (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (and window-system
               sanityinc/prev-frame
               (sanityinc/maximized-p sanityinc/prev-frame))
      (maximize-frame))))

(when (not *mac?*)
  (add-hook 'after-make-frame-functions 'sanityinc/maybe-maximize-frame)
  (add-hook 'after-init-hook 'sanityinc/maybe-maximize-frame))

(defun within-p (a b delta)
  (<= (abs (- b a)) delta))

(defun sanityinc/maximized-p (&optional frame)
  (or (not (with-selected-frame (or frame (selected-frame)) window-system))
      (and (within-p (mf-max-display-pixel-width)
                     (frame-pixel-width frame)
                     (frame-char-width frame))
           (within-p (mf-max-display-pixel-height)
                     (+ mf-display-padding-height (frame-pixel-height frame))
                     (frame-char-height frame)))))


(provide 'init-maxframe)
