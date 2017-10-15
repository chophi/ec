(require 'cl)

(defun sanityinc/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun sanityinc/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (sanityinc/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun sanityinc/increase-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height 10))

(defun sanityinc/decrease-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'sanityinc/increase-default-font-height)
(global-set-key (kbd "C-M--") 'sanityinc/decrease-default-font-height)

(defun set-ascii-font (type size &optional weight)
  (if weight
      (set-face-attribute
       'default nil :font (font-spec
                           :family type
                           :size size
                           :weight weight))
    (set-face-attribute
     'default nil :font (font-spec
                         :family type
                         :size size))))

(defun set-cjk-font (type size &optional weight)
  (if weight
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t
                          charset
                          (font-spec
                           :family type
                           :size size
                           :weight weight)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t
                        charset
                        (font-spec
                         :family type
                         :size size)))))

(defun set-ascii-font-size (size)
  (set-face-attribute
   'default nil :font (font-spec
                       :size size)))

(defun set-cjk-font-size (size)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t
		      charset
		      (font-spec
		       :size size)
		      )))

(defun set-font-pair(ascii-type ascii-size cjk-type cjk-size)
   (set-ascii-font ascii-type ascii-size)
   (set-cjk-font cjk-type cjk-size)
   )

;; 中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文
;; llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
;; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
;; (when (eq window-system 'w32)
;;     (set-cjk-font "SimSun" 18.4) 
;;   (set-ascii-font "Monaco" 15.0))


(when (eq window-system 'w32)
    (set-cjk-font "SimSun" 15.0) 
  (set-ascii-font "Consolas" 14.5))

(when (and *amazon-machine?*
	   (window-system))
  (set-cjk-font "SimSun" 16.3)
  (defvar *current-font-index* -1)
  (defvar font-config-list
    '(("Monaco" . 14.5)
      ("Ubuntu Mono" . 16.5)
      ("Consolas" . 15.0)))
  (defun next-font ()
    (interactive)
    (let* ((len (length font-config-list))
           (index (mod (1+ *current-font-index*) len))
           (font-config (nth index font-config-list))
           (font-name (car font-config))
           (font-size (cdr font-config)))
      (set-ascii-font font-name font-size)
      (setq-default line-spacing 0.05)
      (setq *current-font-index* index)))
  (next-font))

;;; DONE: cjk-font working now
(when *mac?*
  (setq *toggle-font-size* nil)
  (defun toggle-font-size ()
    (interactive)
    (if *toggle-font-size*
        (progn (set-cjk-font "STFangsong" 18)
               (set-ascii-font "Monaco" 15)
               (setq *toggle-font-size* nil))
      (progn (set-cjk-font "STFangsong" 22)
             (set-ascii-font "Monaco" 19)
             (setq *toggle-font-size* t))))
  (toggle-font-size))

(defun zoom-frame (&optional n frame amt)
  "Increase the default size of text by AMT inside FRAME N times.
  N can be given as a prefix arg.
  AMT will default to 10.
  FRAME will default the selected frame."
  (interactive "p")
  (let ((frame (or frame (selected-frame)))
        (height (+ (face-attribute 'default :height frame) (* n (or amt 10)))))
    (set-face-attribute 'default frame :height height)
    (when (called-interactively-p)
      (message "Set frame's default text height to %d." height))))

(defun zoom-frame-out (&optional n frame amt)
  "Call `zoom-frame' with -N."
  (interactive "p")
  (zoom-frame (- n) frame amt))


(global-set-key (kbd "C-c z i") 'zoom-frame)
(global-set-key (kbd "C-c z o") 'zoom-frame-out)

(defun set-perferred-large-screen-fontsize ()
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 240))

(provide 'init-fonts)
