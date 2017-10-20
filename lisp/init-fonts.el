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

(defun set-font-for-charset (charset type size &optional weight)
  "Set font for the CHARSET
CHARSET can be 'ascii, 'cjk or a list of selected charset in `charset-list'"
  (if (eq charset 'ascii)
      (set-face-attribute 'default nil :font
                          (apply #'font-spec
                                 :family type
                                 :size size
                                 (when weight
                                   `(:weight ,weight))))
    (progn
      (when (not (listp charset))
        (setq charset
              (case charset
                ('cjk '(kana han symbol cjk-misc bopomofo))
                (t '(nil)))))
      (dolist (cset charset)
        (set-fontset-font t cset
                          (apply #'font-spec
                                 :family type
                                 :size size
                                 (when weight
                                   `(:weight ,weight))))))))

;; 中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文
;; llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
;; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
(when (eq os 'windows)
  (set-font-for-charset ) 
  (set-font-for-charset ))

(defvar preferred-font-config-list
  (cond
   ;; for company computer
   ((and (company-computer-p) (window-system))
    '(((cjk "SimSun" 16.3) (ascii "Monaco" 14.5))
      ((cjk "SimSun" 16.3) (ascii "Ubuntu Mono" 16.5))
      ((cjk "SimSun" 16.3) (ascii "Consolas" 15.0))))
   ;; for non-company macos
   ((eq os 'macos)
    '(((ascii "Monaco" 19) (cjk "STFangsong" 22))
      ((ascii "Monaco" 15) (cjk "STFangsong" 18))))
   ;; for windows
   ((eq os 'windows)
    '(((cjk "SimSun" 15.0) (ascii "Consolas" 14.5)))))
  "The preferred font config list which can be rotated use `next-font'")

(defvar selected-font-index -1
  "The index for selected font in `preferred-font-config-list'")

(defvar selected-font-config nil
  "The selected font config in `preferred-font-config-list'")

(defun next-font ()
  (interactive)
  "Select the next font in `preferred-font-config-list'"
  (let* ((len (length preferred-font-config-list))
         (index (mod (1+ selected-font-index) len))
         (fconf-list (nth index preferred-font-config-list)))
    (setq selected-font-config fconf-list)
    (dolist (fc fconf-list) (apply #'set-font-for-charset fc))
    (setq-default line-spacing 0.05)
    (setq selected-font-index index)))

;; The first call to use the first font in preferred-font-config-list
(next-font)

(defun set-perferred-large-screen-fontsize ()
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 240))

(provide 'init-fonts)
