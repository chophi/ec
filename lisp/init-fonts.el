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

(defun set-font-for-current-frame (font-list)
  "Set font for current frame"
  (dolist (fc (cdr font-list))
    (let* ((script (car fc))
           (font (font-spec :family (cadr fc) :size (caddr fc))))
      ;; FIXME: (workaround) From the doc, the set-fontset-font should work for
      ;; per-frame font setting, but seems it's buggly, but I found a workaround
      ;; for this, use set-face-attribute for the script `ascii`,
      ;; but use set-fontset-font to set "fontset-default" for `han` instead.
      (if (eq script 'ascii)
          (set-face-attribute 'default (selected-frame) :font font))
      (when (fboundp 'set-fontset-font)
        (set-fontset-font "fontset-default"
                          (car fc)
                          (font-spec :family (cadr fc) :size (caddr fc))
                          (selected-frame))))))

;; 中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文中文
;; llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
;; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
;; useful variables and functions:
;; charset-script-alist charset-list =C-u + (what-cursor-position)=
;; Test here for macos non-company:
;; (set-font-for-current-frame '("test" (han "STFangsong" 24) (ascii "Monaco" 20)))
;; (set-face-attribute 'default (selected-frame) :font (font-spec :family "Monaco" :registry "gb2312" :size 10))
(defconst preferred-font-config-list
  (cond
   ;; for non-company macos
   ((eq os 'macos)
    (if (company-computer-p)
        '(("large(23-inch)" (han "STFangsong" 30) (ascii "Monaco for Powerline" 25)) ;; 23-inch display
          ("small(13.3-inch)" (han "STFangsong" 24) (ascii "Monaco for Powerline" 20))) ;; 13.3-inch display
        '(("large" (han "STFangsong" 22) (ascii "Monaco for Powerline" 19))
          ("small(13.3-inch)" (han "STFangsong" 24) (ascii "Monaco for Powerline" 20)))))
   ;; for company computer
   ((and (company-computer-p) (eq os 'linux))
    '(("c1" (han "SimSun" 30) (ascii "Monaco for Powerline" 25))
      ("c2" (han "SimSun" 30) (ascii "Ubuntu Mono" 29))
      ("c3" (han "SimSun" 30) (ascii "Consolas" 28))))
   ;; for windows
   ((eq os 'windows)
    '(("c1" (han "SimSun" 15.0) (ascii "Consolas" 14.5))))
   ;; default
   (t
    '(("c1" (han "STFangsong" 22) (ascii "Monaco" 19))
      ("c2" (han "STFangsong" 18) (ascii "Monaco" 15)))))
  "The preferred font config list which can be rotated use `next-font'")

(defvar selected-font-index -1
  "The index for selected font in `preferred-font-config-list'")

(defvar selected-font-config nil
  "The selected font config in `preferred-font-config-list'")

(defun next-font (&optional inc)
  (interactive)
  "Select the next font in `preferred-font-config-list'"
  (let* ((len (length preferred-font-config-list))
         (index (mod (+ (or inc 1) selected-font-index) len))
         (fconf-list (nth index preferred-font-config-list)))
    (setq selected-font-config fconf-list)
    (set-font-for-current-frame selected-font-config)
    (setq-default line-spacing 0.05)
    (setq selected-font-index index)))

;; The first call to use the first font in preferred-font-config-list
(next-font)

(defun refresh-font-setting ()
  (interactive)
  (set-font-for-current-frame selected-font-config))

(defun my-select-font ()
  (interactive)
  (let* ((prompt
         (seq-reduce
          (lambda (a b) (format "%s%s\n" a b))
          (mapcar
           (lambda (fc)
             (format "%s: [%s]" (car fc) (prin1-to-string (cdr fc))))
           preferred-font-config-list)
          ""))
        (choices
         (mapcar 'car preferred-font-config-list))
        (choice
         (ido-completing-read (format "Choose a fontset:\n%sPlease select: " prompt) choices)))
    (set-font-for-current-frame (assoc choice preferred-font-config-list))))

(when window-system
  (defun other-frame-post-advice ()
    (interactive)
    (let* ((cur-font (split-string (frame-parameter nil 'font) "-"))
           (font-name (nth 2 cur-font))
           (font-size (string-to-number (nth 7 cur-font)))
           (cur-font-set
            (catch 'return
              (dolist (f preferred-font-config-list nil)
                (let* ((ascii-font (assoc 'ascii f))
                       (sfont-size (nth 2 ascii-font))
                       (sfont-name (nth 1 ascii-font)))
                  (when (and (equal font-name sfont-name) (equal sfont-size font-size))
                    (throw 'return f)))))))
      (when cur-font-set
        (set-font-for-current-frame cur-font-set))))

  (defadvice other-frame (around font-resize)
    ad-do-it
    (other-frame-post-advice))

  (ad-activate 'other-frame))

(provide 'init-fonts)
