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

;; (defun set-ascii-font (type size)
;;   (set-face-attribute
;;    'default nil :font (concat type " " (number-to-string size))))

(defun set-ascii-font (type size)
  (set-face-attribute
   'default nil :font (font-spec
                       :family type
                       :size size)))

(defun set-cjk-font (type size)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t
		      charset
		      (font-spec
		       :family type
		       :size size)
		      )))

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

(when (and *is-amazon-machine*
	   (window-system))
  (set-cjk-font "SimSun" 16.3)
  (set-ascii-font "Ubuntu Mono" 16.0))

;;; DONE: cjk-font working now
(when *is-mac-machine*
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

;; (when (eq window-system 'w32)
;;   (set-cjk-font "SimSun" 14.3) 
;;   (set-ascii-font "Monaco" 12.0))

(provide 'init-fonts)
