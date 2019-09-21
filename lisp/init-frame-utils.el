(require 'array)

(defun my-set-frame-name ()
  (interactive)
  (set-frame-name (read-string "Frame name: ")))

(defun my-select-frame ()
  (interactive)
  (let* ((current-window-id (frame-parameter nil 'window-id))
         (frame-names-alist (make-frame-names-alist)))
    (cl-labels ((make-frame-names-alist
                 nil
                 (seq-filter
                  (lambda (f)
                    (and (not (equal (car f) default-terminal-frame-name))
                         (not (xor current-window-id (frame-parameter (cdr f) 'window-id)))))
                  frame-names-alist)))
      (let ((choice-list (mapcar (lambda (f) (car f)) (make-frame-names-alist))))
        (case (length choice-list)
          ((0 1)
           (if (and (= (length choice-list) 1) (equal (frame-parameter nil 'name) default-terminal-frame-name))
               (select-frame-by-name (car choice-list))
             (select-frame-by-name default-terminal-frame-name)))
          (2
           (select-frame-by-name
            (if (equal (car choice-list) (frame-parameter nil 'name))
                (cadr choice-list)
              (car choice-list))))
          (t
           (call-interactively 'select-frame-by-name)))))))

(defun my-make-frame ()
  (interactive)
  (let ((name (read-string "New Frame name: ")))
    (set-frame-parameter (make-frame-command) 'name name)))

(defun _select-frame (frame)
  (select-frame-set-input-focus frame))

(defun my-next-frame ()
  (interactive)
  (_select-frame (next-frame)))

(defun my-previous-frame ()
  (interactive)
  (_select-frame (previous-frame)))

(defun my-delete-other-frames ()
  (interactive)
  (and (y-or-n-p "Delete all other frames?") (delete-other-frames)))

(defconst default-terminal-frame-name "terminal"
  "The default name for terminal frame")

(defun my-set-term-frame ()
  (interactive)
  (and (y-or-n-p "Select current frame as terminal frame?")
       (set-frame-name default-terminal-frame-name)))

(defun my-switch-to-terminal-frame ()
  (interactive)
  (select-frame-by-name default-terminal-frame-name))

(defun _get_frame_rect_lrtd (&optional frame)
  (let ((outer-position (cdar (frame-geometry frame)))
        (outer-size (cdadr (frame-geometry frame))))
    (list (car outer-position)
          (+ (car outer-position) (car outer-size))
          (cdr outer-position)
          (+ (cdr outer-position) (cdr outer-size)))))

(defun _center-point-angle (&optional frame)
  (let ((lrtd (_get_frame_rect_lrtd frame)))
    (atan (- (/ (+ (nth 2 lrtd) (nth 3 lrtd)) 2.0))
          (/ (+ (nth 0 lrtd) (nth 1 lrtd)) 2.0))))

(defun my-switch-to-the-screen (pred)
  (let ((tmp-frame-list
         (copy-list (frame-list))))
    (_select-frame (car (sort tmp-frame-list pred)))))

(defun my-switch-to-topmost-screen ()
  (interactive)
  (my-switch-to-the-screen
   (lambda (a b)
     (< (nth 2 (_get_frame_rect_lrtd a))
        (nth 2 (_get_frame_rect_lrtd b))))))

(defun my-switch-to-downmost-screen ()
  (interactive)
  (my-switch-to-the-screen
   (lambda (a b)
     (> (nth 3 (_get_frame_rect_lrtd a))
        (nth 3 (_get_frame_rect_lrtd b))))))

(defun my-switch-to-leftmost-screen ()
  (interactive)
  (my-switch-to-the-screen
   (lambda (a b)
     (< (nth 0 (_get_frame_rect_lrtd a))
        (nth 0 (_get_frame_rect_lrtd b))))))

(defun my-switch-to-rightmost-screen ()
  (interactive)
  (my-switch-to-the-screen
   (lambda (a b)
     (> (nth 1 (_get_frame_rect_lrtd a))
        (nth 1 (_get_frame_rect_lrtd b))))))

(defun _my-switch-screen-clockwise (next)
  (let* ((tmp-frame-list
          (copy-list (frame-list)))
         (nframes (length tmp-frame-list))
         (indexOfCurrent 0)
         (index 0))
    (setq tmp-frame-list
          (sort tmp-frame-list
                (lambda (a b)
                  (> (_center-point-angle a)
                     (_center-point-angle b)))))
    (dolist (f tmp-frame-list)
      (when (equal f (selected-frame))
        (setq indexOfCurrent index))
      (setq index (1+ index)))
    (_select-frame
     (nth
      (% (+ next indexOfCurrent nframes) nframes)
      tmp-frame-list))))

(defun my-switch-to-next-screen-clockwise ()
  (interactive)
  (_my-switch-screen-clockwise 1))

(defun my-switch-to-next-screen-anticlockwise ()
  (interactive)
  (_my-switch-screen-clockwise -1))

(defun my-switch-screen ()
  (interactive)
  (if window-system
      (call-interactively
       (cu-make-commands-map-with-help-msg
        '((?l . my-switch-to-leftmost-screen)
          (?r . my-switch-to-rightmost-screen)
          (?t . my-switch-to-topmost-screen)
          (?d . my-switch-to-downmost-screen)
          (?n . my-switch-to-next-screen-clockwise)
          (?p . my-switch-to-next-screen-anticlockwise)
          (?s . my-select-frame))))
    (my-select-frame)))

(defun my-make-switch-frame-commands ()
  (interactive)
  (call-interactively
   (cu-make-commands-map-with-help-msg
    `((?c . my-make-frame)
      (?d . delete-frame)
      (?w . my-delete-other-frames)
      (?r . my-set-frame-name)
      ,@(when (fboundp 'control-x-f)
          `((?f . control-x-f)))
      (?s . my-select-frame)
      (?t . my-set-term-frame)
      ,@(if (not window-system)
            `((?n . my-next-frame)
              (?p . my-previous-frame))
          `((?n . my-switch-to-next-screen-clockwise)
            (?p . my-switch-to-next-screen-anticlockwise)))))))

(defun my-set-frame-name ()
  (interactive)
  (let ((name (read-string (concat "Frame name[" frame-default-name "]: "))))
    (when (or (not name) (equal name ""))
      (setq name frame-default-name))
    (set-frame-parameter (selected-frame) 'title name)))

(provide 'init-frame-utils)
