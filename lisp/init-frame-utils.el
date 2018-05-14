(defun my-set-frame-name ()
  (interactive)
  (set-frame-name (read-string "Frame name: ")))

(require 'array)
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

(defun my-next-frame ()
  (interactive)
  (select-frame (next-frame)))

(defun my-previous-frame ()
  (interactive)
  (select-frame (previous-frame)))

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

(provide 'init-frame-utils)
