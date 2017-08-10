(defun cu-is-dir-or-dirlink? (path)
  (and (file-exists-p path)
       (let ((first-attr (car (file-attributes path))))
         (or (eq first-attr t)
             (and (stringp first-attr) (cu-is-dir-or-dirlink? first-attr))))))

(defun cu-trash-files(file-list &optional delete-p)
  "if delete-p set, then delete files, or trash files"
  (dolist (file-name file-list)
    (if (file-exists-p file-name)
        (if delete-p
            (progn
              (shell-command (format "rm %s" file-name)))
          (progn
            (move-file-to-trash file-name)))
      (warn "file doesn't exist: %s" file-name))))

(defun cu-trash-file(file &optional delete-p)
  "if delete-p set, then delete file, or trash file"
  (cu-trash-files `(,file) delete-p))

(defun cu-kill-cur-path ()
  "kill current buffer file name to ring"
  (interactive)
  (if (buffer-file-name)
      (kill-new (buffer-file-name))
    (kill-new default-directory)))

(defun cu-shell-command-output (command)
  (let ((str (shell-command-to-string command)))
    (when (and (stringp str) (> (length str) 0))
      (substring str 0 (1- (length str)))
      )))

(defun cu-no-comment-content (buf comment-prefix)
  (let (str-list
        (content ""))
    (when (not buf)
      (setq buf (current-buffer)))
    (setq str-list
          (with-current-buffer buf
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
    (dolist (str str-list content)
      (when (or (< (length str) (length comment-prefix))
                (not (equal comment-prefix
                            (substring str 0 (length comment-prefix))))) 
        (setq content (concat content "\n" str))))
    content))

(defun cu-extract-list(buf comment-prefix regex part)
  (let ((pos 1)
        (result '())
        (str (cu-no-comment-content buf comment-prefix)))
    (while (and (< pos (point-max))
                (string-match regex str pos))
      (add-to-list 'result (match-string part str))
      (setq pos (match-end part)))
    result))

(defun cu-gerrit-view (&optional refresh)
  (interactive "P")
  (let* ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (regex "https://\\(.*\\)/gerrit/.*?\\([0-9]+\\)")
         (link (progn (string-match regex str) (match-string 0 str)))
         (server (progn (string-match regex str) (match-string 1 str)))
         (change-id (progn (string-match regex str) (match-string 2 str)))
         (command (format "ssh -p 9418 %s gerrit query --current-patch-set %s --format json" server change-id))
         (buffer-name (format "*gerrit [%s]*" change-id)))
    (message "link: %s, server: %s, change-id: %s" link server change-id)
    (if (and (get-buffer buffer-name) (not refresh))
        nil
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert (shell-command-to-string command))
        (json-mode)
        (json-pretty-print-buffer)))
    (switch-to-buffer-other-window buffer-name)))

(provide 'init-common-utils)

