(defvar grok-indexing-list '("~/git-repo/config"))
(defvar global-grok-index-mutex (make-mutex "global-grok-index-mutex"))
(defvar global-grok-log-mutex (make-mutex "global-grok-log-mutex"))
(defvar some-thread-is-creating-grok-index nil)

(setq-default thread-list nil)
(setq-default grok-current-dir nil)

(defun global-grok-indexing-buffer ()
  (get-buffer-create "*grok-indexing-buffer*"))
(defun grok-log (&rest args)
  (mutex-lock global-grok-log-mutex)
  (with-current-buffer (global-grok-indexing-buffer)
    (goto-char (point-max))
    (apply #'insert args))
  (mutex-unlock global-grok-log-mutex))

(defun _open-indexing-maybe ()
  (let ((is-indexing nil))
    (mutex-lock global-grok-index-mutex)
    (setq is-indexing some-thread-is-creating-grok-index)
    (when (not is-indexing)
        (setq some-thread-is-creating-grok-index t))
    (mutex-unlock global-grok-index-mutex)
    (not is-indexing)))

(defun _close-indexing ()
  (let ((is-indexing nil))
    (mutex-lock global-grok-index-mutex)
    (setq some-thread-is-creating-grok-index nil)
    (mutex-unlock global-grok-index-mutex)))

(defun _eopengrok--process-sentinel (process event)
  "Handle eopengrok PROCESS EVENT."
  (let* ((buf (process-buffer process))
         (project-dir (buffer-local-value 'grok-current-dir buf)))
    (grok-log (format "Process Buffer: %s\n Received event %s\n" buf event))
    (with-current-buffer buf
      (cond ((or (string= "killed\n" event) (string= "finished\n" event))
             (kill-buffer buf)
             (_close-indexing)
             (grok-log (format "Save timestamp for [%s]\n" project-dir))
             (write-grok-complete-timestamp project-dir))
            (t nil)))))

(defun thread-opengrok-create-index (dir)
  (let* ((cur-thread-name (thread-name (current-thread)))
         (eopengrok-indexing-buffer
          (format "*eopengrok-indexing[%s]*" cur-thread-name)))
    (eopengrok-create-index dir nil '_eopengrok--process-sentinel)))

(defun project-timestamp-file (dir)
  (cu-join-path (expand-file-name dir) ".log" "last-index-timestamp"))

(defun write-grok-complete-timestamp (dir)
  (let* ((timestamp-file
          (cu-join-path eopengrok-database-root-dir (cu-dir-to-sha1 dir) "timestamp"))
         (project-timestamp (project-timestamp-file dir))
         (time-stamp (format-time-string "%Y.%m.%d-%H.%M.%S")))
    (unless (file-exists-p (file-name-directory project-timestamp))
      (make-directory (file-name-directory project-timestamp)))
    (dolist (f `(,timestamp-file ,project-timestamp))
      (with-current-buffer (find-file-noselect f)
        (erase-buffer)
        (insert time-stamp)
        (save-buffer)
        (kill-buffer)))))

(defun* grok-need-to-renew-index (dir)
  (let ((timestamp-file
         (cu-join-path eopengrok-database-root-dir (cu-dir-to-sha1 dir) "timestamp")))
    (unless (file-exists-p timestamp-file)
      (return-from grok-need-to-renew-index t))
    t))

(defun grok-construct-name-for (dir)
  (replace-regexp-in-string (expand-file-name "~/") "" (expand-file-name dir)))

(defun add-to-thread-list (thread)
  (with-current-buffer (global-grok-indexing-buffer)
    (let ((thread-list-copy (buffer-local-value 'thread-list (current-buffer))))
      (add-to-list 'thread-list-copy thread)
      (dolist (tr thread-list-copy)
        (when (not (thread-alive-p tr))
          (delete* tr thread-list-copy)))
      (setq-local thread-list thread-list-copy))))

(defun kill-thread-hook ()
  (interactive)
  (with-current-buffer (global-grok-indexing-buffer)
    (let ((thread-list-copy (buffer-local-value 'thread-list (current-buffer))))
      (if thread-list-copy
          (if (y-or-n-p (format "There are alive threads: [%s]\nKill them anyway?"
                                (reduce (lambda (a b) (concat a "," b))
                                        (mapcar 'thread-name thread-list-copy))))
              (progn
                (dolist (tr thread-list-copy)
                  (thread-signal tr 'error "Quit"))
                t)
            nil)
        t))))

(defun get-current-grok-threads ()
  (interactive)
  (with-current-buffer (global-grok-indexing-buffer)
    (print (buffer-local-value 'thread-list (current-buffer)))))

(defun install-kill-buffer-hook ()
  (with-current-buffer (global-grok-indexing-buffer)
    (let ((kill-buffer-hook-copy
           (buffer-local-value 'kill-buffer-hook (current-buffer))))
      (add-to-list 'kill-buffer-hook-copy 'kill-thread-hook)
      (setq-local kill-buffer-hook kill-buffer-hook-copy))))

(defun __thread-grok-index-main (&optional in-new-thread)
  (install-kill-buffer-hook)
  (setq some-thread-is-creating-grok-index nil)
  (when in-new-thread
    (add-to-thread-list (current-thread)))
  (while t
    (dolist (dir grok-indexing-list)
      (setq dir (expand-file-name dir))
      (grok-log (format "Check condition for dir: %s\n" dir))
      (when (and (grok-need-to-renew-index dir) (_open-indexing-maybe))
        (grok-log (format "Start to create grok index for %s\n" dir))
        (add-to-thread-list
         (make-thread `(lambda () (thread-opengrok-create-index ,dir))
                      (concat "index-" (grok-construct-name-for dir))))))
    (sleep-for 3)))

(defun thread-grok-index-main ()
  (interactive)
  (let ((main-grok-thread-name "grok-main-index-thread"))
    (if (member main-grok-thread-name (mapcar 'thread-name (all-threads)))
        (message "thread already started")
      (make-thread (lambda () (__thread-grok-index-main t))
                   main-grok-thread-name))
    (switch-to-buffer-other-window (global-grok-indexing-buffer))))

(defun kill-grok-main-thread ()
  (interactive)
  (when (member "grok-main-index-thread" (mapcar 'thread-name (all-threads)))
    (dolist (th (all-threads))
      (when (equal "grok-main-index-thread" (thread-name th))
        (thread-signal th 'error "Quit")))))

(provide 'init-grok-daemon)
