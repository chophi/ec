(defun program-exists-p (program)
  (not (equal (shell-command-to-string (concat "which " program)) "")))

(defun show-gerrit-at-point (&optional refresh)
  "Query the gerrit change at point with
'ssh -p 9418 <server> gerrit query --current-patch-set <id> --format json'
and pretty print the output into *gerrit [<id>]* buffer"
  (interactive "P")
  (let* ((str (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))
         (regex "https://\\(.*\\)/gerrit/.*?\\([0-9]+\\)")
         (link (progn (string-match regex str) (match-string 0 str)))
         (server (progn (string-match regex str) (match-string 1 str)))
         (change-id (progn (string-match regex str) (match-string 2 str)))
         (command
          (format
           "ssh -p 9418 %s gerrit query --current-patch-set %s --format json"
           server change-id))
         (buffer-name (format "*gerrit [%s]*" change-id)))
    (if (and (get-buffer buffer-name) (not refresh))
        nil
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert (shell-command-to-string command))
        (json-mode)
        (json-pretty-print-buffer)))
    (switch-to-buffer-other-window buffer-name)))

(provide 'init-common-utils)

