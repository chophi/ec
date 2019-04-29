(defun lc-get-next-unsolved-problem ()
  (interactive)
  (cu-strip-string
   (shell-command-to-string
    "leetcode list -q LhD | tac | head -6 | tail -1 | cut -d \"[\" -f 2 | cut -d \"]\" -f 1")
   t t))

(defvar lc-home-dir (format "%s/work/lc" (getenv "HOME")))
(defvar lc-default-output "*[leetcode output]*")

(defun lc-get-source-for-next-problem ()
  (interactive)
  (let* ((next-p (lc-get-next-unsolved-problem))
         (check-file-command (format "ls %s/%s.*" lc-home-dir next-p)))
    (if (equal (shell-command check-file-command) 0)
        (cu-strip-string (shell-command-to-string check-file-command) t t)
      (cu-strip-string
       (shell-command-to-string
        (format "leetcode show %s -gx -l cpp -o %s | grep \"Source Code\" | cut -d \":\" -f 2"
                next-p lc-home-dir))
       t t))))

(defun lc-open-next-problem ()
  (interactive)
  (find-file-other-window (lc-get-source-for-next-problem)))

(defun lc-test-current-buffer ()
  (interactive)
  (shell-command (format "leetcode test %s" buffer-file-name)
                 lc-default-output lc-default-output))

(defun lc-judge-current-buffer ()
  (interactive)
  (shell-command (format "leetcode submit %s" buffer-file-name)
                 lc-default-output lc-default-output))

(defun lc-clear-cache ()
  (interactive)
  (let ((display-buffer-alist '(("*Async Shell Command*"  display-buffer-no-window))))
    (async-shell-command "lc-clear-cache.sh")))
(provide 'init-leetcode-cli)
