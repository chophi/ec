(defvar lc-default-output "*[leetcode output]*")

(defun lc-get-all-unsolved-problem ()
  (interactive)
  (let ((cmdline-get-hard "leetcode list -q LhD | tac | cut -d \"[\" -f 2 | cut -d \"]\" -f 1 | xargs")
        (cmdline-get-medium "leetcode list -q LmD | tac | cut -d \"[\" -f 2 | cut -d \"]\" -f 1 | xargs")
        ret)
    (dolist (name-cmd-pair `((hard . ,cmdline-get-hard)
                             (medium . ,cmdline-get-medium)) ret)
      (add-to-list
       'ret
       (cons (car name-cmd-pair)
             (split-string
              (cu-strip-string
               (shell-command-to-string
                (cdr name-cmd-pair))
               t t) " "))))))

(defun lc-fetch-source-for-problem (problem-id language save-dir)
  (let* ((check-file-command (format "ls %s/%s.*" save-dir problem-id))
         (check-language-supports (format "leetcode show %s -gx -l %s -o /tmp/leetcode && rm -rf /tmp/leetcode"
                                          problem-id language)))
    (if (equal (shell-command check-file-command) 0)
        (cu-strip-string (shell-command-to-string check-file-command) t t)
      (unless (equal (shell-command check-language-supports) 0)
        (error "unsupported language(%s) for (%s)" language problem-id))
      (cu-strip-string
       (shell-command-to-string
        (format
         "leetcode show %s -gx -l %s -o %s | grep \"Source Code\" | cut -d \":\" -f 2"
         problem-id language save-dir))
       t t))))

(defun lc-fetch-all-unsolved-problems (&optional language)
  (interactive)
  (unless language
    (setq language "java"))
  (dolist (problems (lc-get-all-unsolved-problem))
    (let* ((problem-type (car problems))
           (problem-list (cdr problems))
           (dir (cu-join-path (getenv "HOME") "work/lc" (symbol-name problem-type))))
      (dolist (id problem-list)
        (lc-fetch-source-for-problem id language dir)))))

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
