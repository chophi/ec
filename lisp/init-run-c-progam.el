(defun make-run-list-for-c()
  "Make a run list for c program.
each list formated like: '(key (requried_file_list) command_to_execute),
and there are format specifier can be used as below:
%F  absolute pathname           :  /usr/local/bin/netscape.bin
%f  file name without directory :  netscape.bin
%n  file name without extension :  netscape
%e  extension of file name      :  bin
"
  (let ((program (if (eq os 'windows) "%n.exe" "./%n")))
    `((?e (,program) ,(format "%s &" program))
      (?i nil (open-the-input-file "%n.in"))
      (?< (,program) ,(format "%s < %%n.in &" program))
      (?s (,program) (select-input-file-and-run ,program))
      (?t nil (delete-input-output-files)))))

(require 'init-windows)

;; Don't remove this function.
;; It's used to provide a name in the help message when call run-c-program
(defun open-the-input-file (file-name)
  (split-window-horizontally-instead)
  (find-file-other-window file-name))

(defun select-input-file-and-run (program)
  (let* ((input (ido-read-file-name "read-input-file: "))
         (output (concat (file-name-sans-extension input) ".out")))
    (shell-command (format "%s < %s > %s" program input output))))

(defun delete-input-output-files()
  (interactive)
  (let ((in-out-flist (directory-files "." t "\\(in\\|out\\)$")))
    (when (yes-or-no-p
           (concat "remove all these files: "
                   (let ((ret ""))
                     (dolist (name in-out-flist ret)
                       (setq ret (concat ret "\n" name))))))
      (dolist (name in-out-flist)
        (delete-file name)))))

(defun run-c-program ()
  (interactive)
  (cu-smart-run
   (make-run-list-for-c)))

(provide 'init-run-c-progam)
