(defun path-seperator() (if *is-windows-system-p* ";" ":"))
(defun head-to-path(new-path)
  (setenv "PATH" (concat (expand-file-name new-path) (path-seperator) (getenv "PATH"))))
(defun append-to-path(new-path)
  (setenv "PATH" (concat (getenv "PATH") (path-seperator) (expand-file-name new-path))))

(let ((win-append-to-path-list
       '("d:/ProgEnv/Ruby21-x64/bin/"
	 "d:/ProgEnv/RubyDevkit/bin/"
	 "~/scripts"
	 "d:/ProgEnv/texlive/2014/bin/win32/"
         "D:/ProgEnv/gs9.15/bin"
         "D:/ProgEnv/R-3.1.2/bin"
         ;; 这两个放最后
         "d:/ProgEnv/Git/bin"
         "d:/ProgEnv/mingw/bin"))
      (win-exec-path-list
       '("d:/ProgEnv/Racket/"
         "D:/ProgEnv/gs9.15/bin"
         "D:/ProgEnv/R-3.1.2/bin"))
      (win-head-to-path-list
       '(""))      
      ;; linux path list
      (linux-append-to-path-list
       '(""
	 ))

      (linux-head-to-path-list
       '("~/.emacs.d/.emacs-bin"))
      (linux-exec-path-list
       '(""
	 )))
  (dolist (p (if *is-windows-system-p* win-append-to-path-list linux-append-to-path-list))
    (append-to-path p))
  (dolist (p (if *is-windows-system-p* win-exec-path-list linux-exec-path-list))
    (add-to-list 'exec-path p))
  (dolist (p (if *is-windows-system-p* win-head-to-path-list linux-head-to-path-list))
    (head-to-path p)))

(provide 'init-path)

