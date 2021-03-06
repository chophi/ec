(defvar tikz-latexmk-options nil)
(defvar tikz-svg-no-fonts t)
(defconst tikz-dvisvgm-options
  (concat "--clipjoin --page=1-" (if tikz-svg-no-fonts " --no-fonts" nil)))

(defconst tikz-output-buffer "*Generate tikz[Output]*")
(defconst tikz-error-buffer "*Generate tikz[Error]*")

(defun dvisvgm-extra-gs-args ()
  (if (and (equal os 'darwin)
           (file-exists-p "/usr/local/lib/libgs.dylib"))
      (format "--libgs=\"%s\"" "/usr/local/lib/libgs.dylib")
    ""))

(defun compile-tikz-to-svg (&optional file)
  (interactive)
  (let* ((tmp-dir (cu-join-path "/tmp" (sha1 default-directory)))
         (file (if file (expand-file-name file) buffer-file-name))
         (dir (file-name-directory file))
         (file-name (file-name-nondirectory file))
         (base-name (file-name-sans-extension file-name))
         (dvi-name (concat base-name ".dvi"))
         (svg-name (concat base-name (if tikz-svg-no-fonts  ".nofonts.svg" ".svg")))
         (cmd
          (format
           "latexmk %s %s --output-directory=%s && dvisvgm %s %s %s --output=%s"
           (or tikz-latexmk-options " ")
           file tmp-dir
           (dvisvgm-extra-gs-args)
           (or tikz-dvisvgm-options " ")
           (cu-join-path tmp-dir dvi-name)
           (cu-join-path tmp-dir svg-name)))
         (command-code (shell-command cmd tikz-output-buffer tikz-error-buffer))
         (prev-buffer (current-buffer)))
    (when (equal command-code 0)
      (kill-buffer tikz-output-buffer)
      (kill-buffer tikz-error-buffer)
      (let ((target
             (if (file-directory-p (cu-join-path dir "../images"))
                 (cu-join-path dir (concat "../images/" svg-name))
               (cu-join-path dir svg-name))))
        (copy-file (cu-join-path tmp-dir svg-name) target t)
        (find-file-noselect target)))))

(provide 'init-tikz)
