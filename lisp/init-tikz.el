(defvar tikz-latexmk-options nil)
(defconst tikz-dvisvgm-options "--clipjoin --page=1- --no-fonts")
(defconst tikz-output-buffer "*Generate tikz[Output]*")
(defconst tikz-error-buffer "*Generate tikz[Error]*")

(defun dvisvgm-extra-gs-args ()
  (if (and (equal os 'macos)
           (file-exists-p "/usr/local/lib/libgs.dylib"))
      (format "--libgs=\"%s\"" "/usr/local/lib/libgs.dylib")
    ""))

(defun compile-tikz-to-svg ()
  (interactive)
  (let* ((tmp-dir (cu-join-path "/tmp" (sha1 default-directory)))
         (file-name (file-name-nondirectory buffer-file-name))
         (base-name (file-name-sans-extension file-name))
         (dvi-name (concat base-name ".dvi"))
         (svg-name (concat base-name ".svg"))
         (cmd
          (format
           "latexmk %s %s --output-directory=%s && dvisvgm %s %s %s --output=%s"
           (or tikz-latexmk-options " ")
           buffer-file-name tmp-dir
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
             (if (file-directory-p (cu-join-path default-directory "../images"))
                 (cu-join-path default-directory (concat "../images/" svg-name))
               (cu-join-path default-directory svg-name))))
        (copy-file (cu-join-path tmp-dir svg-name) target t)
        (find-file-noselect target)))))

(provide 'init-tikz)
