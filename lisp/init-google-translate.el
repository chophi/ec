(defvar google-translate-program "~/.emacs.d/scripts/trans")
(defvar google-translate-buffer "*google-translate*")

(defun _google-translate (to-language &optional brief query)
  (shell-command (format "%s :%s %s \"%s\"" google-translate-program
                         to-language (if brief "-b" "")
                         (if query
                             (read-string "To Translate: ")
                           (cu-read-word-or-region)))
                 google-translate-buffer
                 google-translate-buffer))

(defun google-translate-at-point-to-english ()
  (interactive)
  (_google-translate "en"))

(defun google-translate-at-point-to-chinese ()
  (interactive)
  (_google-translate "ch"))

(defun google-translate-query-to-english ()
  (interactive)
  (_google-translate "en" nil t))

(defun google-translate-query-to-chinese ()
  (interactive)
  (_google-translate "ch" nil t))

(provide 'init-google-translate)
