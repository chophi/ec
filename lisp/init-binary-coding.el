(defun __is_base64_char (c)
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9))
      (eq c ?+)
      (eq c ?/)
      (eq c ?=)))

(defun __valid_base64_string? (string)
  (equal (shell-command-to-string
          (format "echo %s | base64 -d 2>&1 | grep \"invalid input\"" string))
         ""))

(defun __decode_base64_may_append_equal (string)
  (if (__valid_base64_string? string)
      (base64-decode-string string)
    (if (__valid_base64_string? (concat string "="))
        (base64-decode-string  (concat string "="))
      (if (__valid_base64_string? (concat string "=="))
          (base64-decode-string (concat string "=="))
        string))))

(defun __convert-all-to-base64 (string)
  (let ((beg 0)
        (end 0)
        (ret "")
        (len (length string)))
    (while (< end len)
      (when (not (__is_base64_char (elt string end)))
        (if (> end beg)
            (setq ret
                  (format
                   "%s%s%c" ret
                   (__decode_base64_may_append_equal (substring string beg end))
                   (elt string end)))
          (setq ret (format "%s%c" ret (elt string end))))
        (setq beg (1+ end)))
      (setq end (1+ end)))
    (when (__is_base64_char (elt string (1- len)))
      (setq ret (format
                 "%s%s" ret
                 (__decode_base64_may_append_equal (substring string beg len)))))
    ret))

(defun my-region-base64-decode (start end)
  "Convert the select region from base64 to string"
  (interactive "*r")
  (let* ((content (buffer-substring-no-properties start end))
         (result (__convert-all-to-base64 content))
         (selected (y-or-n-p (format "The output is as following(y/n)? \n %s \n:" result))))
    (when selected
      (delete-region start end)
      (insert result))))

(defun my-region-base64-decode-to-hex-string (start end)
  "Convert the select region from base64 to hex string"
  (interactive "*r")
  (let* ((content (buffer-substring-no-properties start end))
         (result (__convert-all-to-base64 content))
         (selected (y-or-n-p (format "The output is as following(y/n)? \n %s \n:" result))))
    (when selected
      (delete-region start end)
      (insert (url-hexify-string result)))))

(defun my-func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun my-hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (my-func-region start end #'url-hexify-string))

(defun my-unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (my-func-region start end #'url-unhex-string))

(provide 'init-binary-coding)
