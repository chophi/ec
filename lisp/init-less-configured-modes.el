;;; apples-mode
(require 'apples-mode)
(add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))

(defun my-run (params)
  (interactive "P")
  (if params
      (setq params (read-string (format "osascript %s \\\n" (buffer-file-name))))
    (setq params " "))
  (shell-command (format "osascript %s %s" (buffer-file-name) params)))

;;; glsl-mode
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))

;;; gn-mode
(require 'gn-mode)

;;; haskell mode
(require 'haskell-mode)

;;; js/css/html
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;; json mode
(require 'json)
(add-to-list 'auto-mode-alist '("\\.bp\\'" . json-mode))

;;; kotlin mode
(flycheck-kotlin-setup)

(add-hook 'kotlin-mode-hook 'flycheck-mode)
(add-hook 'kotlin-mode-hook 'electric-pair-mode)

(add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode))

(add-to-path (format "%s/.sdkman/candidates/kotlin/current/bin" (getenv "HOME")))
(when (eq os 'linux)
  (add-to-path "/usr/lib/jvm/java-8-openjdk-amd64/bin" t)
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-8-openjdk-amd64"))

;;; latex
(add-to-list 'auto-mode-alist '("\\.latex\\'" . latex-mode))
(add-hook 'LaTeX-mode-hook
          (lambda()
            (push '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t) TeX-command-list)
            (push '("XeLaTeX(minted)" "%`xelatex%(mode) -shell-escape %' %t" TeX-run-TeX nil t)
                  TeX-command-list)
            (setq TeX-command-default "XeLaTeX(minted)")
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

;;; logcat mode
;;; before using this module, please install fb-adb
;;; git clone https://github.com/facebook/fb-adb.git
;; ./autogen.sh
;; export ANDROID_NDK=/path/to/android-ndk
;; mkdir build
;; cd build
;; ../configure
;; make
;; sudo make install
(require 'logcat)
(add-to-list 'auto-mode-alist '("\.logcat$" . logcat-mode))
(add-to-list 'auto-mode-alist '("/logcat.*\.log$" . logcat-mode))
(add-hook 'logcat-mode-hook '(lambda () (line-number-mode 1)))

;;; lua-mode
(require 'lua-mode)

;;; markdown
(require 'markdown-mode)
(require 'markdown-mode+)

;;; openwith
(require 'openwith)
(openwith-mode)

(setq
 openwith-associations
 (case os
   ('linux
    '(;;("\\.pdf$" "evince" (file))
      ("\\.mp3$" "mplayer" (file) )
      ("\\.mov\\|\\.RM$\\|\\.RMVB$\\|\\.avi$\\|\\.AVI$\\|\\.flv$\\|\\.mp4\\|\\.mkv$\\|\\.rmvb$" "mplayer" (file) )
      ("\\.CHM$\\|\\.chm$" "chmsee"  (file))
      ("\\.\\(?:jp?g\\|png\\|svg\\|pdf\\)\\'" "remote-show-image"
       (file))))
   ('darwin
    '(("\\.pdf\\|svg$" "open" (file))))))

;;; yasnippet
(yas-global-mode 1)

;;; protobuf mode
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; TODO: FIXME: funny char shows when poly-org-mode enabled.
(setq auto-mode-alist  (delete '("\\.org\\'" . poly-org-mode) auto-mode-alist))

;;; swift mode
(eval-after-load 'flycheck '(flycheck-swift-setup))

(when (eq os 'linux)
  (add-to-list 'exec-path "~/software/swift-dev/bin/")
  (add-to-path "~/software/swift-dev/bin/")
  (setq swift-mode:repl-executable "repl_swift"))

;;; smali mode
(autoload 'smali-mode "smali-mode" "Major mode for editing and viewing smali issues" t)
(add-to-list 'auto-mode-alist '(".smali$" . smali-mode))

(provide 'init-less-configured-modes)
