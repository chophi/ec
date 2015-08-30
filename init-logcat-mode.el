;;; before using this module, please install fb-adb
;;; git clone https://github.com/facebook/fb-adb.git
;; ./autogen.sh
;; export ANDROID_NDK=/path/to/android-ndk
;; mkdir build
;; cd build
;; ../configure
;; make
;; sudo make install
(add-to-list 'load-path "~/.emacs.d/site-lisp/logcat-mode")
(require 'logcat)

(add-to-list 'auto-mode-alist '("\.logcat$" . logcat-mode))
(add-to-list 'auto-mode-alist '("/logcat.*\.log$" . logcat-mode))

(provide 'init-logcat-mode)
