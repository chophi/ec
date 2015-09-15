(ede-cpp-root-project "cv"
                      :name "cv"
                      :file "~/.emacs.d/init.el" ;; make sure this file/dir exists
                      :include-path '("/src"
                                      "/test"
                                      "/tool"
                                      "/opencv-3.0.0/include/opencv"
                                      "/opencv-3.0.0/include")
                      :system-include-path '("/usr/local/include/c++/4.8.4/")
                      :spp-table '(("CV_PROP_RW" . "")
                                   ("CV_EXPORTS" . "")
                                   ("CV_EXPORTS_W_SIMPLE" . "")
                                   ("CV_EXPORTS_W" . "")
                                   ("CV_EXPORTS_W_MAP" . "")
                                   ("CV_INLINE" . "")))

(provide 'init-ede-projects)


