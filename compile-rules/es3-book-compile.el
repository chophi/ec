(let* ((env_list
        (if (equal os 'mac)
            '(("JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/")
              ("ANDROID_SDK" "~/Android/sdk/")
              ("ANDROID_NDK" "~/Android/android-ndk-r10e/")
              ("ANT_DIR" "/usr/local/bin/"))
          '(("JAVA_HOME" "~/software/jdk1.8.0_91/")
            ("ANDROID_SDK" "~/software/android-sdk-linux/")
            ("ANDROID_NDK" "~/software/android-ndk-r10e/")
            ("ANT_DIR" "/usr/bin/"))
          ))
       (android-platform-target
        (if (equal os 'mac) "android-24" "android-25"))
       (project-root (expand-file-name "~/git-repo/opengles3-book"))
       (curdir (expand-file-name default-directory))
       (command-list '()))
  (when (string-prefix-p project-root curdir)
    (let ((env_set_list '()))
      (dolist (env env_list)
        (add-to-list 'env_set_list
                     `(term ,(format "export %s=%s" (car env) (cadr env))) t)
        (add-to-list 'env_set_list
                     `(term ,(format "export PATH=$%s:$PATH" (car env))) t))
      (add-to-list 'env_set_list `(term ,(format "export PATH=${ANDROID_SDK}/tools:$PATH")) t)
      (add-to-list 'env_set_list `(term ,(format "export PATH=${ANDROID_SDK}/platform-tools:$PATH")) t)
      (add-to-list 'command-list `(,curdir ((unit "set-env" ,@env_set_list))))
      )
    (when (string-prefix-p (concat project-root "/Chapter") curdir)
      (let ((relative-path
             (if (string-match "Android" curdir)
                 (concat (substring curdir
                                    (1+ (length project-root))
                                    (string-match "Android" curdir))
                         "Android")
               (concat (substring curdir (1+ (length project-root)))
                       (car (sort 
                             (split-string (shell-command-to-string "find . -name \"Android\""))
                             (lambda (first second)
                               (or (not (string-match "Android" second))
                                   (< (length first) (length second))))))
                       )
               )))
        (add-to-list 'command-list
                     `(,(concat project-root "/" relative-path)
                       ((unit "build"
                              (term ,(format "android update project -p . -t %s" android-platform-target))
                              (term "cd jni && ndk-build && cd .. && ant debug"))
                        (unit "install"
                               (term "adb install -r bin/NativeActivity-debug.apk"))))))))
    command-list)
