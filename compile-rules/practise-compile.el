(let* ((env_list
        (if (equal os 'darwin)
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
        (if (equal os 'darwin) "android-24" "android-25"))
       (project-root (expand-file-name "~/work/practise/ndk"))
       (curdir (expand-file-name default-directory))
       (command-list '())
       (android-makefile (cu-find-nearest-ancestor-match default-directory "Android.mk")))
  (when (string-prefix-p project-root curdir)
    (when android-makefile
      (let ((env_set_list '())
            (project-root (file-name-directory android-makefile)))
        (dolist (env env_list)
          (add-to-list 'env_set_list
                       `(term ,(format "export %s=%s" (car env) (cadr env))) t)
          (add-to-list 'env_set_list
                       `(term ,(format "export PATH=$%s:$PATH" (car env))) t))
        (add-to-list 'env_set_list `(term ,(format "export PATH=${ANDROID_SDK}/tools:$PATH")) t)
        (add-to-list 'env_set_list `(term ,(format "export PATH=${ANDROID_SDK}/platform-tools:$PATH")) t)
        (add-to-list 'command-list `(,project-root ((unit "set-env" ,@env_set_list))))
        (add-to-list 'command-list
                     `(,project-root
                       ((unit "build"
                              (term ,(format "ndk-build")))
                        (unit "run"
                              (term (let ((executable 
                                           (ido-completing-read "Executable: "
                                                                (split-string
                                                                 (shell-command-to-string
                                                                  (if (equal os 'darwin)
                                                                      "find ../libs -type f -perm +111"
                                                                    "find ../libs -type f -executable"))))))
                                    (format "adb root &&\n adb push %s /data/ &&\n adb shell chmod 555 /data/%s &&\n echo -e \"\\n\\n== begin run program == \" &&\n adb shell /data/%s"
                                            executable
                                            (file-name-nondirectory executable)
                                            (file-name-nondirectory executable)))))
                        ))))))
  command-list)

