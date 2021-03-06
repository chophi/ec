(require 'init-lsp)

(use-package lsp-java :ensure t :after lsp)

(setq-default
 lsp-java-server-install-dir (cu-join-path (getenv "HOME") ".jdt-server/")
 lsp-java-workspace-dir lsp-java-server-install-dir
 lsp-java-boot-enabled nil)

(use-package dap-java :after (lsp-java))

(require 'lsp-java-boot)

(defun my-set-extra-jdt-ls-vmargs ()
  (let ((lombok (cu-list-files-recursively "~/.jdt-extra-packages/" "lombok.*jar" 2)))
        (when (and lombok (file-exists-p (car lombok)))
          (add-to-list 'lsp-java-vmargs (format "-javaagent:%s" (car lombok)))
          (add-to-list 'lsp-java-vmargs (format "-Xbootclasspath/a:%s" (car lombok))))))

(defun my-check-enable-lsp-for-java-hook ()
  (interactive)
  (when (and (featurep 'projectile) (projectile-project-p)
             (file-exists-p (cu-join-path (projectile-project-root) ".classpath")))
    (let ((lsp-auto-guess-root t))
      (my-set-extra-jdt-ls-vmargs)
      (lsp)
      (lsp-java-boot-lens-mode 1))))

(require 's)
(require 'f)
(defun my-make-jdt-project-configuration ()
  (interactive)
  (unless (executable-find "brazil-path")
    (error "Can't find exectuable brazil-path"))
  (let ((default-directory (projectile-project-root))
        (libs nil)
        (lib-str nil)
        (package-name nil))

    ;; get classpaths
    (setq libs (shell-command-to-string "brazil-path testrun.classpath 2>/dev/null"))
    (setq libs (seq-filter (lambda (lib) (not (string-empty-p lib)))
                           (mapcar (lambda (path) (cu-strip-string path t t))
                                   (split-string libs ":"))))
    (unless libs (error "No libs found"))

    ;; get package-name
    (setq package-name (shell-command-to-string "brazil-path package-name 2>/dev/null"))
    (setq package-name (cu-strip-string package-name t t))
    (unless (and package-name (not (string-empty-p package-name)))
      (error "No package found"))

    ;; generate the lib list string
    ;; Some extra libs in ~/.jdt-extra-packages/ will also be added
    (dolist (lib (append libs (cu-list-files-recursively "~/.jdt-extra-packages/" ".*jar" 2) lib-str))
      (setq lib-str (concat lib-str (s-lex-format "\t<classpathentry kind=\"lib\" path=\"${lib}\"/>\n"))))

    ;; write the .classpath
    (f-write-text
     (s-lex-format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<classpath>
	<classpathentry kind=\"src\" path=\"src\"/>
	<classpathentry kind=\"src\" path=\"tst\"/>
${lib-str}
	<classpathentry kind=\"con\" path=\"org.eclipse.jdt.launching.JRE_CONTAINER\"/>
	<classpathentry kind=\"output\" path=\"eclipse-bin\"/>
</classpath>") 'utf-8 (cu-join-path default-directory ".classpath"))

    ;; write the .project
    (f-write-text
     (s-lex-format
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<projectDescription>
	<name>${package-name}</name>
	<comment></comment>
	<projects>
	</projects>
	<buildSpec>
		<buildCommand>
			<name>org.eclipse.jdt.core.javabuilder</name>
			<arguments>
			</arguments>
		</buildCommand>
	</buildSpec>
	<natures>
		<nature>org.eclipse.jdt.core.javanature</nature>
		<nature>amazon.devtools.brazil.eclipse.BrazilEclipsePlugin.brazilNature</nature>
	</natures>
</projectDescription>")
     'utf-8 (cu-join-path default-directory ".project"))

    ;; cheers
    (message "Successfully create configuration for %s" default-directory)))

;; check and enable lsp conditionally
(add-hook 'lsp-java-mode #'lsp-lens-mode)
(add-hook 'java-mode-hook #'my-check-enable-lsp-for-java-hook)

(provide 'init-java)
