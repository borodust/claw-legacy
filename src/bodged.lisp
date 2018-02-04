(in-package :claw)


(defun parse-sysincludes (system includes)
  (loop for include in includes
     collect (if (stringp include)
                 include
                 (namestring
                  (asdf:component-pathname
                   (asdf:find-component (asdf:find-system system) include))))))


(defun package-functions (package-name)
  (loop for sym being the symbol in (find-package package-name)
        as fu = (find-function sym)
        when fu collect fu))


(defmacro c-include (header system-name &body body
                     &key in-package include-sources include-definitions
                       exclude-sources exclude-definitions
                       rename-symbols sysincludes (windows-environment "gnu"))
  (declare (ignore body))
  `(progn
     (%c-include
      ',(list system-name header)
      :spec-path ',(list system-name :spec)
      :definition-package ,in-package
      :local-environment #+windows ,windows-environment
                         #-windows "gnu"
      :include-arch ("x86_64-pc-linux-gnu"
                     "i686-pc-linux-gnu"
                     ,(string+ "x86_64-pc-windows-" windows-environment)
                     ,(string+ "i686-pc-windows-" windows-environment)
                     "x86_64-apple-darwin-gnu"
                     "i686-apple-darwin-gnu")
      :sysincludes ',(append (parse-sysincludes system-name sysincludes)
                             #+(and unix (not darwin))
                             (let* ((gcc-ver (dump-gcc-version))
                                    (x86-64-includes (string+ "/usr/lib/gcc/x86_64-pc-linux-gnu/"
                                                              gcc-ver
                                                              "/include/"))
                                    (x86-includes (string+ "/usr/lib/gcc/i686-pc-linux-gnu/"
                                                           gcc-ver
                                                           "/include/")))
                               (append (when (uiop:directory-exists-p x86-64-includes)
                                         (list x86-64-includes))
                                       (when (uiop:directory-exists-p x86-includes)
                                         (list x86-includes))
                                       (list "/usr/include/linux/")))
                             #+windows
                             (list "c:/msys64/mingw64/x86_64-w64-mingw32/include/"
                                   "c:/msys64/mingw64/include/"
                                   "c:/msys64/usr/local/include/"))
      :include-sources ,include-sources
      :include-definitions ,include-definitions
      :exclude-sources ,exclude-sources
      :exclude-definitions ,exclude-definitions
      :no-accessors t
      :filter-spec-p t
      :symbol-regex ,rename-symbols)
     ,@(let ((dump-fu-name (format-symbol in-package 'dump-claw-c-wrapper)))
         `((defun ,dump-fu-name (library-path)
             (write-c-library-implementation library-path
                                             ,header
                                             (package-functions ,in-package)))
           (export ',dump-fu-name ,in-package)))))
