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
                       rename-symbols sysincludes)
  (declare (ignore body))
  `(progn
     (%c-include
      ',(list system-name header)
      :spec-path ',(list system-name :spec)
      :definition-package ,in-package
      :local-environment "gnu"
      :include-arch ("x86_64-pc-linux-gnu" "i686-pc-linux-gnu"
                                           "x86_64-pc-windows-gnu" "i686-pc-windows-gnu"
                                           "x86_64-apple-darwin-gnu" "i686-apple-darwin-gnu")
      :sysincludes ',(append (parse-sysincludes system-name sysincludes)
                             #+(and unix (not darwin))
                             (list "/usr/include/x86_64-pc-linux-gnu/")
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
