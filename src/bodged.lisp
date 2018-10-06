(cl:in-package :claw)

(defvar *local-only* nil)

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

(defun collect-unix-system-includes ()
  (let ((gcc-ver (dump-gcc-version)))
    (loop for path in (list "/usr/include/"
                            "/usr/local/include/"
                            "/usr/lib/gcc/x86_64-linux-gnu/"
                            "/usr/lib/gcc/i686-linux-gnu/"
                            ;; archlinux
                            (string+ "/usr/lib/gcc/x86_64-pc-linux-gnu/" gcc-ver "/include/")
                            (string+ "/usr/lib/gcc/i686-pc-linux-gnu/" gcc-ver "/include/")
                            (string+ "/usr/include/c++/" gcc-ver "/")
                            (string+ "/usr/include/c++/" gcc-ver "/x86_64-pc-linux-gnu/")
                            (string+ "/usr/include/c++/" gcc-ver "/i686-pc-linux-gnu/")
                            ;; ubuntu
                            (string+ "/usr/include/c++/" gcc-ver "/")
                            (string+ "/usr/include/x86_64-linux-gnu/c++/" gcc-ver "/")
                            (string+ "/usr/include/i686-linux-gnu/c++/" gcc-ver "/")
                            (string+ "/usr/lib/gcc/x86_64-linux-gnu/" gcc-ver "/include/")
                            (string+ "/usr/lib/gcc/i686-linux-gnu/" gcc-ver "/include/"))
          when (uiop:directory-exists-p path)
            collect path)))


(defgeneric dump-c-wrapper (package-name wrapper-path))


(defmacro c-include (header system-name &body body
                     &key in-package include-sources include-definitions
                       exclude-sources exclude-definitions
                       (spec-module :spec) rename-symbols
                       sysincludes
                       includes
                       (windows-environment "gnu")
                       (local-only nil) language standard)
  (declare (ignore body))
  `(progn
     (%c-include
      ',(list system-name header)
      :spec-path ',(list system-name spec-module)
      :definition-package ,in-package
      :local-environment #+windows ,windows-environment
      #-windows "gnu"
      :local-only ,(or *local-only* local-only)
      :include-arch ("x86_64-pc-linux-gnu"
                     "i686-pc-linux-gnu"
                     ,(string+ "x86_64-pc-windows-" windows-environment)
                     ,(string+ "i686-pc-windows-" windows-environment)
                     "x86_64-apple-darwin-gnu"
                     "i686-apple-darwin-gnu")
      :sysincludes ',(append (parse-sysincludes system-name sysincludes)
                             #+(and unix (not darwin))
                             (collect-unix-system-includes)
                             #+windows
                             (list "c:/msys64/mingw64/x86_64-w64-mingw32/include/"
                                   "c:/msys64/mingw64/include/"
                                   "c:/msys64/mingw32/i686-w64-mingw32/include/"
                                   "c:/msys64/mingw32/include/"
                                   "c:/msys64/usr/local/include/"))
      :includes ',(parse-sysincludes system-name includes)
      :include-sources ,include-sources
      :include-definitions ,include-definitions
      :exclude-sources ,exclude-sources
      :exclude-definitions ,exclude-definitions
      :filter-spec-p t
      :language ,language
      :standard ,standard
      :symbol-regex ,rename-symbols)
     (defmethod dump-c-wrapper ((package-name (eql ,in-package)) wrapper-path)
       (declare (ignore package-name))
       (write-c-library-implementation wrapper-path
                                       ,header
                                       (package-functions ,in-package)))))
