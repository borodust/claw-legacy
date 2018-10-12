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


(defgeneric dump-c-wrapper (package-name wrapper-path))


(defmacro c-include (header system-name &body body
                     &key in-package include-sources include-definitions
                       exclude-sources exclude-definitions
                       (spec-module :spec) rename-symbols
                       sysincludes
                       includes
                       (windows-environment "gnu")
                       language standard)
  (declare (ignore body))
  (destructuring-bind (in-package &rest nicknames) (ensure-list in-package)
    (unless in-package
      (error ":in-package must be supplied"))
    `(progn
       (uiop:define-package ,in-package
           (:nicknames ,@nicknames)
         (:use))
       (%c-include
        ',(list system-name header)
        :spec-path ',(list system-name spec-module)
        :definition-package ,in-package
        :local-environment #+windows ,windows-environment
        #-windows "gnu"
        :local-only ,*local-only*
        :include-arch ("x86_64-pc-linux-gnu"
                       "i686-pc-linux-gnu"
                       ,(string+ "x86_64-pc-windows-" windows-environment)
                       ,(string+ "i686-pc-windows-" windows-environment)
                       "x86_64-apple-darwin-gnu"
                       "i686-apple-darwin-gnu")
        :sysincludes ',(append (parse-sysincludes system-name sysincludes)
                               (dump-all-gcc-include-paths))
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
                                         (package-functions ,in-package))))))
