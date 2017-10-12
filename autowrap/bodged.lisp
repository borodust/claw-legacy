(in-package :bodge-autowrap)


(defun make-prefix-cutter (prefix-length)
  (lambda (name matches regex)
    (declare (ignore matches regex))
    (subseq name prefix-length)))


(defun expand-symbol-exceptions (symbol-exceptions)
  (loop for (name symbol) in symbol-exceptions
     collect (cons name (symbol-name symbol))))


(defun expand-symbol-prefix (symbol-prefixes)
  (labels ((make-regexp (prefix)
             (format nil "^~A\\w+$" prefix))
           (make-regex-pair (prefix)
             (list (make-regexp prefix) (length prefix))))
    (let ((pairs (mapcar #'make-regex-pair (sort symbol-prefixes #'> :key #'length))))
      (expand-symbol-regex pairs))))


(defun expand-symbol-regex (symbol-regexes)
  (loop for (regex idx) in symbol-regexes
     collect (list regex nil (list 'make-prefix-cutter idx))))


(defun parse-sysincludes (system includes)
  (loop for include in includes
     collect (if (stringp include)
                 include
                 (namestring
                  (asdf:component-pathname
                   (asdf:find-component (asdf:find-system system) include))))))


(defmacro c-include (header system-name &body body
                     &key package include-sources include-definitions
                       exclude-sources exclude-definitions
                       symbol-exceptions symbol-prefix prefix-regex
                       sysincludes)
  (declare (ignore body))
  `(%c-include
    ',(list system-name header)
    :spec-path ',(list system-name :spec)
    :definition-package ,package
    :local-os (or #+windows "windows-gnu" #+darwin "darwin-gnu")
    :include-arch ,(append #+(and unix (not darwin)) '("x86_64-pc-linux-gnu" "i686-pc-linux-gnu")
			   #+windows '("x86_64-pc-windows-gnu" "i686-pc-windows-gnu")
			   #+darwin '("x86_64-apple-darwin-gnu" "i686-apple-darwin-gnu"))
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
    :symbol-exceptions ,(expand-symbol-exceptions symbol-exceptions)
    :symbol-regex ,(append (expand-symbol-prefix symbol-prefix)
                           (expand-symbol-regex prefix-regex))))
