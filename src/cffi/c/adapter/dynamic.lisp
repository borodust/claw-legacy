(cl:in-package :claw.cffi.c)


(defgeneric initialize-adapter (library-name))


(defclass dynamic-adapter (adapter) ())


(defun make-dynamic-adapter (wrapper path)
  (make-instance 'dynamic-adapter :wrapper wrapper :path path))


(defun load-dynamic-adapter-template ()
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :claw/cffi "src/cffi/c/adapter/template/dynamic.c")))


(defun library-loader-name (library-name)
  (let* ((library-string (symbol-name library-name))
         (c-name (ppcre:regex-replace-all "[^_\\w]+" library-string "_"))
         (hex (claw-sha1:sha1-hex
               (concatenate 'string
                            (if-let ((package (symbol-package library-name)))
                              (package-name package)
                              "")
                            (symbol-name library-name)))))
    (subseq (format nil "~A~(~A~)_loader_~A" +adapted-function-prefix+ c-name hex) 0 64)))


(defun preprocess-dynamic-adapter-template (includes
                                            loader-name
                                            function-types
                                            function-pointers
                                            function-pointers-init
                                            function-definitions)
  (preprocess-template (load-dynamic-adapter-template)
                       "timestamp" (get-timestamp)
                       "includes" includes
                       "loader-name" loader-name
                       "function-types" function-types
                       "function-pointers" function-pointers
                       "function-pointers-init" function-pointers-init
                       "function-definitions" function-definitions))


(defmethod generate-adapter-file ((this dynamic-adapter))
  (when (%adapter-needs-rebuilding-p this)
    (ensure-directories-exist (uiop:pathname-directory-pathname (adapter-file-of this)))
    (with-output-to-file (output (adapter-file-of this) :if-exists :supersede)
      (let* ((functions (functions-of this))
             (definitions
               (%generate-adapted-function-definitions functions
                                                       +adapted-variable-prefix+)))
        (flet ((function-types ()
                 (with-output-to-string (out)
                   (loop for function in functions
                         do (format out "~&")
                            (%generate-function-type function out))))
               (function-variables ()
                 (with-output-to-string (out)
                   (loop for function in functions
                         do (format out "~&")
                            (%generate-function-variable function out))))
               (function-variable-inits ()
                 (with-output-to-string (out)
                   (loop for function in functions
                         do (format out "~&")
                            (%generate-function-variable-init function out))))
               (header-files ()
                 (format nil "~{#include \"~A\"~^~%~}" (headers-of this))))
          (format output "~A"
                  (preprocess-dynamic-adapter-template
                   (header-files)
                   (library-loader-name (wrapper-name-of this))
                   (function-types)
                   (function-variables)
                   (function-variable-inits)
                   definitions)))))))


(defun build-dynamic-adapter (standard adapter-file includes target-file &key pedantic)
  (uiop:run-program (append (list "gcc" "-shared" (namestring adapter-file))
                            (when standard
                              (list (format nil "-std=~A" standard)))
                            (when pedantic
                              (list "-pedantic"))
                            (list "-O3" "-fPIC")
                            (loop for directory in includes
                                  collect (format nil "-I~A"
                                                  (namestring directory)))
                            (list "-o" (namestring target-file)))
                    :output *standard-output*
                    :error-output *debug-io*))


(defun %verify-adapter-initialization (result)
  (unless (zerop result)
    (error "Failed to initialize adapater")))


(defun shared-extension-name ()
  #+(and unix (not darwin))
  "so"
  #+windows
  "dll"
  #+darwin
  "dylib"
  #-(or windows unix darwin)
  (error "Unrecognized system"))


(defun default-shared-adapter-library-name ()
  (format nil "adapter.~A" (shared-extension-name)))


(defmethod expand-adapter-routines ((this dynamic-adapter) wrapper)
  (let ((name (wrapper-name-of this))
        (shared-library-name (default-shared-adapter-library-name)))
    `((defmethod build-adapter ((wrapper-name (eql ',name)) &optional target)
        (declare (ignore wrapper-name))
        (build-dynamic-adapter ,(standard-of this)
                               ,(adapter-file-of this)
                               (list ,@(includes-of this))
                               (merge-pathnames (or target ,shared-library-name)
                                                ,(claw.wrapper:merge-wrapper-pathname
                                                  "" wrapper))))
      (defmethod initialize-adapter ((wrapper-name (eql ',name)))
        (declare (ignore wrapper-name))
        (%verify-adapter-initialization
         (cffi:foreign-funcall ,(library-loader-name name) :int))))))
