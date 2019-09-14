(cl:in-package :claw.cffi.c)


(defclass static-adapter (adapter) ())


(defun make-static-adapter (wrapper path)
  (make-instance 'static-adapter :wrapper wrapper :path path))


(defun load-static-adapter-template ()
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :claw/cffi "src/template/static.c")))


(defun preprocess-static-adapter-template (header-file function-definitions)
  (preprocess-template (load-static-adapter-template)
                       "timestamp" (get-timestamp)
                       "header-file" header-file
                       "function-definitions" function-definitions))


(defmethod generate-adapter-file ((this static-adapter))
  (ensure-directories-exist (uiop:pathname-directory-pathname (adapter-file-of this)))
  (with-output-to-file (output (adapter-file-of this) :if-exists :supersede)
    (write-sequence "" output))
  #++(format output "~A" (preprocess-static-adapter-template
                          (library-headers library-name)
                          (%generate-adapted-function-definitions library-name))))


(defun build-static-adapter (library-name target-file)
  (declare (ignore library-name target-file))
  (error "unimplemented"))


(defmethod expand-adapter-routines ((this static-adapter) wrapper))
