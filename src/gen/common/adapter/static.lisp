(cl:in-package :claw.generator.common)


(defclass static-adapter (adapter) ())


(defun make-static-adapter (wrapper path extract-pointers package)
  (make-instance 'static-adapter :wrapper wrapper
                                 :path path
                                 :extract-pointers extract-pointers
                                 :package package))


(defun load-static-adapter-template ()
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :claw/generator/common
                                  "src/gen/common/adapter/template/static.c")))


(defun preprocess-static-adapter-template (defines header-file function-definitions)
  (preprocess-template (load-static-adapter-template)
                       "timestamp" (get-timestamp)
                       "defines" defines
                       "includes" header-file
                       "function-definitions" function-definitions))


(defmethod generate-adapter-file ((this static-adapter))
  (when (%adapter-needs-rebuilding-p this)
    (ensure-directories-exist (uiop:pathname-directory-pathname (adapter-file-of this)))
    (with-output-to-file (output (adapter-file-of this) :if-exists :supersede)
      (let* ((functions (adapted-functions))
             (definitions (%generate-adapted-function-definitions functions "")))
        (format output "~A"
                (preprocess-static-adapter-template
                 (format-defines this)
                 (format-includes this)
                 definitions))))))


(defun build-static-adapter (standard adapter-file includes target-file
                             &key pedantic dependencies compiler flags intrinsics)
  (%build-adapter standard adapter-file includes target-file
                  :pedantic pedantic
                  :dependencies dependencies
                  :compiler compiler
                  :flags flags
                  :intrinsics intrinsics))


(defmethod expand-adapter-routines ((this static-adapter) wrapper)
  (let ((name (wrapper-name-of this))
        (shared-library-name (default-shared-adapter-library-name)))
    `(,@(when (builder-enabled-p this)
          `((defmethod build-adapter ((wrapper-name (eql ',name)) &key target
                                                                    dependencies
                                                                    compiler
                                                                    flags)
              (declare (ignore wrapper-name))
              (build-static-adapter ,(standard-of this)
                                    ,(adapter-file-of this)
                                    (list ,@(includes-of this))
                                    (merge-pathnames (or target ,shared-library-name)
                                                     ,(claw.wrapper:merge-wrapper-pathname
                                                       "" wrapper))
                                    :dependencies dependencies
                                    :compiler compiler
                                    :flags flags
                                    :intrinsics ',(intrinsics-of this))))))))
