(cl:in-package :claw.generator.common)


(defgeneric generate-adapter-file (adapter))
(defgeneric build-adapter (wrapper-name &optional target-file))
(defgeneric expand-adapter-routines (adapter wrapper))


(defclass adapter ()
  ((wrapper-name :reader wrapper-name-of)
   (adapter-file :reader adapter-file-of)
   (headers :reader headers-of)
   (standard :reader standard-of)
   (includes :reader includes-of)
   (functions :initform nil :reader functions-of)))


(defmethod initialize-instance :after ((this adapter) &key wrapper path)
  (with-slots (wrapper-name wrapper-last-update-time
               adapter-file headers standard includes)
      this
    (let ((opts (claw.wrapper:wrapper-options wrapper)))
      (setf wrapper-name (claw.wrapper:wrapper-name wrapper)
            adapter-file (uiop:ensure-pathname
                          (claw.wrapper:merge-wrapper-pathname path wrapper)
                          :want-file t)
            headers (claw.wrapper:wrapper-options-headers opts)
            standard (claw.wrapper:wrapper-options-standard opts)
            includes (claw.wrapper:wrapper-options-includes opts)))))


(defun register-adapted-function (adapter function)
  (with-slots (functions) adapter
    (push function functions)))


(defun preprocess-template (source &rest args)
  (labels ((%replace (name value source)
             (let ((regex (format nil "{{\\s*~A\\s*}}"
                                  (ppcre:quote-meta-chars name))))
               (ppcre:regex-replace-all regex source value)))
           (%to-source (result arg)
             (destructuring-bind (name . value) arg
               (%replace name value result))))
    (reduce #'%to-source (alexandria:plist-alist args) :initial-value source)))


(defun %make-function-type-name (function)
  (format nil "~A_t" (adapted-function-name function)))


(defun %generate-function-type (function out)
  (format out "typedef ~A;"
          (adapted-function-original-type function
                                          (%make-function-type-name function))))


(defun %generate-function-variable (function out)
  (format out "static ~A ~A;"
          (%make-function-type-name function)
          (adapt-function-name function)))


(defun %generate-function-variable-init (function out)
  (format out "~A = (~A) claw_get_proc_addr(\"~A\");"
          (adapted-function-name function)
          (%make-function-type-name function)
          (adapted-function-name function)))


(defgeneric entity->c-name (entity))


(defun format-function (function stream)
  (format stream "~A ~A(~{~A~^, ~}) {~%~A~%}"
          (entity->c-name (adapted-function-return-type function))
          (adapted-function-name function)
          (mapcar #'entity->c-name (adapted-function-parameters function))
          (adapted-function-body function)))


(defun %generate-adapted-function-definitions (functions invocation-prefix)
  (with-output-to-string (stream)
    (loop for function in functions
          do (format stream "~&~%__CLAW_API ")
             (format-function function stream))))


(defun %adapter-needs-rebuilding-p (this)
  (or (uiop:featurep :claw-rebuild-adapter)
      (not (probe-file (adapter-file-of this)))))
