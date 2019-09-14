(cl:in-package :claw.cffi.c)


(defgeneric generate-adapter-file (adapter))
(defgeneric build-adapter (wrapper-name target-file))
(defgeneric expand-adapter-routines (adapter wrapper))


(defclass adapter ()
  ((wrapper-name :reader wrapper-name-of)
   (adapter-file :reader adapter-file-of)
   (wrapper-last-update-time :reader wrapper-last-update-time-of)
   (headers :reader headers-of)
   (standard :reader standard-of)
   (includes :reader includes-of)
   (functions :initform nil :reader functions-of)))


(defmethod initialize-instance :after ((this adapter) &key wrapper path)
  (with-slots (wrapper-name wrapper-last-update-time
               adapter-file headers standard includes)
      this
    (setf wrapper-name (claw.wrapper:wrapper-name wrapper)
          adapter-file (uiop:ensure-pathname
                        (claw.wrapper:merge-wrapper-pathname path wrapper)
                        :want-file t)
          headers (claw.wrapper:wrapper-headers wrapper)
          standard (claw.wrapper:wrapper-standard wrapper)
          includes (claw.wrapper:wrapper-includes wrapper)
          wrapper-last-update-time (claw.wrapper:wrapper-last-update-time wrapper))))


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


(defun %make-variable-name (function)
  (format nil "~A~A" +adapted-variable-prefix+ (adapted-function-name function)))


(defun %make-function-name (function)
  (format nil "~A~A" +adapted-function-prefix+ (adapted-function-name function)))


(defun %make-function-type-name (function)
  (format nil "~A_t" (%make-function-name function)))


(defun %generate-function-type (function out)
  (format out "typedef ~A;"
          (adapted-function-original-type function
                                          (%make-function-type-name function))))


(defun %generate-function-variable (function out)
  (format out "static ~A ~A;"
          (%make-function-type-name function)
          (%make-variable-name function)))


(defun %generate-function-variable-init (function out)
  (format out "~A = (~A) claw_get_proc_addr(\"~A\");"
          (%make-variable-name function)
          (%make-function-type-name function)
          (adapted-function-name function)))


(defun %generate-adapted-function-definitions (functions invocation-prefix)
  (with-output-to-string (stream)
    (loop for function in functions
          do (format stream "~&~%__CLAW_API ")
             (adapted-function-definition function
                                          (%make-function-name function)
                                          (format nil "~A~A" invocation-prefix
                                                  (adapted-function-name function))
                                          stream))))

(defun %adapter-needs-rebuilding-p (this)
  (or (uiop:featurep :claw-rebuild-adapter)
      (not (probe-file (adapter-file-of this)))
      (> (wrapper-last-update-time-of this)
         (file-write-date (adapter-file-of this)))))
