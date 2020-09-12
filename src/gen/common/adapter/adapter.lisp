(cl:in-package :claw.generator.common)


(defvar *lib-name-regex* (ppcre:create-scanner "^lib(.*)\\..*$"))

(defgeneric generate-adapter-file (adapter))
(defgeneric build-adapter (wrapper-name &key target dependencies compiler))
(defgeneric expand-adapter-routines (adapter wrapper))


(defclass adapter ()
  ((wrapper-name :reader wrapper-name-of)
   (adapter-file :reader adapter-file-of)
   (headers :reader headers-of)
   (standard :reader standard-of)
   (includes :reader includes-of)
   (functions :initform nil :reader functions-of)
   (defines :initform nil :reader defines-of)
   (pointer-extractor-predicate :initform nil :reader pointer-extractor-predicate-of)))


(defmethod initialize-instance :after ((this adapter) &key wrapper path extract-pointers)
  (with-slots (wrapper-name wrapper-last-update-time
               adapter-file headers standard includes defines
               pointer-extractor-predicate)
      this
    (let ((opts (claw.wrapper:wrapper-options wrapper))
          (extractor-regexes (loop for regex in extract-pointers
                                   collect (ppcre:create-scanner regex))))
      (flet ((%extractor-predicate (name)
               (and (loop for regex in extractor-regexes
                            thereis (ppcre:scan regex name))
                    t)))
        (setf wrapper-name (claw.wrapper:wrapper-name wrapper)
              adapter-file (uiop:ensure-pathname
                            (claw.wrapper:merge-wrapper-pathname path wrapper)
                            :want-file t)
              headers (claw.wrapper:wrapper-options-headers opts)
              standard (claw.wrapper:wrapper-options-standard opts)
              includes (claw.wrapper:wrapper-options-includes opts)
              defines (claw.wrapper:wrapper-options-defines opts)
              pointer-extractor-predicate #'%extractor-predicate)))))


(defun format-adapted-function-name (name)
  (labels ((encode-non-word (match &rest registers)
             (declare (ignore registers))
             (format nil "E~X" (char-code (aref match 0))))
           (prepared-name ()
             (ppcre:regex-replace-all "\\W"
                                      name
                                      #'encode-non-word
                                      :simple-calls t)))
    (format nil "~A~A" +adapted-function-prefix+ (prepared-name))))


(defun register-adapted-function (adapted-function)
  (when-let ((adapter (adapter)))
    (with-slots (functions) adapter
      (push adapted-function functions)))
  (format-adapted-function-name (adapted-function-name adapted-function)))


(defun function-pointer-extractor-required-p (name)
  (when-let ((adapter (adapter)))
    (funcall (pointer-extractor-predicate-of adapter) name)))


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


(defun enveloped-function-protoype-p (entity)
  (if (claw.spec:foreign-envelope-p entity)
      (enveloped-function-protoype-p (claw.spec:foreign-enveloped-entity entity))
      (typep entity 'claw.spec:foreign-function-prototype)))


(defun format-function (function stream)
  (let* ((result-type (adapted-function-result-type function))
         (name (format-adapted-function-name (adapted-function-name function)))
         (params (mapcar #'claw.spec:format-foreign-entity-c-name (adapted-function-parameters function)))
         (body (adapted-function-body function)))
    (if (enveloped-function-protoype-p result-type)
        (format stream "~A {~%~A~%}"
                (claw.spec:format-foreign-entity-c-name result-type
                                :name (format nil "~A(~{~A~^, ~})" name params))
                body)
        (format stream "~A ~A(~{~A~^, ~}) {~%~A~%}"
                (claw.spec:format-foreign-entity-c-name result-type)
                name
                params
                body))))


(defun %generate-adapted-function-definitions (functions invocation-prefix)
  (with-output-to-string (stream)
    (loop for function in functions
          do (format stream "~&~%__CLAW_API ")
             (format-function function stream))))


(defun %adapter-needs-rebuilding-p (this)
  (or (uiop:featurep :claw-rebuild-adapter)
      (not (probe-file (adapter-file-of this)))))

;;;
;;; BUILD
;;;
(defun parse-dependencies (dependencies)
  (flet ((extract-libname (name)
           (let ((groups (second (multiple-value-list (ppcre:scan-to-strings *lib-name-regex* name)))))
             (if (emptyp groups)
                 name
                 (aref groups 0)))))
    (loop for dependency in dependencies
          for name = (file-namestring dependency)
          for path = (directory-namestring dependency)
          collect path into directories
          collect (extract-libname name) into libraries
          finally (return (values directories libraries)))))


(defun %build-adapter (standard adapter-file includes target-file
                       &key pedantic dependencies compiler)
  (multiple-value-bind (library-directories libraries) (parse-dependencies dependencies)
    (uiop:run-program (append (list (ecase (or compiler :gcc)
                                      (:gcc "g++")
                                      (:clang "clang++")))
                              (when (eq compiler :clang)
                                (list "-undefined" "dynamic_lookup" "-Wno-narrowing"))
                              (list "-shared" (namestring adapter-file))
                              (when standard
                                (list (format nil "-std=~A" standard)))
                              (when pedantic
                                (list "-pedantic"))
                              (list "-O2" "-fPIC")
                              (loop for directory in includes
                                    collect (format nil "-I~A"
                                                    (namestring directory)))
                              (loop for dir in library-directories
                                    collect (format nil "-L~A" dir))
                              (loop for lib in libraries
                                    collect (format nil "-l~A" lib))
                              (list "-o" (namestring target-file)))
                      :output *standard-output*
                      :error-output *debug-io*)))


(defun format-defines (adapter)
  (with-output-to-string (out)
    (loop for (name value) on (defines-of adapter) by #'cddr
          do (format out "~&#define ~A~@[ ~A~]" name value))))


(defun format-includes (adapter)
  (format nil "~{#include \"~A\"~^~%~}" (headers-of adapter)))
