(cl:in-package :claw.generator.common)


(defvar *lib-name-regex* (ppcre:create-scanner "^lib(.*)\\..*$"))


(defgeneric generate-adapter-file (adapter))
(defgeneric build-adapter (wrapper-name &key target dependencies compiler flags))
(defgeneric expand-adapter-routines (adapter wrapper))


(defclass adapter ()
  ((wrapper-name :reader wrapper-name-of)
   (adapter-file :reader adapter-file-of)
   (package :initarg :package :reader package-of)
   (headers :reader headers-of)
   (standard :reader standard-of)
   (includes :reader includes-of)
   (defines :initform nil :reader defines-of)
   (intrinsics :initform nil :reader intrinsics-of)
   (pointer-extractor-predicate :initform nil :reader pointer-extractor-predicate-of)
   (target :initform nil :reader target-of)
   (builder-enabled-p :initform nil :reader builder-enabled-p)))


(defmethod adapted-functions ()
  (hash-table-values *adapted-function-table*))


(defmethod initialize-instance :after ((this adapter) &key wrapper path extract-pointers)
  (with-slots (wrapper-name wrapper-last-update-time
               adapter-file headers standard includes defines
               intrinsics pointer-extractor-predicate target package)
      this

    (let ((opts (claw.wrapper:wrapper-options wrapper))
          (extractor-regexes (loop for regex in extract-pointers
                                   collect (ppcre:create-scanner regex))))
      (flet ((%extractor-predicate (name)
               (and (loop for regex in extractor-regexes
                            thereis (ppcre:scan regex name))
                    t)))
        (setf wrapper-name (claw.wrapper:wrapper-name wrapper)
              target (claw.wrapper:wrapper-target wrapper)
              adapter-file (uiop:ensure-pathname
                            (claw.wrapper:merge-wrapper-pathname
                             (merge-pathnames
                              (format nil "~A.~A.~A"
                                      (pathname-name path)
                                      target
                                      (pathname-type path))
                              (uiop:pathname-directory-pathname path))
                             wrapper)
                            :want-file t)
              headers (claw.wrapper:wrapper-options-headers opts)
              standard (claw.wrapper:wrapper-options-standard opts)
              includes (claw.wrapper:wrapper-options-includes opts)
              defines (claw.wrapper:wrapper-options-defines opts)
              intrinsics (claw.wrapper:wrapper-options-intrinsics opts)
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
    (let ((adapted-name (format-adapted-function-name (adapted-function-name adapted-function))))
      (setf (gethash adapted-name *adapted-function-table*) adapted-function)
      adapted-name)))


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
  (let ((original (adapted-function-entity function)))
    (format out "typedef ~A (*~A)(~{~A~^,~});"
            (claw.spec:format-foreign-entity-c-name (claw.spec:foreign-function-result-type original))
            (%make-function-type-name function)
            (loop for param in (claw.spec:foreign-function-parameters original)
                  collect (claw.spec:format-foreign-entity-c-name
                           (claw.spec:foreign-enveloped-entity param))))))


(defun %generate-function-variable (function out)
  (format out "static ~A ~A~A;"
          (%make-function-type-name function)
          +adapted-variable-prefix+
          (adapted-function-name function)))


(defun %generate-function-variable-init (function out)
  (format out "~A~A = (~A) claw_get_proc_addr(\"~A\");"
          +adapted-variable-prefix+
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
         (params (mapcar #'claw.spec:format-foreign-entity-c-name
                         (adapted-function-parameters function)))
         (body (adapted-function-body function))
         (namespaces (when-let ((namespace (adapted-function-namespace function)))
                       (ppcre:split "::" namespace))))
    (format stream "~&~%")
    (loop for namespace in namespaces
          do (format stream "~&namespace ~A {" namespace))
    (format stream "~&__CLAW_API ")
    (if (enveloped-function-protoype-p result-type)
        (format stream "~A {~%~A~%}"
                (claw.spec:format-foreign-entity-c-name result-type
                                                        :name (format nil "~A(~{~A~^, ~})" name params))
                body)
        (format stream "~A ~A(~{~A~^, ~}) {~%~A~%}"
                (claw.spec:format-foreign-entity-c-name result-type)
                name
                params
                body))
    (loop for nil in namespaces
          do (format stream "~&}"))))


(defun %generate-adapted-function-definitions (functions invocation-prefix)
  (with-output-to-string (stream)
    (loop for function in functions
          do (format-function function stream))))


(defun %adapter-needs-rebuilding-p (this)
  (or (not (probe-file (adapter-file-of this)))
      (member :claw-regen-adapter *features*)))


;;;
;;; BUILD
;;;
(defun parse-dependencies (dependencies)
  (flet ((extract-libname (name)
           (let ((groups (second (multiple-value-list (ppcre:scan-to-strings *lib-name-regex* name)))))
             (if (emptyp groups)
                 name
                 (aref groups 0)))))
    (loop for dependency in (ensure-list dependencies)
          for name = (file-namestring dependency)
          for path = (directory-namestring dependency)
          collect path into directories
          collect (extract-libname name) into libraries
          finally (return (values directories libraries)))))


(defun %build-adapter (standard adapter-file includes target-file
                       &key pedantic dependencies compiler flags intrinsics)
  (multiple-value-bind (library-directories libraries) (parse-dependencies dependencies)
    (uiop:run-program (append (list (ecase (or compiler :clang)
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
                              (loop for intrinsic in intrinsics
                                    collect (ecase intrinsic
                                              (:sse "-msse")
                                              (:sse2 "-msse2")
                                              (:sse3 "-msse3")
                                              (:sse41 "-msse4.1")
                                              (:sse42 "-msse4.2")
                                              (:avx "-mavx")
                                              (:avx2 "-mavx2")
                                              (:neon "-mfpu=neon")))
                              (when flags
                                (ensure-list flags))
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
